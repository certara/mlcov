`%>%` <- dplyr::`%>%`

#' MLCovSearch
#'
#' @param tab A data frame containing the data to be analyzed
#' @param list_pop_param Parameters
#' @param cov_continuous Character vector of continuous covariate names
#' @param cov_factors Character vector of categorical or occasion covariate names
#' @param seed Numeric value for usage of \code{set.seed()} inside function.
#'
#' @return list
#' @export
#'
MLCovSearch <- function(tab, list_pop_param, cov_continuous, cov_factors, seed = 123) {
  
  # Check that covariates supplied by user exist in the data
  errors <- data_validation(tab, list_pop_param, cov_continuous, cov_factors)
  if (length(errors) > 0) {
    stop(paste0(errors, sep = "\n"), call. = FALSE)
  }
  
  stopifnot(is.numeric(seed))
  set.seed(seed)

  stopifnot(requireNamespace("caret", quietly = TRUE))
  # Selection of columns required
  tab <- tab %>%
    dplyr::select(ID, dplyr::all_of(list_pop_param), dplyr::all_of(cov_continuous), dplyr::all_of(cov_factors))

  # In order to have the individual parameter and one point per subject
  dat <- unique(tab) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(cov_factors), as.factor))

  # Data for XGBoost
  pop_param <- dat %>% dplyr::select(dplyr::all_of(list_pop_param))
  factors <- dat %>% dplyr::select(dplyr::all_of(cov_factors))
  continuous <- dat %>% dplyr::select(dplyr::all_of(cov_continuous))

  # One-hot encoding of categorical covariates for covariates with more than 2 levels
  modified_columns <- data.frame(matrix(ncol = 0, nrow = nrow(factors)))
  for (col in names(factors)) {
    if (is.factor(factors[[col]]) && nlevels(factors[[col]]) > 2) {
      dmy <- caret::dummyVars(paste0("~", col), data = factors)
      encoded <- data.frame(predict(dmy, newdata = factors))
      modified_columns <- cbind(modified_columns,encoded)
    } else {
      modified_columns[[col]] <- as.numeric(as.character(factors[[col]]))
    }
  }

  dat_XGB <- cbind(pop_param, modified_columns, continuous)

  full_covariate_xgm <- names(dat_XGB)
  full_covariate_xgm <- setdiff(full_covariate_xgm, list_pop_param)

  # Assign the independent and dependent covariates
  x_xgb <- data.matrix(dat_XGB[, c(full_covariate_xgm)])

  # Creation of results datasets for selected covariates of the 5 folds
  result_5folds <- data.frame(
    fold1 = rep(NA, length(list_pop_param)),
    fold2 = rep(NA, length(list_pop_param)),
    fold3 = rep(NA, length(list_pop_param)),
    fold4 = rep(NA, length(list_pop_param)),
    fold5 = rep(NA, length(list_pop_param))
  )
  rownames(result_5folds) <- list_pop_param

  for (i in list_pop_param) {

    y_xgb <- log(dat_XGB[, i])

    # Cross-validation
    ## create 5 partition of the data ( using K-1 folds (80%) as the training set and the remaining one fold (20%) as the test set repeating steps for K iterations )
    x <- as.data.frame(x_xgb)
    folds <- caret::createFolds(seq(1, nrow(x_xgb)), k = 5, list = TRUE, returnTrain = FALSE)

    for (j in 1:5) {
      train.ind <- folds[[j]]
      testing <- x[train.ind, ] # Fold k for testing
      training <- x[-train.ind, ] # Remaining (k-1) for training
      training <- as.matrix(training)
      testing <- as.matrix(testing)

      y <- as.data.frame(y_xgb)
      y_xgb_train <- y[-train.ind, ]
      y_xgb_test <- y[train.ind, ]

      # Lasso regression for variable selection
      X <- as.matrix(training)
      Y <- as.matrix(y_xgb_train)
      # Perform k-fold cross-validation to find optimal lambda value
      cvfit <- glmnet::cv.glmnet(X, Y, alpha = 1, family = "gaussian")
      # Extract the non-zero coefficients from the model at the optimal value of the regularization parameter
      lasso.coef <- coef(cvfit, s = cvfit$lambda.min)[-1, ]
      selected.vars <- names(lasso.coef[lasso.coef != 0])

      # create new training and testing sets using only selected covariates by lasso
      train.lasso <- training[, selected.vars]
      train.lasso <- as.data.frame(train.lasso)
      colnames(train.lasso) <- c(selected.vars)

      # Boruta performed on the covariates selected by lasso
      if (length(selected.vars) != 0) {
        xgb.boruta <- Boruta::Boruta(
          train.lasso,
          y = y_xgb_train,
          maxRuns = 200,
          doTrace = 0,
          seed = 42,
          getImp = Boruta::getImpXgboost,
          nrounds = 200,
          objective = "reg:squarederror"
        )

        # Extracting the result of Boruta algorithm (keep confirmed)
        boruta.df <- Boruta::attStats(xgb.boruta)
        feature.imp <- row.names(boruta.df)[which(boruta.df$decision == "Confirmed")]

        result_5folds[i, j] <- paste(feature.imp, collapse = ', ')


      }
    }
  }

  # Final covariate selection with a voting mechanism
  result_ML <- data.frame(cov_selected = rep(NA, length(list_pop_param)))
  rownames(result_ML) <- list_pop_param

  res <- t(result_5folds[,1:5])
  res <- res %>% dplyr::na_if("")

  for (i in list_pop_param) {
    list_cov <- strsplit(res[, i], ",")
    list_cov_nb <- trimws(unlist(list_cov))
    comptage <- as.data.frame(table(list_cov_nb))
    if (nrow(comptage) != 0) {
    filtered_vars <- comptage %>% dplyr::filter(Freq >= 2) %>% dplyr::select(list_cov_nb)
    variable_list <- as.character(filtered_vars$list_cov_nb)
    cov_selected <- paste(variable_list, collapse = ", ")
    result_ML[i, 1] <- cov_selected
    }
  }


  result_ML$RMSE <- rep(NA,length(list_pop_param))
  result_ML$RMSE_ref <- rep(NA,length(list_pop_param))



  # Evaluation of model with selected covariates

  for (i in list_pop_param) {
    y_xgb <- log(dat_XGB[, i])

    RMSE <- rep(NA,5)
    RMSE_ref <- rep(NA,5)

    if (is.na(result_ML[i, 1]) == FALSE){
      list_cov <- strsplit(gsub(" ", "", result_ML[i, 1]), ",")
      x.selected_final <- as.matrix(dat_XGB %>% dplyr::select(dplyr::all_of(list_cov[[1]])))
      folds <- caret::createFolds(seq(1,nrow(x.selected_final)), k = 5, list = TRUE, returnTrain = FALSE)

      for (j in 1:5){

        train.ind <- folds[[j]]
        testing=x.selected_final[train.ind, ] #the fold k for the test
        training=x.selected_final[-train.ind, ] #the reamaining (k-1) for the train
        training <- as.matrix(training)
        testing <- as.matrix(testing)

        y <- as.data.frame(y_xgb)
        y.xgm_train <- y[-train.ind,]
        y.xgm_test <- y[train.ind,]


        if (length(list_cov[[1]]) != 0 ) {
          xgb.mod <- xgboost::xgboost(
            data = training,
            label = y.xgm_train,
            nrounds = 200,
            objective = "reg:squarederror",
            verbose = 0
          )

          # predict on the test set with the new model
          y.xgb.pred <- predict(xgb.mod, newdata = testing)
          # evaluate the performance of the model
          RMSE[j] <- Metrics::rmse(y.xgm_test,y.xgb.pred)
          result_ML[i,2] <- mean(RMSE,na.rm = TRUE)



          # Calculate the reference RMSE (baseline model without any covariates) by using the mean of the training y values and comparing it with the test y values
          mean_y <- mean(y.xgm_train)
          y.mean <- rep(mean_y, length(y.xgm_test))
          RMSE_ref[j] <-  Metrics::rmse(y.xgm_test,y.mean)
          result_ML[i,3] <- mean(RMSE_ref,na.rm = TRUE)


        }

      }
    }
  }

  # Initialize an empty list to store the SHAP summary data and seed information
  shap_data <- list()
  shap_seed <- list()

  # Interpretation of Selected covariates Beeswarm Plots
  for (i in list_pop_param) {
    y_xgb <- log(dat_XGB[, i])
    
    if (is.na(result_ML[i, 1]) == FALSE) {
      list_cov <- strsplit(gsub(" ", "", result_ML[i, 1]), ",")
      x.selected_final <-
        as.matrix(dat_XGB %>% dplyr::select(dplyr::all_of(list_cov[[1]])))
      
      if (length(list_cov[[1]]) != 0) {
        xgb.mod_final <- xgboost::xgboost(
          data = x.selected_final,
          label = y_xgb,
          nrounds = 200,
          objective = "reg:squarederror",
          verbose = 0
        )
        
        # Generate SHAP summary plot for the current parameter
        shap_values <- SHAPforxgboost::shap.values(xgb_model = xgb.mod_final, X_train = x.selected_final)
        shap_long <- SHAPforxgboost::shap.prep(xgb_model = xgb.mod_final, X_train = x.selected_final)
        
        # Store shap data and seed
        shap_data[[i]] <- list(shap_values = shap_values, shap_long = shap_long)
        shap_seed[[i]] <- .Random.seed
        
      }
    }
  }

  # Return the result_ML table and the SHAP plots for each parameter
  return(
    list(
      result_ML = result_ML,
      result_5folds = result_5folds,
      list_pop_param = list_pop_param,
      cov_continuous = cov_continuous, 
      cov_factors = cov_factors,
      shap_data = shap_data,
      shap_seed = shap_seed
    ) %>% structure(class = "mlcov_data")
  )
} 

data_validation <- function(tab, list_pop_param, cov_continuous, cov_factors) {
  errors <- c()
  
  for (i in 1:3) {
    vectors <- list(list_pop_param, cov_continuous, cov_factors)
    vector_names <- c("list_pop_param", "cov_continuous", "cov_factors")
    
    missing_values <- setdiff(vectors[[i]], colnames(tab))
    
    if (length(missing_values) > 0) {
      error_message <-
        paste( "The following values from", vector_names[[i]], "are missing in the dataset:", toString(missing_values))
      
      errors <- c(errors, error_message)
    }
  }
  return(errors)
  
}







#' Generate Residual Plots for Model Analysis
#'
#' This function generates residual plots for model analysis based on various covariates and model results. It performs data preprocessing, model fitting using XGBoost, and generates plots to visualize the residuals against the covariates.
#' @inheritParams MLCovSearch
#' @param list_pop_param A vector of strings indicating the names of population parameters in `tab`.
#' @param cov_continuous A vector of strings indicating the names of continuous covariates in `tab`.
#' @param cov_factors A vector of strings indicating the names of factor covariates in `tab`.
#' @param result_ML A data frame containing model results for maximum likelihood estimation.
#' @param result_5folds A data frame containing model results from 5-fold cross-validation.
#' @param i An integer indicating the index of the parameter of interest.
#'
#' @return A list of ggplot objects, each representing a residual plot for a different covariate. The function returns an empty list if no significant relationships are found. It also handles cases where covariates are not selected after the vote.
#'
#' @examples
#' # Assuming 'data' is a data frame with the necessary columns
#' \dontrun{
#' plots <- generate_residualsplots(data, 
#' list_pop_param, 
#' cov_continuous, 
#' cov_factors, 
#' result_ML, 
#' result_5folds, 
#' 1,
#' seed = 123)
#' }
#' @export
generate_residualsplots <- function(tab, list_pop_param, cov_continuous, cov_factors, result_ML, result_5folds, i, seed = 123 ) {
  
  # Check that covariates supplied by user exist in the data
  errors <- data_validation(tab, list_pop_param, cov_continuous, cov_factors)
  if (length(errors) > 0) {
    stop(paste0(errors, sep = "\n"), call. = FALSE)
  }
  
  stopifnot(is.numeric(seed))
  set.seed(seed)
  
  # Selection of columns required
  tab <- tab %>%
    dplyr::select(ID, dplyr::all_of(list_pop_param), dplyr::all_of(cov_continuous), dplyr::all_of(cov_factors))



  # In order to have the individual parameter and one point per subject
  dat <- unique(tab) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(cov_factors), as.factor))

  # Data for XGBoost
  pop_param <- dat %>% dplyr::select(dplyr::all_of(list_pop_param))
  factors <- dat %>% dplyr::select(dplyr::all_of(cov_factors))
  continuous <- dat %>% dplyr::select(dplyr::all_of(cov_continuous))

  # One-hot encoding of categorical covariates for covariates with more than 2 levels
  modified_columns <- data.frame(matrix(ncol = 0, nrow = nrow(factors)))
  for (col in names(factors)) {
    if (is.factor(factors[[col]]) && nlevels(factors[[col]]) > 2) {
      dmy <- caret::dummyVars(paste0("~", col), data = factors)
      encoded <- data.frame(predict(dmy, newdata = factors))
      modified_columns <- cbind(modified_columns,encoded)
    } else {
      modified_columns[[col]] <- as.numeric(as.character(factors[[col]]))
    }
  }

  dat_XGB <- cbind(pop_param, modified_columns, continuous)

  full_covariate_xgm <- names(dat_XGB)
  full_covariate_xgm <- setdiff(full_covariate_xgm, list_pop_param)

  full_covariate <- c(cov_continuous, cov_factors)

  res <- t(result_5folds[,1:5])
  res <- res %>% dplyr::na_if("")

  result_ML <- as.matrix(result_ML) %>% dplyr::na_if("")
  #result_ML[result_ML == ""] <- NA

  # Assign the independent and dependent covariates
  x_xgb <- data.matrix(dat_XGB[, c(full_covariate_xgm)])
  y_xgb <- log(dat_XGB[, i])
  plots_list <- list()  # Initialize the list to store plots

  dat <- dat %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(cov_factors), as.numeric))
  # First case: covariates are selected after the vote
  if (is.na(result_ML[i, 1]) == F)  {
    list_cov <- strsplit(gsub(" ", "", result_ML[i, 1]), ",")
    x.selected_final <- as.matrix(dat_XGB %>% dplyr::select(dplyr::all_of(list_cov[[1]])))

    train.ind <- caret::createDataPartition(seq(1, nrow(x.selected_final)), times = 1, p = 0.8, list = FALSE)
    training <- as.matrix(x.selected_final[train.ind, ])
    colnames(training) <- colnames(x.selected_final)
    testing <-  as.matrix(x.selected_final[-train.ind, ])
    colnames(testing) <- colnames(x.selected_final)


    y <- as.data.frame(y_xgb)
    y.xgm_train <- y[train.ind, ]
    y.xgm_test <- y[-train.ind, ]

    if (length(list_cov[[1]]) != 0 ) {
      xgb.mod <- xgboost::xgboost(
        data = training,
        label = y.xgm_train,
        nrounds = 200,
        objective = "reg:squarederror",
        verbose = 0
      )

      # predict on the test set with the new model
      y.xgb.pred <- predict(xgb.mod, newdata = testing)

      residuals <- y.xgb.pred - y.xgm_test

      for (k in full_covariate) {
        if (!(any(grepl(k, list_cov[[1]])))) {
          if (k %in% cov_continuous) {
            data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
            plot <- ggplot2::ggplot(data_plot, ggplot2::aes(x = cov, y = Residuals)) +
              ggplot2::geom_point() +
              ggplot2::labs(x = k, y = paste("Residuals", i)) +
              ggplot2::geom_smooth(method = 'lm')
          } else {
            data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
            plot <- ggplot2::ggplot(data_plot, ggplot2::aes(x = as.factor(cov), y = Residuals)) +
              ggplot2::geom_boxplot() +
              ggplot2::geom_point(position = ggplot2::position_jitter(width = 0, height = 0)) +
              ggplot2::labs(x = k, y = paste("Residuals", i))
          }

          # Calculate correlation and p-value
          cor_test_result <- cor.test(data_plot$Residuals, data_plot$cov)
          p_value <- cor_test_result$p.value

          attempts <- 1
          max_attempts <- 10

          # Try different train indices until a valid p-value is obtained or reach the maximum attempts
          while (is.na(p_value) && attempts <= max_attempts) {
            train.ind <- caret::createDataPartition(seq(1, nrow(x.selected_final)), times = 1, p = 0.8, list = FALSE)
            training <- x.selected_final[train.ind, ]
            testing <- x.selected_final[-train.ind, ]

            y.xgm_train <- y[train.ind, ]
            y.xgm_test <- y[-train.ind, ]

            xgb.mod <- xgboost::xgboost(
              data = training,
              label = y.xgm_train,
              nrounds = 200,
              objective = "reg:squarederror",
              verbose = 0
            )

            y.xgb.pred <- predict(xgb.mod, newdata = testing)
            residuals <- y.xgb.pred - y.xgm_test

            data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
            cor_test_result <- cor.test(data_plot$Residuals, data_plot$cov)
            p_value <- cor_test_result$p.value

            attempts <- attempts + 1
          }

          if (!is.na(p_value) && p_value <= 0.05){
            # Add p-value as an annotation in the top-right corner
            plot <- plot +
              annotate(
                "text",
                x = Inf,
                y = Inf,
                label = paste("p-value =", round(p_value, 4)),
                hjust = 1,
                vjust = 1,
                size = 4
              )

            plots_list[[k]] <- plot
          }
        }
      }
    }
  }

  # Second case: covariates are not selected after the vote
  if (is.na(result_ML[i, 1]))  {
    list_cov <- strsplit(res[, i], ",")
    list_cov_nb <- trimws(unlist(list_cov))
    comptage <- as.data.frame(table(list_cov_nb))
    if (nrow(comptage) != 0) {

    x.selected_final <- as.matrix(dat_XGB %>% dplyr::select(dplyr::all_of(comptage$list_cov_nb)))

    train.ind <- caret::createDataPartition(seq(1, nrow(x.selected_final)), times = 1, p = 0.8, list = FALSE)
    training <- as.matrix(x.selected_final[train.ind, ])
    colnames(training) <- colnames(x.selected_final)
    testing <-  as.matrix(x.selected_final[-train.ind, ])
    colnames(testing) <- colnames(x.selected_final)


    y <- as.data.frame(y_xgb)
    y.xgm_train <- y[train.ind, ]
    y.xgm_test <- y[-train.ind, ]

    if (length(list_cov_nb) != 0 ) {
      xgb.mod <- xgboost::xgboost(
        data = training,
        label = y.xgm_train,
        nrounds = 200,
        objective = "reg:squarederror",
        verbose = 0
      )

      # predict on the test set with the new model
      y.xgb.pred <- predict(xgb.mod, newdata = testing)

      residuals <- y.xgb.pred - y.xgm_test

      for (k in full_covariate) {
        if (k %in% cov_continuous) {
          data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
          plot <- ggplot2::ggplot(data_plot, ggplot2::aes(x = cov, y = Residuals)) +
            ggplot2::geom_point() +
            ggplot2::labs(x = k, y = paste("Residuals", i)) +
            ggplot2::geom_smooth(method = 'lm')
        } else {
          data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
          plot <- ggplot2::ggplot(data_plot, ggplot2::aes(x = as.factor(cov), y = Residuals)) +
            ggplot2::geom_boxplot() +
            ggplot2::geom_point(position = ggplot2::position_jitter(width = 0, height = 0)) +
            ggplot2::labs(x = k, y = paste("Residuals", i))
        }

        # Calculate correlation and p-value
        cor_test_result <- cor.test(data_plot$Residuals, data_plot$cov)
        p_value <- cor_test_result$p.value

        attempts <- 1
        max_attempts <- 10

        # Try different train indices until a valid p-value is obtained or reach the maximum attempts
        while (is.na(p_value) && attempts <= max_attempts) {
          train.ind <- caret::createDataPartition(seq(1, nrow(x.selected_final)), times = 1, p = 0.8, list = FALSE)
          training <- as.matrix(x.selected_final[train.ind, ])
          colnames(training) <- colnames(x.selected_final)
          testing <-  as.matrix(x.selected_final[-train.ind, ])
          colnames(testing) <- colnames(x.selected_final)

          y.xgm_train <- y[train.ind, ]
          y.xgm_test <- y[-train.ind, ]

          xgb.mod <- xgboost::xgboost(
            data = training,
            label = y.xgm_train,
            nrounds = 200,
            objective = "reg:squarederror",
            verbose = 0
          )

          y.xgb.pred <- predict(xgb.mod, newdata = testing)
          residuals <- y.xgb.pred - y.xgm_test

          data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
          cor_test_result <- cor.test(data_plot$Residuals, data_plot$cov)
          p_value <- cor_test_result$p.value

          attempts <- attempts + 1
        }

        if (!is.na(p_value) && p_value <= 0.05){
          # Add p-value as an annotation in the top-right corner
          plot <- plot +
            annotate(
              "text",
              x = Inf,
              y = Inf,
              label = paste("p-value =", round(p_value, 4)),
              hjust = 1,
              vjust = 1,
              size = 4
            )

          plots_list[[k]] <- plot
        }
      }
    }
  }
  else {message(paste("No variables selected for the 5 folds, so no model to be trained for", i))
    return()}
  }

  # Check if plots_list is empty
  if (length(plots_list) == 0) {
    list_cov <- strsplit(res[, i], ",")
    list_cov_nb <- trimws(unlist(list_cov))
    comptage <- as.data.frame(table(list_cov_nb))
    if (nrow(comptage) != 0){
    message(paste("No residuals plots with a significant p-value for", i))
    return()
    }
  }
  
  # Return the list of subplots for the given parameter i
  return(plots_list)
}



