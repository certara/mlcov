#' @import randomForest
#' @import Boruta
#' @import dplyr
#' @import xgboost
#' @import caret
#' @import glmnet
#' @import Metrics
#' @import hrbrthemes
#' @import Ckmeans.1d.dp
#' @import SHAPforxgboost
#' @import BBmisc
#' @import ggplot2
#' @import GGally
#' @import grid
#' @import ggstance
#' @import ggpubr
#' @import gridExtra
#' 
NULL

#' MLCovSearch
#'
#' @param tab A data frame containing the data to be analyzed
#' @param list_pop_param Parameters
#' @param cov_continuous Character vector of continuous covariate names
#' @param cov_factors Character vector of categorical or occasion covariate names
#'
#' @return list
#' @export
#'
MLCovSearch <- function(tab, list_pop_param, cov_continuous, cov_factors, seed = NULL) {
  
  if (is.null(seed)) {
    rm(.Random.seed)
  } else {
    set.seed(seed)
    print(paste0("Using seed: ", seed))
  }

  stopifnot(requireNamespace("caret", quietly = TRUE),
            is.numeric(seed))
  # Selection of columns required
  tab <- tab %>%
    select(ID, all_of(list_pop_param), all_of(cov_continuous), all_of(cov_factors))

  # In order to have the individual parameter and one point per subject
  dat <- unique(tab) %>%
    mutate(across(all_of(cov_factors), as.factor))

  # Data for XGBoost
  pop_param <- dat %>% select(all_of(list_pop_param))
  factors <- dat %>% select(all_of(cov_factors))
  continuous <- dat %>% select(all_of(cov_continuous))

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
    folds <- createFolds(seq(1, nrow(x_xgb)), k = 5, list = TRUE, returnTrain = FALSE)

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
      cvfit <- cv.glmnet(X, Y, alpha = 1, family = "gaussian")
      # Extract the non-zero coefficients from the model at the optimal value of the regularization parameter
      lasso.coef <- coef(cvfit, s = cvfit$lambda.min)[-1, ]
      selected.vars <- names(lasso.coef[lasso.coef != 0])

      # create new training and testing sets using only selected covariates by lasso
      train.lasso <- training[, selected.vars]
      train.lasso <- as.data.frame(train.lasso)
      colnames(train.lasso) <- c(selected.vars)

      # Boruta performed on the covariates selected by lasso
      if (length(selected.vars) != 0) {
        xgb.boruta <- Boruta(
          train.lasso,
          y = y_xgb_train,
          maxRuns = 200,
          doTrace = 0,
          seed = 42,
          getImp = getImpXgboost,
          nrounds = 200,
          objective = "reg:squarederror"
        )

        # Extracting the result of Boruta algorithm (keep confirmed)
        boruta.df <- attStats(xgb.boruta)
        feature.imp <- row.names(boruta.df)[which(boruta.df$decision == "Confirmed")]

        result_5folds[i, j] <- paste(feature.imp, collapse = ', ')


      }
    }
  }

  # Final covariate selection with a voting mechanism
  result_ML <- data.frame(cov_selected = rep(NA, length(list_pop_param)))
  rownames(result_ML) <- list_pop_param

  res <- t(result_5folds[,1:5])
  res <- res %>% na_if("")

  for (i in list_pop_param) {
    list_cov <- strsplit(res[, i], ",")
    list_cov_nb <- trimws(unlist(list_cov))
    comptage <- as.data.frame(table(list_cov_nb))
    if (nrow(comptage) != 0) {
    filtered_vars <- comptage %>% filter(Freq >= 2) %>% select(list_cov_nb)
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
      x.selected_final <- as.matrix(dat_XGB %>% select(all_of(list_cov[[1]])))
      folds <- createFolds(seq(1,nrow(x.selected_final)), k = 5, list = TRUE, returnTrain = FALSE)

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
          xgb.mod <- xgboost(
            data = training,
            label = y.xgm_train,
            nrounds = 200,
            objective = "reg:squarederror",
            verbose = 0
          )

          # predict on the test set with the new model
          y.xgb.pred <- predict(xgb.mod, newdata = testing)
          # evaluate the performance of the model
          RMSE[j] <- rmse(y.xgm_test,y.xgb.pred)
          result_ML[i,2] <- mean(RMSE,na.rm = TRUE)



          # Calculate the reference RMSE (baseline model without any covariates) by using the mean of the training y values and comparing it with the test y values
          mean_y <- mean(y.xgm_train)
          y.mean <- rep(mean_y, length(y.xgm_test))
          RMSE_ref[j] <-  rmse(y.xgm_test,y.mean)
          result_ML[i,3] <- mean(RMSE_ref,na.rm = TRUE)


        }

      }
    }
  }

  # Initialize an empty list to store the SHAP summary plots
  shap_plots <- list()

  # Create a function to generate SHAP summary plots
  generate_shap_summary_plot <- function(xgb_model, X_train, param_name) {
    shap_values <- SHAPforxgboost::shap.values(xgb_model = xgb_model, X_train = X_train)
    shap_long <- SHAPforxgboost::shap.prep(xgb_model = xgb_model, X_train = X_train)
    p <- SHAPforxgboost::shap.plot.summary(shap_long)
    p <- p + ggtitle(param_name)

    return(p)
  }

  # Interpretation of Selected covariates Beeswarm Plots
  for (i in list_pop_param) {
    y_xgb <- log(dat_XGB[, i])

    if (is.na(result_ML[i, 1]) == FALSE){
    list_cov <- strsplit(gsub(" ", "", result_ML[i, 1]), ",")
    x.selected_final <- as.matrix(dat_XGB %>% select(all_of(list_cov[[1]])))

    if (length(list_cov[[1]]) != 0 ) {
      xgb.mod_final <- xgboost(
        data = x.selected_final,
        label = y_xgb,
        nrounds = 200,
        objective = "reg:squarederror",
        verbose = 0
      )

      # Generate SHAP summary plot for the current parameter
      shap_plot <- generate_shap_summary_plot(
        xgb_model = xgb.mod_final,
        X_train = x.selected_final,
        param_name = i
      )
      shap_plots[[i]] <- shap_plot
    }
    }
  }

  combined_plots <- marrangeGrob(grobs = shap_plots,nrow = length(shap_plots),ncol = 1)




  # Return the result_ML table and the SHAP plots for each parameter
  return(list(result_ML = result_ML, result_5folds=result_5folds, shap_plots = shap_plots))
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
#' plots <- generate_residualsplots(data, list_pop_param, cov_continuous, cov_factors, result_ML, result_5folds, 1)
#'
#' @export
generate_residualsplots <- function(tab, list_pop_param, cov_continuous, cov_factors, result_ML, result_5folds, i, seed = NULL) {
  
  if (is.null(seed)) {
    rm(.Random.seed)
  } else {
    set.seed(seed)
    print(paste0("Using seed: ", seed))
  }
  
  stopifnot((is.numeric(seed) || is.null(seed)))
  
  # Selection of columns required
  tab <- tab %>%
    select(ID, all_of(list_pop_param), all_of(cov_continuous), all_of(cov_factors))



  # In order to have the individual parameter and one point per subject
  dat <- unique(tab) %>%
    mutate(across(all_of(cov_factors), as.factor))

  # Data for XGBoost
  pop_param <- dat %>% select(all_of(list_pop_param))
  factors <- dat %>% select(all_of(cov_factors))
  continuous <- dat %>% select(all_of(cov_continuous))

  # One-hot encoding of categorical covariates for covariates with more than 2 levels
  modified_columns <- data.frame(matrix(ncol = 0, nrow = nrow(factors)))
  for (col in names(factors)) {
    if (is.factor(factors[[col]]) && nlevels(factors[[col]]) > 2) {
      dmy <- dummyVars(paste0("~", col), data = factors)
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
  res <- res %>% na_if("")

  result_ML <- as.matrix(result_ML) %>% dplyr::na_if("")
  #result_ML[result_ML == ""] <- NA
  #result_ML <- as.matrix(result_ML) %>% dplyr::na_if("")
  

  # Assign the independent and dependent covariates
  x_xgb <- data.matrix(dat_XGB[, c(full_covariate_xgm)])
  y_xgb <- log(dat_XGB[, i])
  plots_list <- list()  # Initialize the list to store plots

  dat <- dat %>%
    mutate(across(all_of(cov_factors), as.numeric))
  # First case: covariates are selected after the vote
  if (is.na(result_ML[i, 1]) == F)  {
    list_cov <- strsplit(gsub(" ", "", result_ML[i, 1]), ",")
    x.selected_final <- as.matrix(dat_XGB %>% select(all_of(list_cov[[1]])))
    
    set.seed(seed)

    train.ind <- createDataPartition(seq(1, nrow(x.selected_final)), times = 1, p = 0.8, list = FALSE)
    training <- as.matrix(x.selected_final[train.ind, ])
    colnames(training) <- colnames(x.selected_final)
    testing <-  as.matrix(x.selected_final[-train.ind, ])
    colnames(testing) <- colnames(x.selected_final)


    y <- as.data.frame(y_xgb)
    y.xgm_train <- y[train.ind, ]
    y.xgm_test <- y[-train.ind, ]

    if (length(list_cov[[1]]) != 0 ) {
      xgb.mod <- xgboost(
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
            plot <- ggplot(data_plot, aes(x = cov, y = Residuals)) +
              geom_point() +
              labs(x = k, y = paste("Residuals", i)) +
              geom_smooth(method = 'lm')
          } else {
            data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
            plot <- ggplot(data_plot, aes(x = as.factor(cov), y = Residuals)) +
              geom_boxplot() +
              geom_point(position = position_jitter(width = 0, height = 0)) +
              labs(x = k, y = paste("Residuals", i))
          }

          # Calculate correlation and p-value
          cor_test_result <- cor.test(data_plot$Residuals, data_plot$cov)
          p_value <- cor_test_result$p.value

          attempts <- 1
          max_attempts <- 10

          # Try different train indices until a valid p-value is obtained or reach the maximum attempts
          while (is.na(p_value) && attempts <= max_attempts) {
            train.ind <- createDataPartition(seq(1, nrow(x.selected_final)), times = 1, p = 0.8, list = FALSE)
            training <- x.selected_final[train.ind, ]
            testing <- x.selected_final[-train.ind, ]

            y.xgm_train <- y[train.ind, ]
            y.xgm_test <- y[-train.ind, ]

            xgb.mod <- xgboost(
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

    x.selected_final <- as.matrix(dat_XGB %>% select(all_of(comptage$list_cov_nb)))
    
    set.seed(seed)

    train.ind <- createDataPartition(seq(1, nrow(x.selected_final)), times = 1, p = 0.8, list = FALSE)
    training <- as.matrix(x.selected_final[train.ind, ])
    colnames(training) <- colnames(x.selected_final)
    testing <-  as.matrix(x.selected_final[-train.ind, ])
    colnames(testing) <- colnames(x.selected_final)


    y <- as.data.frame(y_xgb)
    y.xgm_train <- y[train.ind, ]
    y.xgm_test <- y[-train.ind, ]

    if (length(list_cov_nb) != 0 ) {
      xgb.mod <- xgboost(
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
          plot <- ggplot(data_plot, aes(x = cov, y = Residuals)) +
            geom_point() +
            labs(x = k, y = paste("Residuals", i)) +
            geom_smooth(method = 'lm')
        } else {
          data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
          plot <- ggplot(data_plot, aes(x = as.factor(cov), y = Residuals)) +
            geom_boxplot() +
            geom_point(position = position_jitter(width = 0, height = 0)) +
            labs(x = k, y = paste("Residuals", i))
        }

        # Calculate correlation and p-value
        cor_test_result <- cor.test(data_plot$Residuals, data_plot$cov)
        p_value <- cor_test_result$p.value

        attempts <- 1
        max_attempts <- 10

        # Try different train indices until a valid p-value is obtained or reach the maximum attempts
        while (is.na(p_value) && attempts <= max_attempts) {
          train.ind <- createDataPartition(seq(1, nrow(x.selected_final)), times = 1, p = 0.8, list = FALSE)
          training <- as.matrix(x.selected_final[train.ind, ])
          colnames(training) <- colnames(x.selected_final)
          testing <-  as.matrix(x.selected_final[-train.ind, ])
          colnames(testing) <- colnames(x.selected_final)

          y.xgm_train <- y[train.ind, ]
          y.xgm_test <- y[-train.ind, ]

          xgb.mod <- xgboost(
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



