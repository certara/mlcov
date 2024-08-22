`%>%` <- dplyr::`%>%`

#' ml_cov_search
#'
#' @param data A data frame containing the data to be analyzed
#' @param pop_param Character vector of population parameter names
#' @param cov_continuous Character vector of continuous covariate names
#' @param cov_factors Character vector of categorical or occasion covariate names
#' @param seed Numeric value for usage of \code{set.seed()} inside function.
#'
#' @return list
#' @export
#'
ml_cov_search <- function(data, pop_param, cov_continuous, cov_factors, seed = 123) {
  
  # Check that covariates supplied by user exist in the data
  data_validation(data, pop_param, cov_continuous, cov_factors)

  # Set seed
  stopifnot(is.numeric(seed))
  set.seed(seed)

  stopifnot(requireNamespace("caret", quietly = TRUE))
  
  # Select columns and generate data for XGBoost
  data <- col_select(data, pop_param, cov_continuous, cov_factors)
  pop_parameters <- data %>% dplyr::select(dplyr::all_of(pop_param))
  factors <- data %>% dplyr::select(dplyr::all_of(cov_factors))
  continuous <- data %>% dplyr::select(dplyr::all_of(cov_continuous))

  # One-hot encoding of categorical covariates for covariates with more than 2 levels
  dat_XGB <- generate_dat_XGB(pop_parameters, factors, continuous)
 
  full_covariate_xgm <- names(dat_XGB)
  full_covariate_xgm <- setdiff(full_covariate_xgm, pop_param)

  # Assign the independent and dependent covariates
  x_xgb <- data.matrix(dat_XGB[, c(full_covariate_xgm)])

  # Creation of results datasets for selected covariates of the 5 folds
  result_5folds <- data.frame(
    fold1 = rep(NA, length(pop_param)),
    fold2 = rep(NA, length(pop_param)),
    fold3 = rep(NA, length(pop_param)),
    fold4 = rep(NA, length(pop_param)),
    fold5 = rep(NA, length(pop_param))
  )
  rownames(result_5folds) <- pop_param

  pb <- progress::progress_bar$new(
    format = "[:bar] :percent :elapsed elapsed / :eta remaining", total = length(pop_param) * 6, clear = FALSE, show_after = 0)
  for (i in pop_param) {
    pb$message(paste0("Searching covariate effects on ", i))
    pb$tick()
    
    y_xgb <- log(dat_XGB[, i])

    # Cross-validation
    ## create 5 partition of the data ( using K-1 folds (80%) as the training set and the remaining one fold (20%) as the test set repeating steps for K iterations )
    x <- as.data.frame(x_xgb)
    folds <- caret::createFolds(seq(1, nrow(x_xgb)), k = 5, list = TRUE, returnTrain = FALSE)

    for (j in 1:5) {
      pb$tick()
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
      lasso.coef <- coef(cvfit, s = cvfit$lambda.1se)[-1, ]
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
  result_ML <- data.frame(cov_selected = rep(NA, length(pop_param)))
  rownames(result_ML) <- pop_param

  res <- t(result_5folds[,1:5])
  res <- res %>% dplyr::na_if("")

  for (i in pop_param) {
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


  result_ML$RMSE <- rep(NA,length(pop_param))
  result_ML$RMSE_ref <- rep(NA,length(pop_param))



  # Evaluation of model with selected covariates

  for (i in pop_param) {
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
          xgb.mod <- generate_xgb.mod(data = training, label = y.xgm_train)
          
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
  for (i in pop_param) {
    y_xgb <- log(dat_XGB[, i])
    
    if (is.na(result_ML[i, 1]) == FALSE) {
      list_cov <- strsplit(gsub(" ", "", result_ML[i, 1]), ",")
      x.selected_final <-
        as.matrix(dat_XGB %>% dplyr::select(dplyr::all_of(list_cov[[1]])))
      
      if (length(list_cov[[1]]) != 0) {
        xgb.mod_final <- generate_xgb.mod(data = x.selected_final, label = y_xgb)
        
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
      pop_param = pop_param,
      cov_continuous = cov_continuous, 
      cov_factors = cov_factors,
      shap_data = shap_data,
      shap_seed = shap_seed
    ) %>% structure(class = "mlcov_data")
  )
} 

