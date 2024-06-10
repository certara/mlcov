`%>%` <- dplyr::`%>%`

# Select columns to generate data for XGBoost
col_select <- function(data, pop_param, cov_continuous, cov_factors) {
  
  # Selection of columns required
  data <- data %>%
    dplyr::select(
      ID,
      dplyr::all_of(pop_param),
      dplyr::all_of(cov_continuous),
      dplyr::all_of(cov_factors)
    )
  
  # In order to have the individual parameter and one point per subject
  dat <- unique(data) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(cov_factors), as.factor))
  
  return(dat)
}



# Check that covariates supplied by user exist in the data
data_validation <- function(data, pop_param, cov_continuous, cov_factors) {
  errors <- c()
  
  for (i in 1:3) {
    vectors <- list(pop_param, cov_continuous, cov_factors)
    vector_names <- c("pop_param", "cov_continuous", "cov_factors")
    
    missing_values <- setdiff(vectors[[i]], colnames(data))
    
    if (length(missing_values) > 0) {
      error_message <-
        paste( "The following values from", vector_names[[i]], "are missing in the dataset:", toString(missing_values))
      
      errors <- c(errors, error_message)
    }
  }
  
  if (length(errors) > 0) {
    stop(paste0(errors, sep = "\n"), call. = FALSE)
  }
}



# One-hot encoding of categorical covariates for covariates with more than 2 levels
generate_dat_XGB <- function(pop_param, factors, continuous) {
  
  modified_columns <- data.frame(matrix(ncol = 0, nrow = nrow(factors)))
  
  for (col in names(factors)) {
    if (is.factor(factors[[col]]) && nlevels(factors[[col]]) > 2) {
      dmy <- caret::dummyVars(paste0("~", col), data = factors)
      encoded <- data.frame(predict(dmy, newdata = factors))
      modified_columns <- cbind(modified_columns,encoded[,-1])
    } else {
      modified_columns[[col]] <- as.numeric(as.character(factors[[col]]))
    }
  }
  cbind(pop_param, modified_columns, continuous)
}



# Create xgb.mod dataframe
generate_xgb.mod <- function(data, label) {
  xgb.mod <- xgboost::xgboost(
    data = data,
    label = label,
    nrounds = 200,
    objective = "reg:squarederror",
    verbose = 0
  )
}
