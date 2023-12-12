`%>%` <- dplyr::`%>%`

col_select <- function(tab, list_pop_param, cov_continuous, cov_factors) {
  
  # Selection of columns required
  tab <- tab %>%
    dplyr::select(
      ID,
      dplyr::all_of(list_pop_param),
      dplyr::all_of(cov_continuous),
      dplyr::all_of(cov_factors)
    )
  
  # In order to have the individual parameter and one point per subject
  dat <- unique(tab) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(cov_factors), as.factor))
  
  return(dat)
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
  
  if (length(errors) > 0) {
    stop(paste0(errors, sep = "\n"), call. = FALSE)
  }
}