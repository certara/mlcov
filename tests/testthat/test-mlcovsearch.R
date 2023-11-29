get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

# Read in tab2 dataset
data <- read.table(system.file(package = "mlcov", "supplementary", "tab2"), skip = 1, header = T)

# Search and select covariates. This function can take a few minutes to run
result <- suppressWarnings(MLCovSearch(data, #NONMEM output
                      list_pop_param = c("V1","CL"),
                      cov_continuous = c("AGE","WT","HT","BMI","ALB","CRT",
                                         "FER","CHOL","WBC","LYPCT","RBC",
                                         "HGB","HCT","PLT"),
                      cov_factors = c("SEX","RACE","DIAB","ALQ","WACT","SMQ")))

testthat::test_that("MLCovSearch result_ML is a data.frame with only one covariate selection", {
  
  testthat::expect_true(is.data.frame(result$result_ML))
  
  # Currently the function returns a row of NA in the demo. This count excludes that
  testthat::expect_true(sum(!is.na(result$result_ML$cov_selected)) == 1)
})

testthat::test_that("MLCovSearch result_5folds is a data.frame with 5 non-NA covariates selected", {
  
  testthat::expect_true(is.data.frame(result$result_5folds))
  testthat::expect_true(sum(!is.na(result$result_5folds[1,])) == 5)
})

testthat::test_that("MLCovSearch shap plots do not change", {
  testthat::skip_if_not(get_os() == "windows")
  vdiffr::expect_doppelganger("shap plots", result$shap_plots)
})
