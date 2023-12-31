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

# Generate plots
plot_CL <- suppressWarnings(generate_residualsplots(data,
                                   list_pop_param = c("V1","CL"),
                                   cov_continuous = c("AGE","WT","HT","BMI","ALB","CRT",
                                                      "FER","CHOL","WBC","LYPCT","RBC",
                                                      "HGB","HCT","PLT"),
                                   cov_factors = c("SEX","RACE","DIAB","ALQ","WACT","SMQ"),
                                   result_ML = result$result_ML, 
                                   result_5folds = result$result_5folds, 
                                   i=c('CL')))

plot_V1 <- suppressWarnings(generate_residualsplots(data,
                                list_pop_param = c("V1","CL"),
                                cov_continuous = c("AGE","WT","HT","BMI","ALB","CRT",
                                                   "FER","CHOL","WBC","LYPCT","RBC",
                                                   "HGB","HCT","PLT"),
                                cov_factors = c("SEX","RACE","DIAB","ALQ","WACT","SMQ"),
                                result_ML = result$result_ML, 
                                result_5folds = result$result_5folds, 
                                i=c('V1')))

testthat::test_that("generate_residualsplots does not return plots for Cl", {
  testthat::expect_null(plot_CL)
})

testthat::test_that("generate_residualsplots returns a list of 3 plots for V1", {
  
  testthat::expect_true(is.list(plot_V1))
  testthat::expect_length(plot_V1, 3)
})

testthat::test_that("test-generate_residualsplots returns the same plots each time for V1", {
  testthat::skip_if_not(get_os() == "windows")
  vdiffr::expect_doppelganger("HT Residuals", plot_V1$HT)
  vdiffr::expect_doppelganger("PLT Residuals", plot_V1$PLT)
  vdiffr::expect_doppelganger("SEX Residuals", plot_V1$SEX)
})

testthat::test_that("Error messages will be generated when values supplied to the function are absent in the data.frame", {
  
  testthat::expect_error(generate_residualsplots(tab = data,
                                                 list_pop_param = c("clearance"),
                                                 cov_continuous = c("weight"),
                                                 cov_factors = c("dIaB"),
                                                 result_ML = result$result_ML, #selected covariates for parameter of interest after the voting
                                                 result_5folds = result$result_5folds, #selected covariates for parameter of interest for each folds
                                                 i=c('CL')))
})

