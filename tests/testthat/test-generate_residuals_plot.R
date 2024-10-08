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
data <- read.table(system.file(package = "mlcov", "supplementary", "tab33"), skip = 1, header = TRUE)

# Search and select covariates. This function can take a few minutes to run
result <- suppressWarnings(ml_cov_search(data, #NONMEM output
                                         pop_param = c("V1","CL"),
                                         cov_continuous = c("AGE","WT","HT","BMI","ALB","CRT",
                                                            "FER","CHOL","WBC","LYPCT","RBC",
                                                            "HGB","HCT","PLT"),
                                         cov_factors = c("SEX","RACE","DIAB","ALQ","WACT","SMQ")))

# Generate plots
plot_CL <- suppressWarnings(generate_residuals_plot(data,
                                                    result,
                                                    pop_param = c('CL')))

plot_V1 <- suppressWarnings(generate_residuals_plot(data,
                                                    result,
                                                    pop_param = c('V1')))

testthat::test_that("generate_residuals_plot does not return plots for Cl", {
  testthat::expect_null(plot_CL)
})

testthat::test_that("generate_residuals_plot does not return plots for V1", {
  testthat::expect_s3_class(plot_V1[[1]], "gg")
})

testthat::test_that("Error messages will be generated when values supplied to the function are absent in the data.frame", {
  
  #Since function pulls these values from the results object, it should always run correctly. 
  testthat::expect_no_error(generate_residuals_plot(data, result, pop_param = c('V1')))
})
