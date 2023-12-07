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

testthat::test_that("generate_shap_plots default plots do not change", {
  testthat::skip_if_not(get_os() == "windows")
  
  test.plots.default <- suppressWarnings(generate_shap_summary_plot(result))
  
  vdiffr::expect_doppelganger("default shap plots", test.plots.default)
})

testthat::test_that("generate_shap_plots custom plots do not change", {
  testthat::skip_if_not(get_os() == "windows")
  
  test.plots.custom <-
    suppressWarnings(
      generate_shap_summary_plot(
        result,
        x_bound = NULL,
        dilute = TRUE,
        scientific = TRUE,
        my_format = NULL,
        min_color_bound = "#336699",
        max_color_bound = "#CC0066",
        kind = "sina",
        title = "Test Title",
        title.position = 0.5,
        ylab = "Test y-axis",
        xlab = "Test x-axis"
      )
    )
  
  vdiffr::expect_doppelganger("custom shap plots", test.plots.custom)
})