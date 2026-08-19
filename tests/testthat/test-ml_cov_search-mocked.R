describe("ml_cov_search mocked search", {
  it("returns an mlcov_data object with settings and voted covariates", {
    dat <- synthetic_mlcov_data()
    result <- with_mocked_bindings(
      run_boruta = function(x, ...) {
        if ("WT" %in% names(x)) "WT" else names(x)[1]
      },
      ml_cov_search(
        dat,
        pop_param = c("CL", "V1"),
        cov_continuous = c("WT", "AGE", "NOISE"),
        cov_factors = c("SEX"),
        n_folds = 2,
        vote_threshold = 1,
        boruta_algorithm = "randomForest",
        use_lasso = FALSE,
        log_ebes = TRUE
      ),
      .package = "mlcov"
    )

    expect_s3_class(result, "mlcov_data")
    expect_true(is.data.frame(result$result_ML))
    expect_equal(ncol(result$result_5folds), 2)
    expect_identical(result$result_folds, result$result_5folds)
    expect_equal(result$settings$boruta_algorithm, "randomForest")
    expect_false(result$settings$use_lasso)
    expect_equal(result$settings$n_folds, 2L)
    expect_equal(result$settings$vote_threshold, 1L)
    expect_match(result$result_ML["CL", "cov_selected"], "WT")
  })

  it("leaves a fold empty when Lasso selects nothing", {
    dat <- synthetic_mlcov_data()
    result <- with_mocked_bindings(
      apply_lasso_filter = function(...) NULL,
      run_boruta = function(...) stop("Boruta should not run"),
      ml_cov_search(
        dat,
        pop_param = "CL",
        cov_continuous = c("WT", "AGE"),
        cov_factors = "SEX",
        n_folds = 2,
        vote_threshold = 1,
        boruta_algorithm = "randomForest",
        use_lasso = TRUE
      ),
      .package = "mlcov"
    )
    expect_true(all(is.na(result$result_5folds["CL", ])))
    expect_true(is.na(result$result_ML["CL", "cov_selected"]))
  })

  it("does not log-transform EBEs when log_ebes is FALSE", {
    dat <- synthetic_mlcov_data()
    seen_y <- NULL
    result <- with_mocked_bindings(
      run_boruta = function(x, y, ...) {
        seen_y <<- y
        "WT"
      },
      ml_cov_search(
        dat,
        pop_param = "CL",
        cov_continuous = "WT",
        n_folds = 2,
        vote_threshold = 1,
        boruta_algorithm = "randomForest",
        use_lasso = FALSE,
        log_ebes = FALSE
      ),
      .package = "mlcov"
    )
    expect_false(isTRUE(result$settings$log_ebes))
    expect_equal(mean(seen_y), mean(dat$CL), tolerance = 1)
  })
})

describe("print.mlcov_data", {
  it("prints algorithm settings and selected covariates", {
    result <- stub_mlcov_result()
    output <- paste(capture.output(print(result)), collapse = "\n")
    expect_match(output, "lightgbm")
    expect_match(output, "lambda.min")
    expect_match(output, "WT")
  })
})
