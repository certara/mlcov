describe("ml_cov_search argument validation", {
  it("errors when no covariates are supplied", {
    dat <- synthetic_mlcov_data()
    expect_error(
      ml_cov_search(dat, pop_param = "CL"),
      "No covariates specified"
    )
  })

  it("errors when requested columns are missing from the data", {
    dat <- synthetic_mlcov_data()
    expect_error(
      ml_cov_search(
        dat,
        pop_param = "clearance",
        cov_continuous = "weight",
        cov_factors = "dIaB"
      ),
      "missing in the dataset"
    )
  })

  it("errors when data has no ID column", {
    dat <- synthetic_mlcov_data()
    dat$ID <- NULL
    expect_error(
      ml_cov_search(
        dat,
        pop_param = "CL",
        cov_continuous = "WT",
        cov_factors = "SEX"
      ),
      "ID"
    )
  })

  it("errors when analysis columns are not unique within ID", {
    dat <- synthetic_mlcov_data(n = 8)
    extra <- dat[1, , drop = FALSE]
    extra$WT <- extra$WT + 5
    dat <- rbind(dat, extra)
    expect_error(
      ml_cov_search(
        dat,
        pop_param = "CL",
        cov_continuous = "WT",
        cov_factors = "SEX",
        use_lasso = FALSE,
        n_folds = 2,
        vote_threshold = 1,
        boruta_algorithm = "randomForest",
        boruta_max_runs = 11
      ),
      "not unique within ID"
    )
  })

  it("errors when vote_threshold exceeds n_folds", {
    dat <- synthetic_mlcov_data()
    expect_error(
      ml_cov_search(
        dat,
        pop_param = "CL",
        cov_continuous = "WT",
        n_folds = 3,
        vote_threshold = 4,
        boruta_algorithm = "randomForest",
        use_lasso = FALSE
      ),
      "vote_threshold"
    )
  })

  it("errors when boruta_pvalue is outside (0, 1)", {
    dat <- synthetic_mlcov_data()
    expect_error(
      ml_cov_search(
        dat,
        pop_param = "CL",
        cov_continuous = "WT",
        boruta_pvalue = 0,
        boruta_algorithm = "randomForest",
        use_lasso = FALSE
      ),
      "boruta_pvalue"
    )
  })

  it("errors when log_ebes is TRUE and EBEs are not strictly positive", {
    dat <- synthetic_mlcov_data()
    dat$CL[1] <- 0
    expect_error(
      with_mocked_bindings(
        run_boruta = function(...) "WT",
        ml_cov_search(
          dat,
          pop_param = "CL",
          cov_continuous = "WT",
          n_folds = 2,
          vote_threshold = 1,
          boruta_algorithm = "randomForest",
          use_lasso = FALSE,
          log_ebes = TRUE
        ),
        .package = "mlcov"
      ),
      "strictly positive"
    )
  })

  it("errors for an unknown boruta_algorithm", {
    dat <- synthetic_mlcov_data()
    expect_error(
      ml_cov_search(
        dat,
        pop_param = "CL",
        cov_continuous = "WT",
        boruta_algorithm = "svm"
      ),
      "should be one of"
    )
  })
})
