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
        boruta_algorithm = "lightgbm",
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
          boruta_algorithm = "lightgbm",
          use_lasso = FALSE,
          log_ebes = TRUE
        ),
        .package = "mlcov"
      ),
      "strictly positive"
    )
  })

  it("errors when ranger is not installed for random forest", {
    dat <- synthetic_mlcov_data()
    expect_error(
      with_mocked_bindings(
        ranger_is_installed = function() FALSE,
        ml_cov_search(
          dat,
          pop_param = "CL",
          cov_continuous = "WT",
          boruta_algorithm = "randomForest",
          use_lasso = FALSE
        ),
        .package = "mlcov"
      ),
      "ranger"
    )
  })

  it("errors when a parameter is also listed as a covariate", {
    dat <- synthetic_mlcov_data()
    expect_error(
      ml_cov_search(
        dat,
        pop_param = "CL",
        cov_continuous = c("CL", "WT")
      ),
      "Overlap"
    )
  })

  it("errors when a covariate is listed as both continuous and categorical", {
    dat <- synthetic_mlcov_data()
    expect_error(
      ml_cov_search(
        dat,
        pop_param = "CL",
        cov_continuous = "SEX",
        cov_factors = "SEX"
      ),
      "both continuous and categorical"
    )
  })

  it("errors when fold arguments are not finite whole numbers", {
    dat <- synthetic_mlcov_data()
    expect_error(
      ml_cov_search(
        dat,
        pop_param = "CL",
        cov_continuous = "WT",
        n_folds = 2.9,
        use_lasso = FALSE
      ),
      "n_folds"
    )
    expect_error(
      ml_cov_search(
        dat,
        pop_param = "CL",
        cov_continuous = "WT",
        n_folds = Inf,
        use_lasso = FALSE
      ),
      "n_folds"
    )
    expect_error(
      ml_cov_search(
        dat,
        pop_param = "CL",
        cov_continuous = "WT",
        vote_threshold = 1.5,
        use_lasso = FALSE
      ),
      "vote_threshold"
    )
    expect_error(
      ml_cov_search(
        dat,
        pop_param = "CL",
        cov_continuous = "WT",
        boruta_max_runs = 10.2,
        use_lasso = FALSE
      ),
      "boruta_max_runs"
    )
  })

  it("errors when n_folds exceeds the number of unique subjects", {
    dat <- synthetic_mlcov_data(n = 3)
    expect_error(
      ml_cov_search(
        dat,
        pop_param = "CL",
        cov_continuous = "WT",
        n_folds = 4,
        vote_threshold = 1,
        use_lasso = FALSE,
        boruta_algorithm = "lightgbm"
      ),
      "unique subjects"
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
