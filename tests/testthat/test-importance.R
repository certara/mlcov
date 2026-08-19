describe("importance adapters", {
  it("resolves a getImp function for each supported algorithm", {
    rf <- boruta_importance_spec("randomForest")
    expect_type(rf$get_imp, "closure")
    xgb <- boruta_importance_spec("xgboost")
    expect_identical(xgb$extra$objective, "reg:squarederror")
    lgb <- boruta_importance_spec("lightgbm")
    expect_type(lgb$get_imp, "closure")
    catb <- boruta_importance_spec("catboost")
    expect_type(catb$get_imp, "closure")
  })

  it("returns a named importance vector from getImpXgboost", {
    skip_if_not_installed("xgboost")
    set.seed(1)
    x <- data.frame(a = rnorm(40), b = rnorm(40), c = rnorm(40))
    y <- x$a + rnorm(40, sd = 0.1)
    imp <- getImpXgboost(x, y, nrounds = 10, objective = "reg:squarederror")
    expect_equal(length(imp), 3)
    expect_equal(names(imp), names(x))
    expect_true(imp[["a"]] >= imp[["c"]])
  })

  it("returns a named importance vector from getImpLightGBM", {
    skip_if_not_installed("lightgbm")
    set.seed(1)
    x <- data.frame(
      a = rnorm(50),
      b = rnorm(50),
      g = factor(sample(c("x", "y"), 50, replace = TRUE))
    )
    y <- x$a + rnorm(50, sd = 0.1)
    imp <- getImpLightGBM(x, y)
    expect_equal(length(imp), 3)
    expect_equal(names(imp), names(x))
  })

  it("accepts Boruta's single-column cbind name that LightGBM would reject", {
    skip_if_not_installed("lightgbm")
    set.seed(1)
    x <- data.frame(
      check.names = FALSE,
      `x[, decReg != "Rejected"]` = rnorm(40),
      shadow1 = rnorm(40)
    )
    y <- x[[1]] + rnorm(40, sd = 0.1)
    imp <- getImpLightGBM(x, y)
    expect_equal(length(imp), 2)
    expect_equal(names(imp), names(x))
    expect_true(is.finite(imp[[1]]))
  })
})

describe("optional integration", {
  it("can recover a strong continuous signal with a short LightGBM search", {
    skip_on_cran()
    skip_if_not_installed("lightgbm")
    dat <- synthetic_mlcov_data(n = 120, seed = 42)
    result <- ml_cov_search(
      dat,
      pop_param = "CL",
      cov_continuous = c("WT", "AGE", "NOISE"),
      n_folds = 2,
      vote_threshold = 1,
      boruta_algorithm = "lightgbm",
      use_lasso = FALSE,
      boruta_max_runs = 20,
      seed = 42
    )
    selected <- parse_cov_selected(result$result_ML["CL", "cov_selected"])
    expect_true("WT" %in% selected)
  })
})
