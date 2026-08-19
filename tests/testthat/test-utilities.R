describe("encoding and voting helpers", {
  it("dummy-encodes multi-level factors and maps columns back to the original name", {
    dat <- synthetic_mlcov_data(n = 30)
    encoded <- dummy_encode_predictors(
      dat[, c("WT", "SEX", "RACE")],
      cov_factors = c("SEX", "RACE")
    )
    expect_true("WT" %in% colnames(encoded$X))
    expect_true("SEX" %in% colnames(encoded$X))
    expect_false("RACE" %in% colnames(encoded$X))
    race_dummies <- names(encoded$map)[encoded$map == "RACE"]
    expect_true(length(race_dummies) >= 1)
    expect_equal(unique(unname(encoded$map[race_dummies])), "RACE")
  })

  it("keeps binary factors as a 0/1 column with the original name in the XGBoost frame", {
    pop <- data.frame(CL = c(1, 2, 3))
    factors <- data.frame(SEX = factor(c("0", "1", "0")))
    continuous <- data.frame(WT = c(60, 70, 80))
    out <- prepare_xgb_frame(pop, factors, continuous)
    expect_true("SEX" %in% names(out))
    expect_equal(sort(unique(out$SEX)), c(0, 1))
  })

  it("dummy-encodes multi-level factors in the XGBoost frame", {
    pop <- data.frame(CL = 1:3)
    factors <- data.frame(RACE = factor(c("White", "Black", "Asian")))
    continuous <- data.frame(WT = c(60, 70, 80))
    out <- prepare_xgb_frame(pop, factors, continuous)
    expect_false("RACE" %in% names(out))
    expect_true(any(startsWith(names(out), "RACE")))
  })

  it("ignores NA fold cells when voting so they cannot become a covariate named NA", {
    folds <- data.frame(
      fold1 = c("WT, SEX", NA_character_),
      fold2 = c("WT", ""),
      fold3 = c(NA_character_, NA_character_),
      stringsAsFactors = FALSE
    )
    rownames(folds) <- c("CL", "V1")
    voted <- vote_covariates(folds, c("CL", "V1"), vote_threshold = 2)
    expect_equal(voted["CL", "cov_selected"], "WT")
    expect_true(is.na(voted["V1", "cov_selected"]))
    expect_false(any(grepl("NA", voted$cov_selected, fixed = TRUE), na.rm = TRUE))
  })

  it("respects vote_threshold when counting fold frequency", {
    folds <- data.frame(
      fold1 = "WT",
      fold2 = "WT",
      fold3 = "AGE",
      stringsAsFactors = FALSE
    )
    rownames(folds) <- "CL"
    at_two <- vote_covariates(folds, "CL", vote_threshold = 2)
    at_three <- vote_covariates(folds, "CL", vote_threshold = 3)
    expect_equal(at_two["CL", "cov_selected"], "WT")
    expect_true(is.na(at_three["CL", "cov_selected"]))
  })

  it("parses comma-separated selections and drops empty tokens", {
    expect_equal(parse_cov_selected("WT, SEX"), c("WT", "SEX"))
    expect_equal(parse_cov_selected(NA_character_), character())
    expect_equal(parse_cov_selected(""), character())
  })

  it("expands original factor names onto XGBoost dummy columns", {
    xgb_names <- c("WT", "AGE", "SEX", "RACEBlack", "RACEWhite")
    expanded <- expand_to_xgb_columns(
      selected = c("WT", "RACE"),
      xgb_names = xgb_names,
      cov_continuous = c("WT", "AGE"),
      cov_factors = c("SEX", "RACE")
    )
    expect_true("WT" %in% expanded)
    expect_true(all(c("RACEBlack", "RACEWhite") %in% expanded))
    expect_false("RACE" %in% expanded)
  })

  it("maps Lasso dummy names back to original covariates for tree learners", {
    dat <- synthetic_mlcov_data(n = 40)
    training <- dat[, c("WT", "AGE", "SEX", "RACE")]
    y <- log(dat$CL)
    out <- apply_lasso_filter(
      training = training,
      y = y,
      use_lasso = TRUE,
      lambda_lasso = "lambda.min",
      n_folds = 5,
      cov_factors = c("SEX", "RACE"),
      keep_dummies = FALSE
    )
    if (!is.null(out)) {
      expect_true(all(names(out) %in% names(training)))
      expect_false(any(startsWith(names(out), "RACE") & names(out) != "RACE"))
    }
  })
})
