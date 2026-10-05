# Boruta importance adapters used by ml_cov_search().
# Not exported. Each adapter follows Boruta's getImp(x, y, ...) contract.

#' XGBoost gain importance for Boruta
#'
#' Restored from Boruta 8.0.0 (`getImpXgboost`), which was removed in Boruta 10.
#' Licensed under GPL (>= 2); original authors Miron B. Kursa and Witold R. Rudnicki.
#'
#' @param x Data frame of predictors, including Boruta shadow features.
#' @param y Response vector.
#' @param nrounds Number of boosting rounds passed to [xgboost::xgboost()].
#' @param verbose Verbosity passed to [xgboost::xgboost()].
#' @param ... Additional arguments passed to [xgboost::xgboost()].
#' @return Named numeric vector of gain importance, one value per column of `x`.
#' @keywords internal
#' @noRd
getImpXgboost <- function(x, y, nrounds = 5, verbose = 0, ...) {
  for (e in seq_len(ncol(x))) {
    x[, e] <- as.numeric(x[, e])
  }
  dots <- list(...)
  if (is.null(dots$verbosity) && is.null(dots$verbose)) {
    dots$verbosity <- verbose
  }
  model <- do.call(
    fit_xgb_regressor,
    c(list(x = x, y = y, nrounds = nrounds), dots)
  )
  imp <- xgboost::xgb.importance(model = model)
  ans <- stats::setNames(rep(0, ncol(x)), colnames(x))
  if (!is.null(imp) && nrow(imp) > 0) {
    ans[imp$Feature] <- imp$Gain
  }
  ans
}
comment(getImpXgboost) <- "xgboost gain importance"

#' LightGBM gain importance for Boruta
#'
#' Matches the simulation-study adapter: column names are passed through
#' [make.names()], factor columns are supplied to LightGBM by name, and the
#' predictor frame is [as.matrix()]. Factors are not integer-coded. On
#' lightgbm 4.x that matrix is coerced to double, which is the encoding the
#' published Boruta selections used.
#'
#' Hyperparameters match the study: regression objective, RMSE metric,
#' learning rate 0.1, 31 leaves, 200 boosting iterations.
#'
#' @param x Data frame of predictors, including Boruta shadow features.
#' @param y Response vector.
#' @param ... Ignored; accepted for Boruta compatibility.
#' @return Named numeric vector of gain importance. Names are [make.names()]
#'   versions of `colnames(x)`.
#' @keywords internal
#' @noRd
getImpLightGBM <- function(x, y, ...) {
  # make.names() also keeps LightGBM from rejecting Boruta's one-column
  # label `x[, decReg != "Rejected"]`.
  colnames(x) <- make.names(colnames(x), unique = TRUE)

  categorical_features <- names(x)[vapply(x, is.factor, logical(1))]
  if (length(categorical_features) > 0) {
    x[categorical_features] <- lapply(x[categorical_features], as.factor)
  }

  # lightgbm 4.x coerces as.matrix() output to double and warns when factor
  # labels are not numeric. That coercion is the simulation-study encoding.
  dtrain <- withCallingHandlers(
    lightgbm::lgb.Dataset(
      data = as.matrix(x),
      label = y,
      categorical_feature = categorical_features
    ),
    warning = function(w) {
      if (grepl("NAs introduced by coercion", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  params <- list(
    objective = "regression",
    metric = "rmse",
    boosting = "gbdt",
    learning_rate = 0.1,
    num_leaves = 31,
    verbosity = -1
  )

  model <- lightgbm::lgb.train(
    params = params,
    data = dtrain,
    nrounds = 200
  )

  importance <- lightgbm::lgb.importance(model)
  importance_vector <- stats::setNames(rep(0, ncol(x)), colnames(x))

  if (!is.null(importance) && nrow(importance) > 0) {
    importance_vector[importance$Feature] <- importance$Gain
  }

  importance_vector
}
comment(getImpLightGBM) <- "lightgbm gain importance"

#' CatBoost feature importance for Boruta
#'
#' Hyperparameters match the v2 manuscript: RMSE loss, depth 5, learning rate
#' 0.05, 100 iterations, and small L2 regularization.
#'
#' @param x Data frame of predictors, including Boruta shadow features.
#' @param y Response vector.
#' @param ... Ignored; accepted for Boruta compatibility.
#' @return Named numeric vector of feature importance, one value per column of `x`.
#' @keywords internal
#' @noRd
getImpCatBoost <- function(x, y, ...) {
  if (!requireNamespace("catboost", quietly = TRUE)) {
    stop(
      "The 'catboost' package is required for boruta_algorithm = \"catboost\". ",
      "It is not on CRAN; see https://catboost.ai/en/docs/installation/r-installation ",
      "for install instructions.",
      call. = FALSE
    )
  }

  catboost_data <- catboost::catboost.load_pool(
    data = x,
    label = y
  )

  params <- list(
    loss_function = "RMSE",
    depth = 5,
    learning_rate = 0.05,
    iterations = 100,
    l2_leaf_reg = 0.001,
    rsm = 0.95,
    border_count = 64,
    logging_level = "Silent",
    allow_writing_files = FALSE
  )

  model <- catboost::catboost.train(
    learn_pool = catboost_data,
    params = params
  )

  importance <- catboost::catboost.get_feature_importance(
    model,
    pool = catboost_data,
    type = "FeatureImportance"
  )
  importance <- as.numeric(importance)
  names(importance) <- colnames(x)
  importance
}
comment(getImpCatBoost) <- "catboost feature importance"

#' Stop when the random-forest learner cannot run
#'
#' [Boruta::getImpRfZ()] fits with [ranger::ranger()]. `ranger` is suggested,
#' not imported, because random forest is not the default learner.
#'
#' @return `TRUE`, invisibly, when `ranger` is installed.
#' @keywords internal
#' @noRd
ensure_ranger <- function() {
  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop(
      "The 'ranger' package is required for boruta_algorithm = \"randomForest\". ",
      "Install it with install.packages(\"ranger\").",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Ranger permutation importance for Boruta
#'
#' Thin wrapper around [Boruta::getImpRfZ()] that fails with an install hint
#' when `ranger` is missing.
#'
#' @param x Data frame of predictors, including Boruta shadow features.
#' @param y Response vector.
#' @param ... Passed to [Boruta::getImpRfZ()].
#' @return Named numeric vector of permutation importance.
#' @keywords internal
#' @noRd
getImpRanger <- function(x, y, ...) {
  ensure_ranger()
  Boruta::getImpRfZ(x, y, ...)
}
comment(getImpRanger) <- comment(Boruta::getImpRfZ)

#' Resolve the Boruta importance function and extra arguments for a learner
#'
#' @param boruta_algorithm One of `"randomForest"`, `"xgboost"`, `"lightgbm"`,
#'   `"catboost"`.
#' @return A list with `get_imp` (function) and `extra` (named list).
#' @keywords internal
#' @noRd
boruta_importance_spec <- function(boruta_algorithm) {
  switch(
    boruta_algorithm,
    randomForest = list(get_imp = getImpRanger, extra = list()),
    xgboost = list(
      get_imp = getImpXgboost,
      extra = list(nrounds = 200, objective = "reg:squarederror")
    ),
    lightgbm = list(get_imp = getImpLightGBM, extra = list()),
    catboost = list(get_imp = getImpCatBoost, extra = list()),
    stop("Unsupported boruta_algorithm: ", boruta_algorithm, call. = FALSE)
  )
}

#' Run Boruta and return names of confirmed features
#'
#' Wrapped so unit tests can mock the search without calling Boruta.
#'
#' @param x Predictor data frame.
#' @param y Response vector.
#' @param pValue Boruta significance level.
#' @param maxRuns Maximum Boruta runs.
#' @param get_imp Importance function.
#' @param extra Extra arguments passed to [Boruta::Boruta()].
#' @return Character vector of confirmed feature names (possibly empty).
#' @keywords internal
#' @noRd
run_boruta <- function(x, y, pValue, maxRuns, get_imp, extra) {
  boruta_obj <- do.call(
    Boruta::Boruta,
    c(
      list(
        x,
        y = y,
        pValue = pValue,
        maxRuns = maxRuns,
        doTrace = 0,
        getImp = get_imp
      ),
      extra
    )
  )
  boruta_df <- Boruta::attStats(boruta_obj)
  row.names(boruta_df)[boruta_df$decision == "Confirmed"]
}
