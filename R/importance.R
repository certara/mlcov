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
#' Hyperparameters match the v2 manuscript: regression objective, RMSE metric,
#' learning rate 0.1, 31 leaves, 200 boosting iterations.
#'
#' @param x Data frame of predictors, including Boruta shadow features.
#' @param y Response vector.
#' @param ... Ignored; accepted for Boruta compatibility.
#' @return Named numeric vector of gain importance, one value per column of `x`.
#' @keywords internal
#' @noRd
getImpLightGBM <- function(x, y, ...) {
  x <- as.data.frame(x, optional = TRUE, stringsAsFactors = FALSE)
  orig_names <- colnames(x)
  if (is.null(orig_names) || length(orig_names) != ncol(x)) {
    orig_names <- paste0("V", seq_len(ncol(x)))
  }
  is_cat <- vapply(x, is.factor, logical(1))

  mat <- vapply(
    seq_along(x),
    function(j) {
      col <- x[[j]]
      if (is_cat[[j]]) {
        as.numeric(as.integer(col) - 1L)
      } else {
        as.numeric(col)
      }
    },
    numeric(nrow(x))
  )
  if (is.null(dim(mat))) {
    mat <- matrix(mat, ncol = 1L)
  }
  # LightGBM rejects JSON-special characters in names. Boruta's single-column
  # cbind() labels that column `x[, decReg != "Rejected"]`, which contains
  # `[`, `]`, `,`, and `"`. Use positional names internally and map back.
  safe_names <- paste0("f", seq_len(ncol(mat)))
  colnames(mat) <- safe_names
  cat_idx <- as.integer(which(is_cat) - 1L)

  dtrain <- lightgbm::lgb.Dataset(
    data = mat,
    label = as.numeric(y),
    categorical_feature = if (length(cat_idx) > 0L) cat_idx else NULL
  )

  params <- list(
    objective = "regression",
    metric = "rmse",
    boosting = "gbdt",
    learning_rate = 0.1,
    num_leaves = 31L,
    verbosity = -1L
  )

  model <- lightgbm::lgb.train(
    params = params,
    data = dtrain,
    nrounds = 200L
  )

  importance <- lightgbm::lgb.importance(model)
  importance_vector <- stats::setNames(rep(0, ncol(mat)), orig_names)

  if (!is.null(importance) && nrow(importance) > 0) {
    feat <- as.character(importance$Feature)
    gain <- importance$Gain
    mapped <- match(feat, safe_names)
    ok <- !is.na(mapped)
    importance_vector[mapped[ok]] <- gain[ok]
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
    randomForest = list(get_imp = Boruta::getImpRfZ, extra = list()),
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
  get_imp_positional <- function(x_imp, y_imp, ...) {
    x_imp <- as.data.frame(x_imp, optional = TRUE, stringsAsFactors = FALSE)
    colnames(x_imp) <- paste0("f", seq_len(ncol(x_imp)))
    get_imp(x_imp, y_imp, ...)
  }
  comment(get_imp_positional) <- comment(get_imp)

  boruta_obj <- do.call(
    Boruta::Boruta,
    c(
      list(
        x,
        y = y,
        pValue = pValue,
        maxRuns = maxRuns,
        doTrace = 0,
        getImp = get_imp_positional
      ),
      extra
    )
  )
  boruta_df <- Boruta::attStats(boruta_obj)
  row.names(boruta_df)[boruta_df$decision == "Confirmed"]
}
