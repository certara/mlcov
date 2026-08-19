`%>%` <- dplyr::`%>%`

#' Machine-learning covariate search
#'
#' Identifies covariates associated with empirical Bayes estimates (EBEs) of
#' population parameters using optional Lasso pre-screening, Boruta feature
#' selection with a tree-based learner, and a fold-wise voting rule.
#'
#' The eight specification variants
#' (random forest / XGBoost / LightGBM / CatBoost, each with or without Lasso)
#' are consolidated here. Defaults follow the recommended workflow from the
#' v2 evaluation study: Lasso with `lambda.min` plus Boruta-LightGBM.
#'
#' Diagnostic SHAP and residual plots are **not** computed here; use
#' [generate_shap_summary_plot()] and [generate_residuals_plot()] afterwards.
#'
#' @param data A data frame containing EBEs, covariates, and an `ID` column
#'   (used to keep one row per subject).
#' @param pop_param Character vector of population parameter (EBE) names.
#' @param cov_continuous Character vector of continuous covariate names.
#'   Optional if `cov_factors` is supplied.
#' @param cov_factors Character vector of categorical covariate names.
#'   Optional if `cov_continuous` is supplied. Multi-level factors are
#'   dummy-encoded for XGBoost and Lasso; tree-based Boruta learners keep
#'   them as factors.
#' @param seed Numeric seed passed to [set.seed()] at the start of each
#'   parameter search. Defaults to 123.
#' @param use_lasso Logical. If `TRUE` (default), Lasso pre-filters covariates
#'   within each training fold before Boruta.
#' @param lambda_lasso Lasso penalty chosen by [glmnet::cv.glmnet()]:
#'   `"lambda.min"` (default) or `"lambda.1se"`.
#' @param boruta_algorithm Base learner used by Boruta. One of `"lightgbm"`
#'   (default), `"randomForest"`, `"xgboost"`, or `"catboost"`. CatBoost
#'   requires the optional `catboost` package.
#' @param boruta_pvalue Significance level passed to [Boruta::Boruta()].
#'   Defaults to 0.01.
#' @param n_folds Number of outer cross-validation folds (caret 80/20
#'   train/test splits). Defaults to 5. Inner Lasso CV uses at least 3 folds.
#' @param vote_threshold Minimum number of folds in which a covariate must be
#'   confirmed to be retained (`Freq >= vote_threshold`). Defaults to 2.
#'   The v2 manuscript majority rule ("more than two out of five") corresponds
#'   to `vote_threshold = 3`.
#' @param log_ebes Logical. If `TRUE` (default), EBEs are log-transformed
#'   before model fitting and must be strictly positive.
#' @param boruta_max_runs Maximum Boruta iterations (`maxRuns`). Defaults to
#'   200. The v2 manuscript describes up to 100 iterations; pass
#'   `boruta_max_runs = 100` to match that setting.
#'
#' @return An object of class `mlcov_data` with:
#' \item{result_ML}{Data frame of voted covariates per parameter (`cov_selected`).}
#' \item{result_5folds}{Per-fold confirmed covariates (name kept for compatibility
#'   even when `n_folds` is not 5).}
#' \item{result_folds}{Identical to `result_5folds`.}
#' \item{pop_param, cov_continuous, cov_factors}{The names supplied by the caller.}
#' \item{settings}{List of resolved arguments (`use_lasso`, `lambda_lasso`,
#'   `boruta_algorithm`, `boruta_pvalue`, `n_folds`, `vote_threshold`,
#'   `log_ebes`, `boruta_max_runs`, `seed`).}
#'
#' @examples
#' \dontrun{
#' result <- ml_cov_search(
#'   data = my_data,
#'   pop_param = c("CL", "V1"),
#'   cov_continuous = c("AGE", "WT"),
#'   cov_factors = c("SEX", "RACE"),
#'   boruta_algorithm = "lightgbm",
#'   use_lasso = TRUE,
#'   lambda_lasso = "lambda.min"
#' )
#' }
#'
#' @export
ml_cov_search <- function(data,
                          pop_param,
                          cov_continuous,
                          cov_factors,
                          seed = 123,
                          use_lasso = TRUE,
                          lambda_lasso = c("lambda.min", "lambda.1se"),
                          boruta_algorithm = c("lightgbm", "randomForest",
                                               "xgboost", "catboost"),
                          boruta_pvalue = 0.01,
                          n_folds = 5L,
                          vote_threshold = 2L,
                          log_ebes = TRUE,
                          boruta_max_runs = 200L) {

  lambda_lasso <- match.arg(lambda_lasso)
  boruta_algorithm <- match.arg(boruta_algorithm)

  if (missing(cov_continuous) && missing(cov_factors)) {
    stop(
      "No covariates specified. Use `cov_continuous` and/or `cov_factors` ",
      "to specify covariates to include in `ml_cov_search()`.",
      call. = FALSE
    )
  }
  if (missing(cov_continuous) || is.null(cov_continuous)) {
    cov_continuous <- character()
  }
  if (missing(cov_factors) || is.null(cov_factors)) {
    cov_factors <- character()
  }

  if (is.null(pop_param) || length(pop_param) == 0) {
    stop("`pop_param` must contain at least one parameter name.", call. = FALSE)
  }

  data_validation(data, pop_param, cov_continuous, cov_factors)
  validate_ml_cov_search_args(
    seed = seed,
    use_lasso = use_lasso,
    boruta_pvalue = boruta_pvalue,
    n_folds = n_folds,
    vote_threshold = vote_threshold,
    log_ebes = log_ebes,
    boruta_max_runs = boruta_max_runs,
    boruta_algorithm = boruta_algorithm
  )

  n_folds <- as.integer(n_folds)
  vote_threshold <- as.integer(vote_threshold)
  boruta_max_runs <- as.integer(boruta_max_runs)
  use_lasso <- isTRUE(use_lasso)
  log_ebes <- isTRUE(log_ebes)
  use_matrix <- identical(boruta_algorithm, "xgboost")

  data <- col_select(data, pop_param, cov_continuous, cov_factors)
  pop_parameters <- data %>% dplyr::select(dplyr::all_of(pop_param))
  factors <- if (length(cov_factors) > 0) {
    data %>% dplyr::select(dplyr::all_of(cov_factors))
  } else {
    data[, integer(), drop = FALSE]
  }
  continuous <- if (length(cov_continuous) > 0) {
    data %>% dplyr::select(dplyr::all_of(cov_continuous))
  } else {
    data[, integer(), drop = FALSE]
  }

  if (use_matrix) {
    dat_encoded <- prepare_xgb_frame(pop_parameters, factors, continuous)
  } else {
    dat_encoded <- prepare_tree_frame(pop_parameters, factors, continuous)
  }

  full_covariate <- setdiff(names(dat_encoded), pop_param)
  if (length(full_covariate) == 0) {
    stop("No covariate columns remain after data preparation.", call. = FALSE)
  }

  if (use_matrix) {
    x_pred <- as.data.frame(
      data.matrix(dat_encoded[, full_covariate, drop = FALSE])
    )
  } else {
    x_pred <- dat_encoded[, full_covariate, drop = FALSE]
  }

  imp_spec <- boruta_importance_spec(boruta_algorithm)

  result_folds <- as.data.frame(
    matrix(NA_character_, nrow = length(pop_param), ncol = n_folds),
    stringsAsFactors = FALSE
  )
  names(result_folds) <- paste0("fold", seq_len(n_folds))
  rownames(result_folds) <- pop_param

  pb <- progress::progress_bar$new(
    format = "[:bar] :percent :elapsed elapsed / :eta remaining",
    total = length(pop_param) * (n_folds + 1L),
    clear = FALSE,
    show_after = 0
  )

  for (i in pop_param) {
    set.seed(seed)
    pb$message(paste0("Searching covariate effects on ", i))
    pb$tick()

    y_all <- transform_ebes(dat_encoded[[i]], log_ebes, i)
    folds <- caret::createFolds(
      seq_len(nrow(x_pred)),
      k = n_folds,
      list = TRUE,
      returnTrain = FALSE
    )

    for (j in seq_len(n_folds)) {
      pb$tick()
      test_ind <- folds[[j]]
      training <- x_pred[-test_ind, , drop = FALSE]
      y_train <- y_all[-test_ind]

      train_sel <- apply_lasso_filter(
        training = training,
        y = y_train,
        use_lasso = use_lasso,
        lambda_lasso = lambda_lasso,
        n_folds = n_folds,
        cov_factors = cov_factors,
        keep_dummies = use_matrix
      )
      if (is.null(train_sel) || ncol(train_sel) == 0) {
        next
      }

      if (!use_matrix) {
        train_sel <- retype_tree_predictors(
          train_sel,
          cov_continuous,
          cov_factors
        )
      }

      feature_imp <- run_boruta(
        x = train_sel,
        y = y_train,
        pValue = boruta_pvalue,
        maxRuns = boruta_max_runs,
        get_imp = imp_spec$get_imp,
        extra = imp_spec$extra
      )
      result_folds[i, j] <- paste(feature_imp, collapse = ", ")
    }
  }

  result_ML <- vote_covariates(result_folds, pop_param, vote_threshold)

  structure(
    list(
      result_ML = result_ML,
      result_5folds = result_folds,
      result_folds = result_folds,
      pop_param = pop_param,
      cov_continuous = cov_continuous,
      cov_factors = cov_factors,
      settings = list(
        use_lasso = use_lasso,
        lambda_lasso = lambda_lasso,
        boruta_algorithm = boruta_algorithm,
        boruta_pvalue = boruta_pvalue,
        n_folds = n_folds,
        vote_threshold = vote_threshold,
        log_ebes = log_ebes,
        boruta_max_runs = boruta_max_runs,
        seed = seed
      )
    ),
    class = "mlcov_data"
  )
}

#' Validate scalar arguments for [ml_cov_search()]
#'
#' @keywords internal
#' @noRd
validate_ml_cov_search_args <- function(seed,
                                        use_lasso,
                                        boruta_pvalue,
                                        n_folds,
                                        vote_threshold,
                                        log_ebes,
                                        boruta_max_runs,
                                        boruta_algorithm) {
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
    stop("`seed` must be a single finite numeric value.", call. = FALSE)
  }
  if (!is.logical(use_lasso) || length(use_lasso) != 1L || is.na(use_lasso)) {
    stop("`use_lasso` must be a single logical value.", call. = FALSE)
  }
  if (!is.logical(log_ebes) || length(log_ebes) != 1L || is.na(log_ebes)) {
    stop("`log_ebes` must be a single logical value.", call. = FALSE)
  }
  if (!is.numeric(boruta_pvalue) || length(boruta_pvalue) != 1L ||
      !is.finite(boruta_pvalue) || boruta_pvalue <= 0 || boruta_pvalue >= 1) {
    stop("`boruta_pvalue` must be a single number in (0, 1).", call. = FALSE)
  }
  if (!is.numeric(n_folds) || length(n_folds) != 1L ||
      is.na(n_folds) || n_folds < 2) {
    stop("`n_folds` must be an integer >= 2.", call. = FALSE)
  }
  if (!is.numeric(vote_threshold) || length(vote_threshold) != 1L ||
      is.na(vote_threshold) || vote_threshold < 1) {
    stop("`vote_threshold` must be an integer >= 1.", call. = FALSE)
  }
  if (as.integer(vote_threshold) > as.integer(n_folds)) {
    stop("`vote_threshold` cannot exceed `n_folds`.", call. = FALSE)
  }
  if (!is.numeric(boruta_max_runs) || length(boruta_max_runs) != 1L ||
      is.na(boruta_max_runs) || boruta_max_runs < 2) {
    stop("`boruta_max_runs` must be an integer >= 2.", call. = FALSE)
  }

  if (identical(boruta_algorithm, "lightgbm") &&
      !requireNamespace("lightgbm", quietly = TRUE)) {
    stop(
      "Package 'lightgbm' is required for boruta_algorithm = \"lightgbm\".",
      call. = FALSE
    )
  }
  if (identical(boruta_algorithm, "catboost") &&
      !requireNamespace("catboost", quietly = TRUE)) {
    stop(
      "The 'catboost' package is required for boruta_algorithm = \"catboost\". ",
      "It is not on CRAN; see https://catboost.ai/en/docs/installation/r-installation.",
      call. = FALSE
    )
  }
  if (identical(boruta_algorithm, "xgboost") &&
      !requireNamespace("xgboost", quietly = TRUE)) {
    stop(
      "Package 'xgboost' is required for boruta_algorithm = \"xgboost\".",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
