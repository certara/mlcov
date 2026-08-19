`%>%` <- dplyr::`%>%`

#' Select analysis columns and keep one row per subject
#'
#' Requires an `ID` column. Categorical covariates are converted to factors.
#'
#' @param data Input data frame.
#' @param pop_param Character vector of EBE / population-parameter columns.
#' @param cov_continuous Character vector of continuous covariate names, or `NULL`.
#' @param cov_factors Character vector of categorical covariate names, or `NULL`.
#' @return A data frame with `ID`, parameters, and covariates.
#' @keywords internal
#' @noRd
col_select <- function(data, pop_param, cov_continuous, cov_factors) {
  if (!"ID" %in% colnames(data)) {
    stop(
      "`data` must contain an `ID` column (used to keep one row per subject).",
      call. = FALSE
    )
  }

  cols <- c("ID", pop_param, cov_continuous, cov_factors)
  cols <- unique(cols[!is.null(cols) & !is.na(cols) & nzchar(cols)])

  dat <- data %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    unique()

  n_conflict <- sum(duplicated(dat$ID))
  if (n_conflict > 0) {
    n_id <- length(unique(dat$ID[duplicated(dat$ID)]))
    stop(
      "Analysis columns are not unique within ID for ", n_id,
      " subject(s). Supply one row per subject; EBEs and covariates must be ",
      "constant within ID so subjects are not split across folds.",
      call. = FALSE
    )
  }

  if (length(cov_factors) > 0) {
    dat <- dat %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(cov_factors), as.factor))
  }

  dat
}

#' Check that requested columns exist in `data`
#'
#' @param data Input data frame.
#' @param pop_param Character vector of parameter names.
#' @param cov_continuous Continuous covariate names, or `NULL`.
#' @param cov_factors Categorical covariate names, or `NULL`.
#' @return Invisibly `TRUE` if validation succeeds.
#' @keywords internal
#' @noRd
data_validation <- function(data, pop_param, cov_continuous, cov_factors) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  vectors <- list(
    pop_param = pop_param,
    cov_continuous = cov_continuous,
    cov_factors = cov_factors
  )
  errors <- character()

  if (!"ID" %in% colnames(data)) {
    errors <- c(errors, "`data` must contain an `ID` column.")
  }

  for (nm in names(vectors)) {
    vals <- vectors[[nm]]
    if (is.null(vals) || length(vals) == 0) {
      next
    }
    missing_values <- setdiff(as.character(vals), colnames(data))
    if (length(missing_values) > 0) {
      errors <- c(
        errors,
        paste(
          "The following values from", nm,
          "are missing in the dataset:", toString(missing_values)
        )
      )
    }
  }

  if (length(errors) > 0) {
    stop(paste(errors, collapse = "\n"), call. = FALSE)
  }

  invisible(TRUE)
}

#' One-hot / numeric encoding of categorical covariates for XGBoost
#'
#' Multi-level factors are dummy-encoded with the first level dropped. Binary
#' factors are stored as 0/1 using factor level order.
#'
#' @param pop_param Data frame of parameter columns.
#' @param factors Data frame of categorical covariates (or zero-column).
#' @param continuous Data frame of continuous covariates (or zero-column).
#' @return Combined data frame of parameters and numeric predictors.
#' @keywords internal
#' @noRd
prepare_xgb_frame <- function(pop_param, factors, continuous) {
  n <- nrow(pop_param)
  if (is.null(n)) {
    n <- nrow(factors)
  }
  if (is.null(n)) {
    n <- nrow(continuous)
  }

  modified_columns <- data.frame(matrix(ncol = 0, nrow = n))

  if (!is.null(factors) && ncol(factors) > 0) {
    for (col in names(factors)) {
      fac <- factors[[col]]
      if (!is.factor(fac)) {
        fac <- as.factor(fac)
      }
      if (nlevels(fac) > 2) {
        encoded <- factor_dummies(fac, col)
        modified_columns <- cbind(modified_columns, encoded)
      } else {
        modified_columns[[col]] <- as.integer(fac) - 1L
      }
    }
  }

  cbind(pop_param, modified_columns, continuous)
}

# Historical name used in earlier mlcov versions
generate_dat_XGB <- prepare_xgb_frame

#' Keep categorical covariates as factors for tree-based learners
#'
#' @inheritParams prepare_xgb_frame
#' @return Combined data frame of parameters, factors, and continuous covariates.
#' @keywords internal
#' @noRd
prepare_tree_frame <- function(pop_param, factors, continuous) {
  modified_factors <- factors
  if (!is.null(modified_factors) && ncol(modified_factors) > 0) {
    for (col in names(modified_factors)) {
      modified_factors[[col]] <- as.factor(modified_factors[[col]])
    }
  }
  cbind(pop_param, modified_factors, continuous)
}

#' Dummy-encode a predictor frame for glmnet
#'
#' Continuous columns are left numeric. Binary factors become a 0/1 column with
#' the original name. Multi-level factors are dummy-encoded (first level dropped).
#'
#' @param df Predictor data frame.
#' @param cov_factors Names of categorical covariates that may appear in `df`.
#' @return A list with `X` (numeric matrix) and `map` (named character vector
#'   mapping encoded column names to original covariate names).
#' @keywords internal
#' @noRd
dummy_encode_predictors <- function(df, cov_factors) {
  cov_factors <- intersect(cov_factors, names(df))
  map <- character()
  parts <- list()

  other_cols <- setdiff(names(df), cov_factors)
  if (length(other_cols) > 0) {
    num_part <- as.data.frame(
      lapply(df[other_cols], function(col) as.numeric(col)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(num_part) <- other_cols
    parts <- c(parts, list(num_part))
    map <- c(map, stats::setNames(other_cols, other_cols))
  }

  for (col in cov_factors) {
    fac <- df[[col]]
    if (!is.factor(fac)) {
      fac <- as.factor(fac)
    }
    if (nlevels(fac) <= 2) {
      encoded <- data.frame(as.integer(fac) - 1L, check.names = FALSE)
      names(encoded) <- col
      parts <- c(parts, list(encoded))
      map <- c(map, stats::setNames(col, col))
    } else {
      encoded <- factor_dummies(fac, col)
      parts <- c(parts, list(encoded))
      map <- c(map, stats::setNames(rep(col, ncol(encoded)), names(encoded)))
    }
  }

  if (length(parts) == 0) {
    return(list(X = matrix(numeric(), nrow = nrow(df), ncol = 0), map = map))
  }

  encoded_df <- do.call(cbind, parts)
  list(X = as.matrix(encoded_df), map = map)
}

#' Non-zero Lasso coefficients at the chosen lambda
#'
#' @param X Numeric predictor matrix.
#' @param y Response vector.
#' @param lambda_lasso `"lambda.min"` or `"lambda.1se"`.
#' @param n_folds Number of inner glmnet CV folds (at least 3 when possible).
#' @return Character vector of selected column names (possibly empty).
#' @keywords internal
#' @noRd
lasso_nonzero_names <- function(X, y, lambda_lasso, n_folds) {
  if (ncol(X) == 0 || nrow(X) < 4) {
    return(character())
  }

  inner_folds <- as.integer(n_folds)
  inner_folds <- max(3L, inner_folds)
  inner_folds <- min(inner_folds, nrow(X))
  if (inner_folds < 3L) {
    return(character())
  }

  cvfit <- glmnet::cv.glmnet(
    x = X,
    y = as.numeric(y),
    alpha = 1,
    family = "gaussian",
    nfolds = inner_folds
  )
  coefs <- stats::coef(cvfit, s = cvfit[[lambda_lasso]])
  coefs <- coefs[-1, , drop = TRUE]
  names(coefs)[coefs != 0 & !is.na(coefs)]
}

#' Apply optional Lasso pre-screening to a training fold
#'
#' For XGBoost (already dummy-encoded) selected dummy names are kept. For
#' tree-based learners, dummy columns are mapped back to original covariates.
#'
#' @param training Training predictor data frame.
#' @param y Training response.
#' @param use_lasso Logical.
#' @param lambda_lasso `"lambda.min"` or `"lambda.1se"`.
#' @param n_folds Inner CV folds for glmnet.
#' @param cov_factors Original categorical covariate names.
#' @param keep_dummies If `TRUE`, return dummy column names (XGBoost path).
#' @return A data frame of selected predictors, or `NULL` if Lasso keeps nothing.
#' @keywords internal
#' @noRd
apply_lasso_filter <- function(training,
                               y,
                               use_lasso,
                               lambda_lasso,
                               n_folds,
                               cov_factors,
                               keep_dummies) {
  if (!isTRUE(use_lasso)) {
    return(as.data.frame(training, stringsAsFactors = FALSE))
  }

  if (isTRUE(keep_dummies)) {
    selected <- lasso_nonzero_names(
      X = as.matrix(training),
      y = y,
      lambda_lasso = lambda_lasso,
      n_folds = n_folds
    )
    if (length(selected) == 0) {
      return(NULL)
    }
    out <- as.data.frame(training[, selected, drop = FALSE], stringsAsFactors = FALSE)
    names(out) <- selected
    return(out)
  }

  encoded <- dummy_encode_predictors(training, cov_factors)
  selected_dummies <- lasso_nonzero_names(
    X = encoded$X,
    y = y,
    lambda_lasso = lambda_lasso,
    n_folds = n_folds
  )
  if (length(selected_dummies) == 0) {
    return(NULL)
  }

  orig <- unique(unname(encoded$map[selected_dummies]))
  orig <- orig[!is.na(orig) & nzchar(orig)]
  orig <- intersect(orig, names(training))
  if (length(orig) == 0) {
    return(NULL)
  }

  as.data.frame(training[, orig, drop = FALSE], stringsAsFactors = FALSE)
}

#' Restore numeric / factor types after Lasso subsetting
#'
#' @param df Training data frame of selected covariates.
#' @param cov_continuous Original continuous covariate names.
#' @param cov_factors Original categorical covariate names.
#' @return `df` with columns re-typed.
#' @keywords internal
#' @noRd
retype_tree_predictors <- function(df, cov_continuous, cov_factors) {
  cont_cols <- intersect(names(df), cov_continuous)
  fact_cols <- intersect(names(df), cov_factors)
  if (length(cont_cols) > 0) {
    df[cont_cols] <- lapply(df[cont_cols], as.numeric)
  }
  if (length(fact_cols) > 0) {
    df[fact_cols] <- lapply(df[fact_cols], as.factor)
  }
  df
}

#' Parse a comma-separated covariate string
#'
#' @param x A single character value, possibly `NA`.
#' @return Character vector of covariate names (empty if none).
#' @keywords internal
#' @noRd
parse_cov_selected <- function(x) {
  if (length(x) == 0 || is.null(x) || is.na(x) || !nzchar(trimws(as.character(x)))) {
    return(character())
  }
  parsed <- trimws(unlist(strsplit(as.character(x), ",", fixed = TRUE)))
  parsed[nzchar(parsed) & parsed != "NA"]
}

#' Majority-style vote across folds
#'
#' Empty / `NA` fold cells are ignored so they cannot appear as a fake
#' covariate named `"NA"`.
#'
#' @param result_folds Data frame of fold selections (rows = parameters).
#' @param pop_param Parameter names (row names).
#' @param vote_threshold Minimum number of folds for retention.
#' @return Data frame with column `cov_selected` (`NA` if nothing retained).
#' @keywords internal
#' @noRd
vote_covariates <- function(result_folds, pop_param, vote_threshold) {
  result_ML <- data.frame(
    cov_selected = rep(NA_character_, length(pop_param)),
    stringsAsFactors = FALSE
  )
  rownames(result_ML) <- pop_param

  n_folds <- ncol(result_folds)
  fold_mat <- as.matrix(result_folds[, seq_len(n_folds), drop = FALSE])
  fold_mat[fold_mat == ""] <- NA_character_

  for (i in pop_param) {
    cells <- fold_mat[i, ]
    pieces <- lapply(cells, parse_cov_selected)
    list_cov_nb <- unlist(pieces, use.names = FALSE)
    if (length(list_cov_nb) == 0) {
      next
    }
    comptage <- as.data.frame(table(list_cov_nb), stringsAsFactors = FALSE)
    names(comptage) <- c("list_cov_nb", "Freq")
    kept <- comptage$Freq >= vote_threshold
    if (!any(kept)) {
      next
    }
    result_ML[i, 1] <- paste(as.character(comptage$list_cov_nb[kept]), collapse = ", ")
  }

  result_ML
}

#' Transform EBEs, optionally with a log
#'
#' @param y Numeric vector of EBEs.
#' @param log_ebes Logical; if `TRUE`, `y` must be strictly positive.
#' @param param_name Parameter name used in error messages.
#' @return Transformed numeric vector.
#' @keywords internal
#' @noRd
transform_ebes <- function(y, log_ebes, param_name) {
  y <- as.numeric(y)
  if (isTRUE(log_ebes)) {
    bad <- !is.finite(y) | y <= 0
    if (any(bad)) {
      stop(
        "log_ebes = TRUE requires strictly positive finite EBEs for parameter `",
        param_name, "`. Found ", sum(bad), " invalid value(s).",
        call. = FALSE
      )
    }
    return(log(y))
  }
  if (any(!is.finite(y))) {
    stop(
      "Non-finite EBEs found for parameter `", param_name, "`.",
      call. = FALSE
    )
  }
  y
}

#' Settings stored on an `mlcov_data` object, with defaults for older objects
#'
#' @param result An `mlcov_data` object.
#' @return Named list of search settings.
#' @keywords internal
#' @noRd
mlcov_settings <- function(result) {
  s <- result$settings
  if (is.null(s)) {
    s <- list()
  }
  if (is.null(s$log_ebes)) {
    s$log_ebes <- TRUE
  }
  if (is.null(s$n_folds)) {
    folds <- result$result_5folds
    s$n_folds <- if (!is.null(folds)) ncol(folds) else 5L
  }
  if (is.null(s$boruta_algorithm)) {
    s$boruta_algorithm <- "xgboost"
  }
  s
}

#' Map an encoded column name back to its original covariate
#'
#' Dummy names are `paste0(factor, level)`. When one factor name is a prefix
#' of another (`RACE` vs `RACE_GROUP`), the longest matching factor is used.
#'
#' @param col Encoded or original column name.
#' @param cov_continuous Original continuous names.
#' @param cov_factors Original categorical names.
#' @return Original covariate name, or `NA_character_` if unmatched.
#' @keywords internal
#' @noRd
original_covariate_for_column <- function(col, cov_continuous, cov_factors) {
  if (length(col) != 1L || is.na(col) || !nzchar(col)) {
    return(NA_character_)
  }
  if (col %in% cov_continuous || col %in% cov_factors) {
    return(col)
  }
  prefixes <- cov_factors[nzchar(cov_factors) & startsWith(col, cov_factors)]
  if (length(prefixes) == 0L) {
    return(NA_character_)
  }
  prefixes[which.max(nchar(prefixes))]
}

#' Original covariate names implied by a selection (dummies or originals)
#'
#' @keywords internal
#' @noRd
selected_original_names <- function(selected, cov_continuous, cov_factors) {
  if (length(selected) == 0) {
    return(character())
  }
  orig <- vapply(
    selected,
    original_covariate_for_column,
    character(1),
    cov_continuous = cov_continuous,
    cov_factors = cov_factors,
    USE.NAMES = FALSE
  )
  unique(orig[!is.na(orig)])
}

#' Map selected covariate names onto columns of an XGBoost-encoded frame
#'
#' Tree-based searches store original factor names; XGBoost searches store
#' dummy column names. SHAP / residual plots always train XGBoost, so original
#' factor names are expanded onto the dummy-encoded frame via exact original
#' identity (longest-prefix match), not `startsWith()` on dummy names.
#'
#' @param selected Character vector of selected names.
#' @param xgb_names Column names of the XGBoost-encoded predictor frame.
#' @param cov_continuous Original continuous names.
#' @param cov_factors Original categorical names.
#' @return Character vector of columns present in `xgb_names`.
#' @keywords internal
#' @noRd
expand_to_xgb_columns <- function(selected, xgb_names, cov_continuous, cov_factors) {
  if (length(selected) == 0) {
    return(character())
  }
  if (all(selected %in% xgb_names)) {
    return(selected)
  }

  orig_of_xgb <- vapply(
    xgb_names,
    original_covariate_for_column,
    character(1),
    cov_continuous = cov_continuous,
    cov_factors = cov_factors,
    USE.NAMES = FALSE
  )
  selected_orig <- selected_original_names(
    selected,
    cov_continuous,
    cov_factors
  )
  unique(xgb_names[orig_of_xgb %in% selected_orig | xgb_names %in% selected])
}

#' Prepare subject-level and XGBoost-encoded frames from a search result
#'
#' @param data Original input data frame.
#' @param result An `mlcov_data` object.
#' @return A list with `dat`, `dat_xgb`, `cov_continuous`, `cov_factors`,
#'   `pop_param`.
#' @keywords internal
#' @noRd
prepare_mlcov_frames <- function(data, result) {
  pop_param <- result$pop_param
  cov_continuous <- result$cov_continuous
  cov_factors <- result$cov_factors
  dat <- col_select(data, pop_param, cov_continuous, cov_factors)
  pop_df <- dat[, pop_param, drop = FALSE]
  factors <- if (length(cov_factors) > 0) {
    dat[, cov_factors, drop = FALSE]
  } else {
    dat[, integer(), drop = FALSE]
  }
  continuous <- if (length(cov_continuous) > 0) {
    dat[, cov_continuous, drop = FALSE]
  } else {
    dat[, integer(), drop = FALSE]
  }
  dat_xgb <- prepare_xgb_frame(pop_df, factors, continuous)
  list(
    dat = dat,
    dat_xgb = dat_xgb,
    pop_param = pop_param,
    cov_continuous = cov_continuous,
    cov_factors = cov_factors
  )
}

#' Fit the package XGBoost interpreter
#'
#' @param data Numeric matrix of predictors.
#' @param label Numeric response.
#' @return An `xgb.Booster` model.
#' @keywords internal
#' @noRd
generate_xgb.mod <- function(data, label) {
  fit_xgb_regressor(x = data, y = label, nrounds = 200)
}

#' Drop-first dummy columns for a multi-level factor
#'
#' Uses treatment contrasts so encoding does not depend on caret being attached
#' (`dummyVars` looks up `contr.ltfr` on the search path).
#'
#' @param fac A factor.
#' @param col Original covariate name used as the dummy prefix.
#' @return A data frame of dummy columns, or zero columns if fewer than 2 levels.
#' @keywords internal
#' @noRd
factor_dummies <- function(fac, col) {
  fac <- droplevels(as.factor(fac))
  n <- length(fac)
  if (nlevels(fac) <= 1L) {
    return(data.frame(matrix(ncol = 0, nrow = n)))
  }
  mm <- stats::model.matrix(~ fac, contrasts.arg = list(fac = stats::contr.treatment))
  encoded <- mm[, -1, drop = FALSE]
  colnames(encoded) <- paste0(col, levels(fac)[-1])
  as.data.frame(encoded, check.names = FALSE)
}

#' Fit a squared-error XGBoost model across xgboost 1.x and 3.x APIs
#'
#' @param x Predictor matrix or data frame.
#' @param y Numeric response.
#' @param nrounds Number of boosting rounds.
#' @param ... Extra arguments forwarded to [xgboost::xgboost()].
#' @return An `xgb.Booster`.
#' @keywords internal
#' @noRd
fit_xgb_regressor <- function(x, y, nrounds = 200, ...) {
  x_mat <- as.matrix(x)
  storage.mode(x_mat) <- "double"
  y <- as.numeric(y)
  dots <- list(...)
  dots$verbose <- NULL
  nthread <- dots$nthread
  if (is.null(nthread)) {
    nthread <- 1L
  }
  dots$nthread <- NULL

  params <- list(objective = "reg:squarederror", verbosity = 0)
  if (!is.null(dots$objective)) {
    params$objective <- dots$objective
    dots$objective <- NULL
  }
  if (!is.null(dots$verbosity)) {
    params$verbosity <- dots$verbosity
    dots$verbosity <- NULL
  }
  params <- c(params, dots)

  dtrain <- xgboost::xgb.DMatrix(data = x_mat, label = y, nthread = nthread)
  xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    verbose = 0
  )
}
