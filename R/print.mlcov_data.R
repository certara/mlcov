#' Print an `mlcov_data` object
#'
#' Shows the resolved search settings and the voted covariates for each
#' population parameter.
#'
#' @param x An `mlcov_data` object from [ml_cov_search()].
#' @param ... Additional arguments (ignored).
#' @return `x`, invisibly.
#' @export
print.mlcov_data <- function(x, ...) {
  stopifnot(inherits(x, "mlcov_data"))
  s <- mlcov_settings(x)

  cat("mlcov covariate search\n")
  cat(sprintf("  Algorithm:        %s\n", s$boruta_algorithm))
  if (isTRUE(s$use_lasso)) {
    cat(sprintf("  Lasso:            yes (%s)\n", s$lambda_lasso))
  } else {
    cat("  Lasso:            no\n")
  }
  cat(sprintf("  Folds / vote:     %s / %s\n", s$n_folds, s$vote_threshold))
  cat(sprintf("  log_ebes:         %s\n", s$log_ebes))
  if (!is.null(s$boruta_pvalue)) {
    cat(sprintf("  Boruta p-value:   %s\n", s$boruta_pvalue))
  }
  if (!is.null(s$boruta_max_runs)) {
    cat(sprintf("  Boruta maxRuns:   %s\n", s$boruta_max_runs))
  }

  pop_params <- rownames(x$result_ML)
  if (is.null(pop_params)) {
    pop_params <- x$pop_param
  }
  for (param in pop_params) {
    cov_selected <- x$result_ML[param, "cov_selected"]
    cat(sprintf("\nPopulation Parameter:\t%s\n", param))
    cat("--------------------------\n")
    if (length(parse_cov_selected(cov_selected)) > 0) {
      cat(sprintf("Covariates Selected:\t%s\n", cov_selected))
    } else {
      cat("Covariates Selected:\tnone\n")
    }
  }
  invisible(x)
}
