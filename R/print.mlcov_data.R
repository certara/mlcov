#' Print \code{mlcov_data}
#'
#' Print generic used to return information about \code{mlcov_data} object
#'
#' @param x An \code{mlcov_data}.
#' @param ... Additional args.
#' @return Returns \code{x} invisibly.
#' @export
print.mlcov_data <- function(x, ...) {
  stopifnot(inherits(x, "mlcov_data"))
  pop_params <- row.names(x$result_ML)
  for (param in pop_params) {
    cov_selected <- x$result_ML[param, "cov_selected"]
    if (!is.na(cov_selected)) {
      cat(sprintf("\nPopulation Parameter:\t%s", param), "\n")
      cat("--------------------------\n")
      cat(sprintf("Covariates Selected:\t%s", paste0(cov_selected, collapse = ", ")), "\n")
    }
  }
  # output error type
  invisible(x)
}
