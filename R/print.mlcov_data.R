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
  
  rmse <- x$result_ML$RMSE[!is.na(x$result_ML$RMSE)]
  rmse_ref <- x$result_ML$RMSE_ref[!is.na(x$result_ML$RMSE_ref)]
  cov_selected <- x$result_ML$cov_selected[!is.na(x$result_ML$cov_selected)]
  
  cat(sprintf("Covariate Names:\t%s", paste0(cov_selected, collapse = ", ")), "\n")
  cat(sprintf("Reference Values:\t%s\n", paste0("RMSE = ", rmse)))
  cat(sprintf("\t\t\t%s\n", paste0("RMSE_ref = ", rmse_ref)))
  
  # output error type
  invisible(x)
}
