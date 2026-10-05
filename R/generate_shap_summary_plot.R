#' Generate SHAP summary plots for selected covariates
#'
#' Trains an XGBoost interpreter on the covariates voted by
#' [ml_cov_search()] and draws SHAP summary plots. SHAP is an XGBoost-based
#' explanation of the selected set and does not depend on the Boruta learner
#' used during search.
#'
#' Plots are built with [xgboost::xgb.ggplot.shap.summary()] when two or more
#' covariates are selected, and with a single-feature SHAP scatter when only
#' one covariate is selected. This path is compatible with xgboost 3.x
#' (`xgb.train` boosters).
#'
#' @param result An `mlcov_data` object from [ml_cov_search()].
#' @param data The original data frame passed to [ml_cov_search()] (must
#'   contain `ID`, the population parameters, and the covariates).
#' @param x_bound Optional x-axis limits `c(min, max)`.
#' @param dilute Unused; retained for compatibility with earlier signatures.
#' @param scientific Unused; retained for compatibility with earlier signatures.
#' @param my_format Unused; retained for compatibility with earlier signatures.
#' @param min_color_bound Low end of the feature-value colour scale.
#' @param max_color_bound High end of the feature-value colour scale.
#' @param kind Retained for compatibility. Only the xgboost SHAP summary
#'   (or a single-feature scatter) is produced. `"bar"` is accepted but
#'   ignored with a warning.
#' @param title A character string used as the plot title. If `NULL`, the
#'   population parameter name is used.
#' @param title.position Horizontal title alignment in `[0, 1]`.
#' @param ylab Y-axis label.
#' @param xlab X-axis label.
#'
#' @return A named list of ggplot objects, one per parameter with at least
#'   one selected covariate. Parameters with no selection are omitted.
#'
#' @examples
#' \dontrun{
#' plots <- generate_shap_summary_plot(result, data)
#' }
#'
#' @importFrom ggplot2 .data
#' @export
generate_shap_summary_plot <- function(result,
                                       data,
                                       x_bound = NULL,
                                       dilute = FALSE,
                                       scientific = FALSE,
                                       my_format = NULL,
                                       min_color_bound = "#FFCC33",
                                       max_color_bound = "#6600CC",
                                       kind = c("sina", "bar"),
                                       title = NULL,
                                       title.position = 0,
                                       ylab = NULL,
                                       xlab = NULL) {
  stopifnot(inherits(result, "mlcov_data"))
  if (missing(data)) {
    stop(
      "`data` is required. Pass the same data.frame used in `ml_cov_search()`.",
      call. = FALSE
    )
  }
  kind <- match.arg(kind)
  if (identical(kind, "bar")) {
    warning(
      "`kind = \"bar\"` is no longer supported; SHAP plots use xgboost's ",
      "summary scatter. The argument is ignored.",
      call. = FALSE
    )
  }
  settings <- mlcov_settings(result)
  frames <- prepare_mlcov_frames(data, result)
  dat_xgb <- frames$dat_xgb
  xgb_names <- setdiff(names(dat_xgb), result$pop_param)

  shap_plots <- list()

  for (i in result$pop_param) {
    selected <- parse_cov_selected(result$result_ML[i, "cov_selected"])
    cols <- expand_to_xgb_columns(
      selected,
      xgb_names,
      result$cov_continuous,
      result$cov_factors
    )
    if (length(cols) == 0) {
      next
    }

    x_selected <- as.matrix(dat_xgb[, cols, drop = FALSE])
    storage.mode(x_selected) <- "double"
    colnames(x_selected) <- cols
    y <- transform_ebes(dat_xgb[[i]], settings$log_ebes, i)
    xgb_mod <- generate_xgb.mod(data = x_selected, label = y)

    p <- shap_summary_ggplot(
      model = xgb_mod,
      x = x_selected,
      min_color_bound = min_color_bound,
      max_color_bound = max_color_bound
    )
    if (!is.null(x_bound)) {
      p <- p + ggplot2::coord_cartesian(xlim = x_bound)
    }
    p <- p +
      ggplot2::ggtitle(if (is.null(title)) i else title) +
      ggplot2::labs(
        y = if (is.null(ylab)) "Feature" else ylab,
        x = if (is.null(xlab)) "SHAP value (impact on model output)" else xlab
      ) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = title.position))
    shap_plots[[i]] <- p
  }

  shap_plots
}

#' SHAP summary ggplot from an xgb.Booster
#'
#' @keywords internal
#' @noRd
shap_summary_ggplot <- function(model, x, min_color_bound, max_color_bound) {
  if (ncol(x) >= 2L) {
    p <- xgboost::xgb.ggplot.shap.summary(data = x, model = model)
    p <- p + ggplot2::scale_color_gradient(
      low = min_color_bound,
      high = max_color_bound
    )
    return(p)
  }

  shap <- stats::predict(model, x, predcontrib = TRUE)
  feat <- colnames(x)[1]
  shap_col <- if (feat %in% colnames(shap)) feat else 1L
  plot_df <- data.frame(
    shap = as.numeric(shap[, shap_col]),
    value = as.numeric(x[, 1]),
    feature = feat
  )
  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data[["shap"]],
      y = .data[["feature"]],
      color = .data[["value"]]
    )
  ) +
    ggplot2::geom_jitter(height = 0.15, width = 0, alpha = 0.7) +
    ggplot2::scale_color_gradient(low = min_color_bound, high = max_color_bound) +
    ggplot2::theme_bw()
}
