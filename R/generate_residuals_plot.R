`%>%` <- dplyr::`%>%`

#' Residual plots for covariates not selected by [ml_cov_search()]
#'
#' After covariate selection, an XGBoost model is trained on the selected
#' covariates. Residuals (predicted minus observed EBEs on a held-out split)
#' are tested against unselected covariates. A plot is returned for a
#' covariate only if at least 6 of 10 random 80/20 partitions produce a
#' p-value below 0.05.
#'
#' If no covariate survives voting, covariates that appeared in any fold are
#' used to train the residual model, and every covariate is screened.
#'
#' @param data The original data frame passed to [ml_cov_search()].
#' @param result An `mlcov_data` object from [ml_cov_search()].
#' @param pop_param Name of the population parameter to assess (length 1).
#' @param seed Numeric seed. Defaults to 123.
#'
#' @return A named list of ggplot objects, or `NULL` if nothing is plotted.
#'
#' @examples
#' \dontrun{
#' plots <- generate_residuals_plot(data, result, pop_param = "CL")
#' }
#'
#' @importFrom ggplot2 .data
#' @export
generate_residuals_plot <- function(data, result, pop_param, seed = 123) {
  stopifnot(inherits(result, "mlcov_data"))
  if (length(pop_param) != 1L) {
    stop("`pop_param` must be a single parameter name.", call. = FALSE)
  }
  if (!pop_param %in% result$pop_param) {
    stop(
      "`pop_param` must be one of the parameters used in `ml_cov_search()`.",
      call. = FALSE
    )
  }
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L) {
      stop("`seed` must be a single numeric value.", call. = FALSE)
    }
    set.seed(seed)
  }

  settings <- mlcov_settings(result)
  cov_continuous <- result$cov_continuous
  cov_factors <- result$cov_factors
  data_validation(data, pop_param, cov_continuous, cov_factors)

  frames <- prepare_mlcov_frames(data, result)
  dat <- frames$dat
  dat_xgb <- frames$dat_xgb
  xgb_names <- setdiff(names(dat_xgb), result$pop_param)
  full_covariate <- c(cov_continuous, cov_factors)
  y_xgb <- transform_ebes(dat_xgb[[pop_param]], settings$log_ebes, pop_param)

  selected_voted <- parse_cov_selected(result$result_ML[pop_param, "cov_selected"])
  skip_selected <- length(selected_voted) > 0
  model_covs <- selected_voted
  if (!skip_selected) {
    model_covs <- covariates_from_folds(
      result$result_5folds,
      pop_param,
      settings$n_folds
    )
    if (length(model_covs) == 0) {
      message(
        "No variables selected for any fold, so no model to be trained for ",
        pop_param
      )
      return(invisible(NULL))
    }
  }

  model_cols <- expand_to_xgb_columns(
    model_covs,
    xgb_names,
    cov_continuous,
    cov_factors
  )
  if (length(model_cols) == 0) {
    message(
      "Selected covariates for ", pop_param,
      " could not be mapped onto the encoded design matrix."
    )
    return(invisible(NULL))
  }

  x_selected <- as.matrix(dat_xgb[, model_cols, drop = FALSE])
  dat_plot <- dat
  if (length(cov_factors) > 0) {
    dat_plot <- dat_plot %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(cov_factors), as.numeric))
  }

  covs_to_test <- full_covariate
  if (skip_selected) {
    covs_to_test <- full_covariate[
      !vapply(
        full_covariate,
        function(k) any(grepl(k, selected_voted, fixed = TRUE)),
        logical(1)
      )
    ]
  }

  if (length(covs_to_test) == 0) {
    message("No unselected covariates remain to screen for ", pop_param)
    return(invisible(NULL))
  }

  pb <- progress::progress_bar$new(
    format = "[:bar] :percent :elapsed elapsed / :eta remaining",
    total = length(covs_to_test),
    clear = FALSE,
    show_after = 0
  )

  plots_list <- list()
  for (k in covs_to_test) {
    pb$message(paste0("Testing pvalue significance for ", k))
    plot_k <- residual_plot_if_significant(
      x_selected = x_selected,
      y = y_xgb,
      dat_plot = dat_plot,
      covariate = k,
      is_continuous = k %in% cov_continuous,
      pop_param = pop_param
    )
    if (!is.null(plot_k)) {
      plots_list[[k]] <- plot_k
    }
    pb$tick()
  }

  if (length(plots_list) == 0) {
    message("No residuals plots with a significant p-value for ", pop_param)
    return(invisible(NULL))
  }

  plots_list
}

#' Unique covariates named in any fold for a parameter
#'
#' @keywords internal
#' @noRd
covariates_from_folds <- function(result_folds, pop_param, n_folds) {
  n_folds <- min(as.integer(n_folds), ncol(result_folds))
  cells <- as.character(unlist(result_folds[pop_param, seq_len(n_folds), drop = FALSE]))
  unique(unlist(lapply(cells, parse_cov_selected), use.names = FALSE))
}

#' Screen one covariate against XGBoost residuals over 10 partitions
#'
#' @return A ggplot, or `NULL` if fewer than 6 p-values are <= 0.05.
#' @keywords internal
#' @noRd
residual_plot_if_significant <- function(x_selected,
                                         y,
                                         dat_plot,
                                         covariate,
                                         is_continuous,
                                         pop_param,
                                         n_attempts = 10L,
                                         p_cutoff = 0.05,
                                         n_sig_required = 6L) {
  p_values <- rep(NA_real_, n_attempts)
  plots <- vector("list", n_attempts)

  for (attempt in seq_len(n_attempts)) {
    split <- residual_train_test_split(x_selected, y, dat_plot)
    xgb_mod <- generate_xgb.mod(data = split$training, label = split$y_train)
    y_pred <- stats::predict(xgb_mod, newdata = split$testing)
    residuals <- y_pred - split$y_test
    data_plot <- data.frame(
      Residuals = residuals,
      cov = split$dat_test[[covariate]]
    )

    built <- build_residual_plot(
      data_plot = data_plot,
      covariate = covariate,
      is_continuous = is_continuous,
      pop_param = pop_param
    )
    p_values[attempt] <- built$p_value
    plots[[attempt]] <- built$plot
  }

  finite_p <- p_values[!is.na(p_values)]
  if (sum(finite_p <= p_cutoff) < n_sig_required) {
    return(NULL)
  }

  first_sig <- which(p_values <= p_cutoff)[1]
  plots[[first_sig]]
}

#' 80/20 split used by residual screening
#'
#' @keywords internal
#' @noRd
residual_train_test_split <- function(x_selected, y, dat_plot) {
  train_ind <- caret::createDataPartition(
    seq_len(nrow(x_selected)),
    times = 1,
    p = 0.8,
    list = FALSE
  )
  training <- as.matrix(x_selected[train_ind, , drop = FALSE])
  testing <- as.matrix(x_selected[-train_ind, , drop = FALSE])
  colnames(training) <- colnames(x_selected)
  colnames(testing) <- colnames(x_selected)
  list(
    training = training,
    testing = testing,
    y_train = y[train_ind],
    y_test = y[-train_ind],
    dat_test = dat_plot[-train_ind, , drop = FALSE]
  )
}

#' Build a continuous or categorical residual plot and its p-value
#'
#' @keywords internal
#' @noRd
build_residual_plot <- function(data_plot, covariate, is_continuous, pop_param) {
  ylab <- paste("Residuals", pop_param)
  if (isTRUE(is_continuous)) {
    ct <- stats::cor.test(data_plot$Residuals, data_plot$cov)
    p_value <- ct$p.value
    subtitle <- sprintf(
      "R = %.3f, t = %.2f, P = %.3g, n = %d",
      unname(ct$estimate),
      unname(ct$statistic),
      p_value,
      nrow(data_plot)
    )
    plot <- ggplot2::ggplot(
      data_plot,
      ggplot2::aes(x = .data[["cov"]], y = .data[["Residuals"]])
    ) +
      ggplot2::geom_point(alpha = 0.3) +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
      ggplot2::labs(x = covariate, y = ylab, subtitle = subtitle) +
      ggplot2::theme_bw()
  } else {
    plot <- suppressMessages(
      ggstatsplot::ggbetweenstats(
        data = data_plot,
        x = "cov",
        y = "Residuals",
        type = "nonparametric",
        xlab = covariate,
        ylab = ylab
      ) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none")
    )
    p_value <- plot$plot_env$subtitle_df$p.value
  }
  list(plot = plot, p_value = p_value)
}
