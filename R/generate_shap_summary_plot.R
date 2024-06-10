#' Generate SHAP Summary Plots
#'
#' This function generates SHAP summary plots for the XGBoost model.
#' @inheritParams SHAPforxgboost::shap.plot.summary
#' @param data A list containing required data frames and results.
#' @param title A character string to customize the title
#' @param title.position A numeric value from 0.0-1.0 to adjust the alignment of the title
#' @param ylab A character string to customize the y-axis
#' @param xlab A character string to customize the x-axis
#' @return A list of ggplot objects, each representing a SHAP summary plot for a different parameter.
#'
#' @examples
#' # Assuming 'data' is a list with necessary components
#' \dontrun{
#' plots <- generate_shap_summary_plot(data, title = "Custom Title", y.inter = 0.25, ...)
#' }
#'
#' @import xgboost
#' @import ggplot2
#' @import SHAPforxgboost
#'
#' @export
#' 
generate_shap_summary_plot <- function(data,
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
                                       xlab = NULL)
{
  # Initialize an empty list to store the SHAP summary plots
  shap_plots <- list()
  
  for (i in data$pop_param) {
    if (!is.null(data$shap_data[[i]])) {
      # Set to the seed used when making the data
      set.seed(data$shap_seed[[i]])
      
      shap_values <- data$shap_data[[i]]$shap_values
      shap_long <- data$shap_data[[i]]$shap_long
      
      # Generate summary plot
      p <-
        SHAPforxgboost::shap.plot.summary(
          shap_long,
          x_bound = x_bound,
          dilute = dilute,
          scientific = scientific,
          my_format = my_format,
          min_color_bound = min_color_bound,
          max_color_bound = max_color_bound,
          kind = kind
        )
      p <- p + 
        ggplot2::ggtitle(ifelse(is.null(title), i, title)) +
        ggplot2::labs(y = ifelse(is.null(ylab), "SHAP value (impact on model output)", ylab), 
                      x = ifelse(is.null(xlab), "", xlab)) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = title.position))
      shap_plots[[i]] <- p
    }
    
  }
  
  return(shap_plots)
}
