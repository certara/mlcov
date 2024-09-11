`%>%` <- dplyr::`%>%`

#' Generate Residual Plots to Assess Covariate-Parameter Relationships
#'
#' The `generate_residuals_plot` function generates residual plots to evaluate the relationships between covariates and population parameters after the covariate selection process. This function is the final step in the `mlcov` methodology, which follows covariate selection using machine learning techniques (Lasso and Boruta) implemented in the \code{\link{ml_cov_search}} function.
#'
#' The function performs the following:
#' 
#' 1. **Residual Analysis:** After selecting covariates using the Lasso and Boruta algorithms, an XGBoost model is trained on the selected covariates. The function then examines the residuals—defined as the difference between the actual target values (population parameters) and the predicted values from the XGBoost model.
#' 
#' 2. **P-value Evaluation:** For each covariate not selected in the \code{\link{ml_cov_search}} function, the function evaluates the statistical significance of its relationship with the residuals. This is done by calculating p-values across 10 different data partitions.
#' 
#' 3. **Threshold for Plot Generation:** A residual plot is generated for a covariate only if at least 6 out of 10 p-values are less than 0.05, ensuring that the relationship is robust across different data partitions.
#'
#' The primary objective of this function is to verify that the machine learning-based covariate selection process did not overlook any significant covariate-parameter relationships. By visualizing residuals against covariates, the function helps identify any remaining trends or relationships that might suggest the need for additional covariates.
#'
#' @param data A data frame containing the input variables, including the population parameters and covariates.
#' @param result An object of class \code{mlcov_data}, obtained from the \code{\link{ml_cov_search}} function. This object contains the results of the covariate selection process.
#' @param pop_param The name of the population parameter for which the residual plots are to be generated.
#' @param seed A numeric value for setting the random seed using \code{set.seed()} within the function, ensuring reproducibility. Defaults to 123.
#'
#' @return A list of \code{ggplot} objects, each representing a residual plot for a different covariate. If no covariates meet the significance threshold, the function returns an empty list.
#'
#' @examples
#' # Example usage:
#' \dontrun{
#' plots <- generate_residuals_plot(data, 
#'                                  result, 
#'                                  pop_param = "CL")
#' }
#' 
#' @import ggpmisc
#' @import ggstatsplot
#' 
#' @export

generate_residuals_plot <- function(data, result, pop_param, seed = 123) {
  
  stopifnot(inherits(result, "mlcov_data"))
  
  if (!is.null(seed)) {
    stopifnot(is.numeric(seed))
    set.seed(seed)
  }
  
  pop_params <- result$pop_param
  cov_continuous <- result$cov_continuous
  cov_factors <- result$cov_factors
  result_ML <- result$result_ML
  result_5folds <- result$result_5folds
  
  # Check that covariates supplied by user exist in the data
  data_validation(data, pop_param, cov_continuous, cov_factors)

  # Select columns and generate data for XGBoost
  dat <- col_select(data, pop_params, cov_continuous, cov_factors)
  pop_params <- dat %>% dplyr::select(dplyr::all_of(pop_params))
  factors <- dat %>% dplyr::select(dplyr::all_of(cov_factors))
  continuous <- dat %>% dplyr::select(dplyr::all_of(cov_continuous))
  
  # One-hot encoding of categorical covariates for covariates with more than 2 levels
  dat_XGB <- generate_dat_XGB(pop_params, factors, continuous)
 
  full_covariate_xgm <- names(dat_XGB)
  full_covariate_xgm <- setdiff(full_covariate_xgm, pop_params)
  
  full_covariate <- c(cov_continuous, cov_factors)
  
  res <- t(result_5folds[,1:5])
  res[res == ""] <- NA
  
  result_ML[result_ML == ""] <- NA
  
  # Assign the independent and dependent covariates
  x_xgb <- data.matrix(dat_XGB[, c(full_covariate_xgm)])
  y_xgb <- log(dat_XGB[, pop_param])
  plots_list <- list()  # Initialize the list to store plots
  
  dat <- dat %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(cov_factors), as.numeric))
  
  # First case: covariates are selected after the vote
  if (is.na(result_ML[pop_param, 1]) == FALSE)  {
    list_cov <- strsplit(gsub(" ", "", result_ML[pop_param, 1]), ",")
    x.selected_final <- as.matrix(dat_XGB %>% dplyr::select(dplyr::all_of(list_cov[[1]])))
    
    train.ind <- caret::createDataPartition(seq(1, nrow(x.selected_final)), times = 1, p = 0.8, list = FALSE)
    training <- as.matrix(x.selected_final[train.ind, ])
    colnames(training) <- colnames(x.selected_final)
    testing <-  as.matrix(x.selected_final[-train.ind, ])
    colnames(testing) <- colnames(x.selected_final)
    
    
    y <- as.data.frame(y_xgb)
    y.xgm_train <- y[train.ind, ]
    y.xgm_test <- y[-train.ind, ]
    
    if (length(list_cov[[1]]) != 0 ) {
      xgb.mod <- generate_xgb.mod(data = training, label = y.xgm_train)
      
      # predict on the test set with the new model
      y.xgb.pred <- predict(xgb.mod, newdata = testing)
      
      residuals <- y.xgb.pred - y.xgm_test
      
      pb <- progress::progress_bar$new(
        format = "[:bar] :percent :elapsed elapsed / :eta remaining",
        total = length(full_covariate) - length(list_cov[[1]]), # total covariates minus those identified from ml_cov_search
        clear = FALSE,
        show_after = 0
      )
      
        
      for (k in full_covariate) {
        p_value_count <- c()
        plots_listK <- list() 
        if (!(any(grepl(k, list_cov[[1]])))) {
          pb$message(paste0("Testing pvalue significance for ", k))

          if (k %in% cov_continuous) {
            data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
            plot <- ggplot2::ggplot(data_plot, aes(x = `cov`, y = `Residuals`))+
              ggplot2::geom_point(alpha = 0.3)+
              ggplot2::geom_smooth(method = "lm")+
              ggpmisc::stat_poly_line() +
              ggpmisc::stat_correlation(mapping = ggpmisc::use_label(c("R", "t", "P", "n"))) +
              ggplot2::labs(x = k, y = paste("Residuals", pop_param)) +
              ggplot2::theme_bw()
            
            cor_test_result <- cor.test(data_plot$Residuals, data_plot$cov)
            p_value <- cor_test_result$p.value
            
          } else {
            data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
            plot <-
              suppressMessages(
                ggstatsplot::ggbetweenstats(
                  data = data_plot,
                  x    = `cov`,
                  y    = `Residuals` ,
                  type = "nonparametric",
                  # nonparametric   parametric    robust   bayes
                  xlab = k,
                  ylab = paste("Residuals", pop_param)
                ) +
                  ggplot2::theme_bw() +
                  ggplot2::theme(legend.position = "none")
              )
            
            p_value <- plot$plot_env$subtitle_df$p.value
          }
          
          
          p_value_count<-c(p_value_count,p_value)
          plots_listK[[1]] <- plot
          
          attempts <- 2
          max_attempts <- 10
          
          # Try different train indices until we have 10 different pvalues
          while (attempts <= max_attempts) {
            train.ind <- caret::createDataPartition(seq(1, nrow(x.selected_final)), times = 1, p = 0.8, list = FALSE)
            training <- as.matrix(x.selected_final[train.ind, ])
            colnames(training) <- colnames(x.selected_final)
            testing <-  as.matrix(x.selected_final[-train.ind, ])
            colnames(testing) <- colnames(x.selected_final)
            
            
            y.xgm_train <- y[train.ind, ]
            y.xgm_test <- y[-train.ind, ]
            
            xgb.mod <- xgboost::xgboost(
              data = training,
              label = y.xgm_train,
              nrounds = 200,
              objective = "reg:squarederror",
              verbose = 0
            )
            
            y.xgb.pred <- predict(xgb.mod, newdata = testing)
            residuals <-  y.xgb.pred - y.xgm_test
            
            if (k %in% cov_continuous) {
              data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
              plot2 <- ggplot2::ggplot(data_plot, aes(x = `cov`, y = `Residuals`)) +
                ggplot2::geom_point(alpha = 0.3)+
                ggplot2::geom_smooth(method = "lm")+
                ggpmisc::stat_poly_line() +
                ggpmisc::stat_correlation(mapping = ggpmisc::use_label(c("R", "t", "P", "n"))) +
                ggplot2::labs(x = k, y = paste("Residuals", pop_param)) +
                ggplot2::theme_bw()
              
              cor_test_result <- cor.test(data_plot$Residuals, data_plot$cov)
              p_value2 <- cor_test_result$p.value
              
            } else {
              data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))

              plot2 <- suppressMessages(
                ggstatsplot::ggbetweenstats(data = data_plot,
                                                  x     = `cov`,
                                                  y     = `Residuals` ,
                                                  type  = "nonparametric",     # nonparametric   parametric    robust   bayes
                                                  xlab  = k,
                                                  ylab  = paste("Residuals", pop_param)) + 
                ggplot2::theme_bw() + 
                ggplot2::theme(legend.position="none")
              )
              
              p_value2 <- plot2$plot_env$subtitle_df$p.value
            }

            
            p_value_count <- c(p_value_count,p_value2)
            plots_listK[[attempts]] <- plot2
            
            attempts <- attempts + 1
            
          }
          
          #if 6 of the 10 pvalues are significant, we draw the residuals plots
          if (sum( p_value_count[!is.na(p_value_count)] <= 0.05) >= 6){
            position_first_below_0.05 <- which.max(p_value_count <=   0.05)
            value_first_below_0.05 <- p_value_count[position_first_below_0.05]
            
            plot <- plots_listK[[position_first_below_0.05]]
  
            
            plots_list[[k]] <- plot
          } 
          pb$tick()
        }
      }
    }
  }
  
  # Second case: covariates are not selected after the vote
  if (is.na(result_ML[pop_param, 1]))  {
    list_cov <- strsplit(as.character(res[, pop_param]), ",")
    list_cov_nb <- trimws(unlist(list_cov))
    comptage <- as.data.frame(table(list_cov_nb))
    if (nrow(comptage) != 0) {
      
      x.selected_final <- as.matrix(dat_XGB %>% dplyr::select(dplyr::all_of(comptage$list_cov_nb)))
      
      
      train.ind <- caret::createDataPartition(seq(1, nrow(x.selected_final)), times = 1, p = 0.8, list = FALSE)
      training <- as.matrix(x.selected_final[train.ind, ])
      colnames(training) <- colnames(x.selected_final)
      testing <-  as.matrix(x.selected_final[-train.ind, ])
      colnames(testing) <- colnames(x.selected_final)
      
      
      y <- as.data.frame(y_xgb)
      y.xgm_train <- y[train.ind, ]
      y.xgm_test <- y[-train.ind, ]
      
      if (length(list_cov_nb) != 0 ) {
        xgb.mod <- generate_xgb.mod(data = training, label = y.xgm_train)
        
        # predict on the test set with the new model
        y.xgb.pred <- predict(xgb.mod, newdata = testing)
        
        residuals <- y.xgb.pred - y.xgm_test
        
        for (k in full_covariate) {
          p_value_count <- c()
          plots_listK <- list() 
          if (k %in% cov_continuous) {
            data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
            plot <- ggplot2::ggplot(data_plot, aes(x = `cov`, y = `Residuals`)) +
              ggplot2::geom_point(alpha = 0.3)+
              ggplot2::geom_smooth(method = "lm")+
              ggpmisc::stat_poly_line() +
              ggpmisc::stat_correlation(mapping = ggpmisc::use_label(c("R", "t", "P", "n"))) +
              ggplot2::labs(x = k, y = paste("Residuals", pop_param)) +
              ggplot2::theme_bw()
            
            cor_test_result <- cor.test(data_plot$Residuals, data_plot$cov)
            p_value <- cor_test_result$p.value
            
          } else {
            data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
            plot <- suppressMessages(
              ggstatsplot::ggbetweenstats(data = data_plot,
                                                x    = `cov`,
                                                y    = `Residuals`,
                                                type = "nonparametric",     # nonparametric   parametric    robust   bayes
                                                xlab = k,
                                                ylab = paste("Residuals", pop_param)) + 
              ggplot2::theme_bw() + 
              ggplot2::theme(legend.position="none")
            )
            
            p_value <- plot$plot_env$subtitle_df$p.value
          }
          
          
          p_value_count<-c(p_value_count,p_value)
          plots_listK[[1]] <- plot
          
          attempts <- 2
          max_attempts <- 10
          
          # Try different train indices until we have 10 different pvalues
          while (attempts <= max_attempts) {
            train.ind <- caret::createDataPartition(seq(1, nrow(x.selected_final)), times = 1, p = 0.8, list = FALSE)
            training <- as.matrix(x.selected_final[train.ind, ])
            colnames(training) <- colnames(x.selected_final)
            testing <-  as.matrix(x.selected_final[-train.ind, ])
            colnames(testing) <- colnames(x.selected_final)
            
            y.xgm_train <- y[train.ind, ]
            y.xgm_test <- y[-train.ind, ]
            
            xgb.mod <- xgboost::xgboost(
              data = training,
              label = y.xgm_train,
              nrounds = 200,
              objective = "reg:squarederror",
              verbose = 0
            )
            
            y.xgb.pred <- predict(xgb.mod, newdata = testing)
            residuals <- y.xgb.pred - y.xgm_test
            
            if (k %in% cov_continuous) {
              data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
              plot2 <- ggplot2::ggplot(data_plot, aes(x = `cov`, y = `Residuals`))+
                ggplot2::geom_point(alpha = 0.3)+
                ggplot2::geom_smooth(method = "lm")+
                ggpmisc::stat_poly_line() +
                ggpmisc::stat_correlation(mapping = ggpmisc::use_label(c("R", "t", "P", "n"))) +
                ggplot2::labs(x = k, y = paste("Residuals", pop_param)) +
                ggplot2::theme_bw()
              
              cor_test_result <- cor.test(data_plot$Residuals, data_plot$cov)
              p_value2 <- cor_test_result$p.value
              
            } else {
              data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
              plot2 <-
                suppressMessages(
                  ggstatsplot::ggbetweenstats(
                    data = data_plot,
                    x    = `cov`,
                    y    = `Residuals` ,
                    type = "nonparametric",
                    # nonparametric   parametric    robust   bayes
                    xlab = k,
                    ylab = paste("Residuals", pop_param)
                  ) +
                    ggplot2::theme_bw() +
                    ggplot2::theme(legend.position = "none")
                )
              
              p_value2 <- plot2$plot_env$subtitle_df$p.value
            }
            
            p_value_count <- c(p_value_count,p_value2)
            plots_listK[[attempts]] <- plot2
            
            attempts <- attempts + 1
          }
          
          #if 6 of the 10 pvalues are significant, we draw the residuals plots
          if (sum( p_value_count[!is.na(p_value_count)] <= 0.05) >= 6){
            position_first_below_0.05 <- which.max(p_value_count <=  0.05)
            value_first_below_0.05 <- p_value_count[position_first_below_0.05]
            
            plot <- plots_listK[[position_first_below_0.05]]
           
            
            plots_list[[k]] <- plot
          }
        }
      }
    }
    else {message(paste("No variables selected for the 5 folds, so no model to be trained for", pop_param))
      return()}
  }
  
  # Check if plots_list is empty
  if (length(plots_list) == 0) {
    list_cov <- strsplit(res[, pop_param], ",")
    list_cov_nb <- trimws(unlist(list_cov))
    comptage <- as.data.frame(table(list_cov_nb))
    if (nrow(comptage) != 0){
      message(paste("No residuals plots with a significant p-value for", pop_param))
      return()
    }
  }
  
  # Return the list of subplots for the given parameter pop_param
  return(plots_list)
}
