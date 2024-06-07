`%>%` <- dplyr::`%>%`

#' Generate Residual Plots for Model Analysis
#'
#' This function generates residual plots for model analysis based on various covariates and model results. It performs data preprocessing, model fitting using XGBoost, and generates plots to visualize the residuals against the covariates.
#' @inheritParams MLCovSearch
#' @param data Data frame containing the input variables.
#' @param result Results object of class "mlcov_data", obtained from the MLCovSearch function.
#' @param i An integer indicating the index of the parameter of interest.
#' @param seed Numeric value for usage of \code{set.seed()} inside function.
#'
#' @return A list of ggplot objects of length 1 or greater, each representing a residual plot for a different covariate. The function returns an empty list if no significant relationships are found. It also handles cases where covariates are not selected after the vote.
#'
#' @examples
#' # Assuming 'data' is a data frame with the necessary columns
#' \dontrun{
#' plots <- generate_residuals_plot(data, 
#' result, 
#' i = "V1",
#' seed = 123)
#' }
#' 
#' @import ggpmisc
#' @import ggstatsplot
#' 
#' @export
#' 
generate_residuals_plot <- function(data, result, i, seed = NULL) {
  
  stopifnot(inherits(result, "mlcov_data"))
  
  if (!is.null(seed)) {
    stopifnot(is.numeric(seed))
    set.seed(seed)
  }
  
  list_pop_param <- result$list_pop_param
  cov_continuous <- result$cov_continuous
  cov_factors <- result$cov_factors
  result_ML <- result$result_ML
  result_5folds <- result$result_5folds
  
  # Check that covariates supplied by user exist in the data
  data_validation(data, list_pop_param, cov_continuous, cov_factors)
  
  # Select columns and generate data for XGBoost
  dat <- col_select(data, list_pop_param, cov_continuous, cov_factors)
  pop_param <- dat %>% dplyr::select(dplyr::all_of(list_pop_param))
  factors <- dat %>% dplyr::select(dplyr::all_of(cov_factors))
  continuous <- dat %>% dplyr::select(dplyr::all_of(cov_continuous))
  
  # One-hot encoding of categorical covariates for covariates with more than 2 levels
  dat_XGB <- generate_dat_XGB(pop_param, factors, continuous)
 
  full_covariate_xgm <- names(dat_XGB)
  full_covariate_xgm <- setdiff(full_covariate_xgm, list_pop_param)
  
  full_covariate <- c(cov_continuous, cov_factors)
  
  res <- t(result_5folds[,1:5])
  res[res == ""] <- NA
  
  result_ML[result_ML == ""] <- NA
  
  # Assign the independent and dependent covariates
  x_xgb <- data.matrix(dat_XGB[, c(full_covariate_xgm)])
  y_xgb <- log(dat_XGB[, i])
  plots_list <- list()  # Initialize the list to store plots
  
  dat <- dat %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(cov_factors), as.numeric))
  
  # First case: covariates are selected after the vote
  if (is.na(result_ML[i, 1]) == FALSE)  {
    list_cov <- strsplit(gsub(" ", "", result_ML[i, 1]), ",")
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
      
      for (k in full_covariate) {
        p_value_count <- c()
        plots_listK <- list() 
        if (!(any(grepl(k, list_cov[[1]])))) {
          if (k %in% cov_continuous) {
            data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
            plot <- ggplot2::ggplot(data_plot, aes(x = `cov`, y = `Residuals`))+
              ggplot2::geom_point(alpha = 0.3)+
              ggplot2::geom_smooth(method = "lm")+
              ggpmisc::stat_poly_line() +
              ggpmisc::stat_correlation(mapping = ggpmisc::use_label(c("R", "t", "P", "n"))) +
              ggplot2::labs(x = k, y = paste("Residuals", i)) +
              ggplot2::theme_bw()
            
            cor_test_result <- cor.test(data_plot$Residuals, data_plot$cov)
            p_value <- cor_test_result$p.value
            
          } else {
            data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
            plot <- ggstatsplot::ggbetweenstats(data = data_plot,
                                                x    = `cov`,
                                                y    = `Residuals` ,
                                                type = "nonparametric",     # nonparametric   parametric    robust   bayes
                                                xlab = k,
                                                ylab = paste("Residuals", i)) + 
              ggplot2::theme_bw() + 
              ggplot2::theme(legend.position="none")
            
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
                ggplot2::labs(x = k, y = paste("Residuals", i)) +
                ggplot2::theme_bw()
              
              cor_test_result <- cor.test(data_plot$Residuals, data_plot$cov)
              p_value2 <- cor_test_result$p.value
              
            } else {
              data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
              plot2 <- ggstatsplot::ggbetweenstats(data = data_plot,
                                                  x     = `cov`,
                                                  y     = `Residuals` ,
                                                  type  = "nonparametric",     # nonparametric   parametric    robust   bayes
                                                  xlab  = k,
                                                  ylab  = paste("Residuals", i)) + 
                ggplot2::theme_bw() + 
                ggplot2::theme(legend.position="none")
              
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
        }
      }
    }
  }
  
  # Second case: covariates are not selected after the vote
  if (is.na(result_ML[i, 1]))  {
    list_cov <- strsplit(res[, i], ",")
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
              ggplot2::labs(x = k, y = paste("Residuals", i)) +
              ggplot2::theme_bw()
            
            cor_test_result <- cor.test(data_plot$Residuals, data_plot$cov)
            p_value <- cor_test_result$p.value
            
          } else {
            data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
            plot <- ggstatsplot::ggbetweenstats(data = data_plot,
                                                x    = `cov`,
                                                y    = `Residuals`,
                                                type = "nonparametric",     # nonparametric   parametric    robust   bayes
                                                xlab = k,
                                                ylab = paste("Residuals", i)) + 
              ggplot2::theme_bw() + 
              ggplot2::theme(legend.position="none")
            
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
                ggplot2::labs(x = k, y = paste("Residuals", i)) +
                ggplot2::theme_bw()
              
              cor_test_result <- cor.test(data_plot$Residuals, data_plot$cov)
              p_value2 <- cor_test_result$p.value
              
            } else {
              data_plot <- data.frame(Residuals = residuals, cov = c(dat[-train.ind, k]))
              plot2 <- ggstatsplot::ggbetweenstats(data = data_plot,
                                                   x    = `cov`,
                                                   y    = `Residuals` ,
                                                   type = "nonparametric",     # nonparametric   parametric    robust   bayes
                                                   xlab = k,
                                                   ylab = paste("Residuals", i)) + 
                ggplot2::theme_bw() + 
                ggplot2::theme(legend.position="none")
              
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
    else {message(paste("No variables selected for the 5 folds, so no model to be trained for", i))
      return()}
  }
  
  # Check if plots_list is empty
  if (length(plots_list) == 0) {
    list_cov <- strsplit(res[, i], ",")
    list_cov_nb <- trimws(unlist(list_cov))
    comptage <- as.data.frame(table(list_cov_nb))
    if (nrow(comptage) != 0){
      message(paste("No residuals plots with a significant p-value for", i))
      return()
    }
  }
  
  # Return the list of subplots for the given parameter i
  return(plots_list)
}
