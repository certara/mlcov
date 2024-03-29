% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_residualsplots.R
\name{generate_residualsplots}
\alias{generate_residualsplots}
\title{Generate Residual Plots for Model Analysis}
\usage{
generate_residualsplots(data, result, i, seed = NULL)
}
\arguments{
\item{data}{Data frame containing the input variables.}

\item{result}{Results object of class "mlcov_data", obtained from the MLCovSearch function.}

\item{i}{An integer indicating the index of the parameter of interest.}

\item{seed}{Numeric value for usage of \code{set.seed()} inside function.}
}
\value{
A list of ggplot objects, each representing a residual plot for a different covariate. The function returns an empty list if no significant relationships are found. It also handles cases where covariates are not selected after the vote.
}
\description{
This function generates residual plots for model analysis based on various covariates and model results. It performs data preprocessing, model fitting using XGBoost, and generates plots to visualize the residuals against the covariates.
}
\examples{
# Assuming 'data' is a data frame with the necessary columns
\dontrun{
plots <- generate_residualsplots(data, 
result, 
i = "V1",
seed = 123)
}
}
