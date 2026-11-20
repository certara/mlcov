
# mlcov

`mlcov` is an R package for machine-learning-based covariate selection
in population pharmacokinetic and pharmacodynamic analyses. The package
evaluates associations between empirical Bayes estimates (EBEs) and
candidate covariates using optional Lasso pre-selection followed by the
Boruta algorithm.

## How the package works

The complete covariate-selection workflow is performed automatically by
`ml_cov_search()`. The user provides the dataset, the population
parameters to analyze, the continuous and categorical covariates, and
any settings that differ from the defaults. Lasso, Boruta, the repeated
fold analyses, and the final voting procedure are all run internally by
the function.

For each population parameter, `ml_cov_search()` automatically performs
the following steps:

1.  **Data preparation**

    The function selects the subject identifier, EBEs, and candidate
    covariates. It retains one unique set of values per subject,
    converts categorical covariates to factors, and log-transforms the
    EBEs when `log_ebes = TRUE`.

2.  **Outer data splitting**

    Subjects are randomly divided into `n_folds` folds. At each
    iteration, one fold is held out and the remaining folds are used as
    the training dataset for covariate selection. By default, five outer
    folds are used.

3.  **Optional Lasso pre-selection**

    When `use_lasso = TRUE`, Lasso is applied within each training
    dataset to reduce the number of candidate covariates before Boruta.
    The penalty can be selected using either `lambda.min` or
    `lambda.1se`. The internal cross-validation of `glmnet::cv.glmnet()`
    uses its own default settings.

4.  **Boruta covariate selection**

    Boruta is applied to the covariates retained after the optional
    Lasso step. Feature importance is calculated using the learner
    specified in `boruta_algorithm`. The available learners are:

    - LightGBM (default);
    - random forest, which requires the suggested `ranger` package;
    - XGBoost;
    - CatBoost, which is optional and is not on CRAN.

5.  **Fold-wise voting**

    Once all folds have been analyzed, the function counts how many
    times each covariate was confirmed by Boruta. A covariate is
    included in the final selection when it is confirmed in at least
    `vote_threshold` folds. The default threshold is two folds.

All these operations are executed through a single call to
`ml_cov_search()`. The user does not need to run Lasso, Boruta, or the
voting step separately. Each Boruta learner can also be used without
Lasso by setting `use_lasso = FALSE`.

The recommended default matches the evaluation study: Lasso with
`lambda.min` plus Boruta-LightGBM. Random forest had a high Type I error
in that study and is not the recommended default.

## Installation

``` r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("certara/mlcov")
```

``` r
library(mlcov)
```

Random forest requires `ranger` (`install.packages("ranger")`). CatBoost
is optional and must be installed separately when using
`boruta_algorithm = "catboost"`; see the [CatBoost R installation
notes](https://catboost.ai/en/docs/installation/r-installation).

## Example dataset

The package contains the `tab33` dataset, which can be used to test the
workflow:

``` r
data_file <- system.file(package = "mlcov", "supplementary", "tab33")
data <- read.table(data_file, skip = 1, header = TRUE)
```

The dataset contains subject identifiers, population parameter
estimates, and candidate continuous and categorical covariates.

## Basic example

The following call runs the complete workflow using the package
defaults:

``` r
result <- ml_cov_search(
  data = data,
  pop_param = c("V1", "CL"),
  cov_continuous = c(
    "AGE", "WT", "HT", "BMI", "ALB", "CRT", "FER",
    "CHOL", "WBC", "LYPCT", "RBC", "HGB", "HCT", "PLT"
  ),
  cov_factors = c("SEX", "RACE", "DIAB", "ALQ", "WACT", "SMQ")
)

print(result)
```

This single call automatically applies Lasso, runs Boruta-LightGBM
within each of the five outer folds, and retains covariates confirmed in
at least two folds.

## Default settings

``` r
seed             = 123
use_lasso        = TRUE
lambda_lasso     = "lambda.min"
boruta_algorithm = "lightgbm"
boruta_pvalue    = 0.01
n_folds          = 5
vote_threshold   = 2
log_ebes         = TRUE
boruta_max_runs  = 200
```

These settings can be changed directly in the `ml_cov_search()` call.

For example, LightGBM can be run without Lasso as follows:

``` r
result_without_lasso <- ml_cov_search(
  data = data,
  pop_param = c("V1", "CL"),
  cov_continuous = c(
    "AGE", "WT", "HT", "BMI", "ALB", "CRT", "FER",
    "CHOL", "WBC", "LYPCT", "RBC", "HGB", "HCT", "PLT"
  ),
  cov_factors = c("SEX", "RACE", "DIAB", "ALQ", "WACT", "SMQ"),
  use_lasso = FALSE,
  boruta_algorithm = "lightgbm"
)
```

## Main arguments

| Argument | Description | Default |
|----|----|---:|
| `data` | Dataset containing `ID`, EBEs, and candidate covariates | Required |
| `pop_param` | Names of the population parameters to analyze | Required |
| `cov_continuous` | Names of continuous covariates | Optional |
| `cov_factors` | Names of categorical covariates | Optional |
| `seed` | Random seed | `123` |
| `use_lasso` | Apply Lasso before Boruta | `TRUE` |
| `lambda_lasso` | Lasso penalty: `lambda.min` or `lambda.1se` | `lambda.min` |
| `boruta_algorithm` | Importance learner used by Boruta | `lightgbm` |
| `boruta_pvalue` | Significance level used by Boruta | `0.01` |
| `n_folds` | Number of outer folds | `5` |
| `vote_threshold` | Minimum number of folds confirming a covariate | `2` |
| `log_ebes` | Log-transform the EBEs | `TRUE` |
| `boruta_max_runs` | Maximum number of Boruta iterations | `200` |

## Output

`ml_cov_search()` returns an object of class `mlcov_data` containing:

- `result_ML`: final covariates retained after the voting step;
- `result_folds`: covariates confirmed within each outer fold;
- `pop_param`: analyzed population parameters;
- `cov_continuous`: continuous covariates supplied by the user;
- `cov_factors`: categorical covariates supplied by the user;
- `settings`: settings used for the analysis.

``` r
result$result_ML
result$result_folds
result$settings
```

## Optional diagnostics

SHAP and residual plots are not part of covariate search. After
`ml_cov_search()`, `generate_shap_summary_plot()` explains the voted
covariates with XGBoost SHAP values, and `generate_residuals_plot()`
checks unselected covariates for leftover residual trends.

``` r
shap_plots <- generate_shap_summary_plot(result, data, title.position = 0.5)
cl_resid <- generate_residuals_plot(data, result, pop_param = "CL")
```

A longer argument reference is in
`vignette("mlcov-updated-usage", package = "mlcov")`.

## Interpretation

`mlcov` is intended to support covariate screening during population
model development. Covariates retained by the workflow should
subsequently be assessed using pharmacological plausibility and
confirmatory model-based analyses.

The original package description is in the [PAGE 2024
abstract](https://www.page-meeting.org/?abstract=10996).

## License

This project is licensed under the GNU General Public License v3.0.
