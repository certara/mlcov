# mlcov 0.1.0

* Unified `ml_cov_search()` covers the eight Boruta learner × Lasso variants:
  LightGBM (default), random forest, XGBoost, and CatBoost, each with or
  without Lasso pre-screening (`use_lasso`, `lambda_lasso`).
* New arguments: `boruta_algorithm`, `boruta_pvalue`, `n_folds`,
  `vote_threshold`, `log_ebes`, and `boruta_max_runs`.
* **Breaking (relative to 0.0.2):** default learner is LightGBM (was XGBoost);
  default Lasso penalty is `lambda.min` (was `lambda.1se`).
* **Breaking (relative to 0.0.2):** SHAP values, SHAP seeds, and RMSE columns
  are no longer computed inside `ml_cov_search()`. Use
  `generate_shap_summary_plot(result, data)` and `generate_residuals_plot()`
  as post-processing steps.
* `generate_shap_summary_plot()` now requires the original `data` argument and
  trains an XGBoost interpreter on the voted covariates. Plots use xgboost's
  SHAP summary (compatible with xgboost 3.x) rather than SHAPforxgboost.
* `print.mlcov_data()` reports algorithm, Lasso/λ, folds, and vote threshold.
* XGBoost importance for Boruta is vendored from Boruta 8 (`getImpXgboost` was
  removed in Boruta 10). LightGBM and CatBoost adapters match the v2 manuscript
  hyperparameters. LightGBM importance follows the simulation-study function:
  `make.names()` on feature names, `as.matrix()`, and categorical columns
  passed by name (factors are not integer-coded). `make.names()` also keeps
  LightGBM from rejecting Boruta's one-column label
  `x[, decReg != "Rejected"]`.
* Lasso pre-screening calls `glmnet::cv.glmnet()` without `nfolds`, so the
  penalty uses glmnet's default of 10 folds, independent of outer `n_folds`.
* **Breaking (relative to 0.0.2):** per-fold selections are returned as
  `result_folds` only. Diagnostic helpers still read `result_5folds` from
  objects saved by earlier versions.
* Vote tallying ignores empty / `NA` fold cells so they cannot appear as a
  covariate named `"NA"`.
* `n_folds`, `vote_threshold`, and `boruta_max_runs` must be finite whole
  numbers. `n_folds` cannot exceed the number of unique subjects. A column
  cannot be both a parameter and a covariate, or both continuous and categorical.
* Objects saved by 0.0.2, which have no `settings` list, print as Lasso
  `lambda.1se`, XGBoost, and a vote threshold of 2.
* `caret` moved out of `Depends`. `lightgbm` is an Import (default learner).
  Random forest uses Boruta's ranger importance (`Suggests: ranger`) and
  stops with an install hint when `ranger` is missing.
  CatBoost is optional (`Suggests`) and is not on CRAN;
  `boruta_algorithm = "catboost"` checks for the package at run time.
  SHAP plots no longer require `SHAPforxgboost`. Continuous residual plots use
  ggplot2 `geom_smooth()` plus a correlation subtitle (ggplot2 4.x is not
  compatible with `ggpmisc::stat_poly_line()` when ggplot2 is not attached).

# mlcov 0.0.2

* Previous GitHub release: Lasso + Boruta-XGBoost search, SHAP summary plots,
  and residual diagnostic plots.
