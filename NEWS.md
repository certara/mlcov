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
  hyperparameters.
* Inner Lasso cross-validation uses `n_folds` (minimum 3), matching the
  manuscript 5-fold λ search rather than glmnet's default of 10.
* LightGBM importance sanitizes feature names before fitting. Boruta's
  single-column `cbind()` labels that column `x[, decReg != "Rejected"]`,
  which LightGBM 4.x rejects as a JSON-special name.
* Vote tallying ignores empty / `NA` fold cells so they cannot appear as a
  covariate named `"NA"`.
* `caret` moved out of `Depends`. `lightgbm` is an Import (default learner).
  CatBoost is optional (`Suggests`) and is not on CRAN;
  `boruta_algorithm = "catboost"` checks for the package at run time.
  SHAP plots no longer require `SHAPforxgboost`. Continuous residual plots use
  ggplot2 `geom_smooth()` plus a correlation subtitle (ggplot2 4.x is not
  compatible with `ggpmisc::stat_poly_line()` when ggplot2 is not attached).

# mlcov 0.0.2

* Previous GitHub release: Lasso + Boruta-XGBoost search, SHAP summary plots,
  and residual diagnostic plots.
