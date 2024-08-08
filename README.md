# mlcov
R package for selection of covariate effects using ML

## Installation

```
devtools::install_github("certara/mlcov")
```

# Usage

```
library(mlcov)
data_file <- system.file(package = "mlcov", "supplementary", "tab2")
result <- ml_cov_search(data = read.table(data_file, skip = 1, header = TRUE), #NONMEM output
                        pop_param = c("V1","CL"),
                        cov_continuous = c("AGE","WT","HT","BMI","ALB","CRT",
                                           "FER","CHOL","WBC","LYPCT","RBC",
                                           "HGB","HCT","PLT"),
                        cov_factors = c("SEX","RACE","DIAB","ALQ","WACT","SMQ"))
print(result)
                      
```

Generate SHAP plots:

```
generate_shap_summary_plot(
      result,
      x_bound = NULL,
      dilute = FALSE,
      scientific = FALSE,
      my_format = NULL,
      title = NULL,
      title.position = 0.5,
      ylab = NULL,
      xlab = NULL)
```

Generate residual plots:

Cl

```
generate_residuals_plot(data = read.table(data_file, skip = 1, header = TRUE), result, pop_param = 'CL')
```

V1

```
generate_residuals_plot(data = read.table(data_file, skip = 1, header = TRUE), result, pop_param ='V1')
```

