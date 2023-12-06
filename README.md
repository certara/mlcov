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
result <- MLCovSearch(tab = read.table(data_file,skip=1,header=T), #NONMEM output
                      list_pop_param = c("V1","CL"),
                      cov_continuous = c("AGE","WT","HT","BMI","ALB","CRT",
                                         "FER","CHOL","WBC","LYPCT","RBC",
                                         "HGB","HCT","PLT"),
                      cov_factors = c("SEX","RACE","DIAB","ALQ","WACT","SMQ"))
print(result)
                      
```

Generate residual plots:

Cl

```
generate_residualsplots(tab = read.table(data_file,skip=1,header=T),
                        list_pop_param = c("V1","CL"),
                        cov_continuous = c("AGE","WT","HT","BMI","ALB","CRT",
                                         "FER","CHOL","WBC","LYPCT","RBC",
                                         "HGB","HCT","PLT"),
                        cov_factors = c("SEX","RACE","DIAB","ALQ","WACT","SMQ"),
                        result_ML = result$result_ML, #selected covariates for parameter of interest after the voting
                        result_5folds = result$result_5folds, #selected covariates for parameter of interest for each folds
                        i=c('CL'))
```

V1

```
generate_residualsplots(tab = read.table(data_file,skip=1,header=T),
                        list_pop_param = c("V1","CL"),
                        cov_continuous = c("AGE","WT","HT","BMI","ALB","CRT",
                                         "FER","CHOL","WBC","LYPCT","RBC",
                                         "HGB","HCT","PLT"),
                        cov_factors = c("SEX","RACE","DIAB","ALQ","WACT","SMQ"),
                        result_ML = result$result_ML, #selected covariates for parameter of interest after the voting
                        result_5folds = result$result_5folds, #selected covariates for parameter of interest for each folds
                        i=c('V1')) #parameter for which the residual plots will be generated
```