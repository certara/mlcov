synthetic_mlcov_data <- function(n = 60L, seed = 1) {
  set.seed(seed)
  WT <- rnorm(n, 70, 10)
  SEX <- factor(sample(c("0", "1"), n, replace = TRUE))
  RACE <- factor(sample(c("White", "Black", "Asian"), n, replace = TRUE))
  AGE <- rnorm(n, 40, 8)
  NOISE <- rnorm(n)
  eta <- 0.8 * as.numeric(scale(WT)) + 0.25 * (as.integer(SEX) - 1L)
  data.frame(
    ID = seq_len(n),
    CL = exp(log(1) + eta + rnorm(n, 0, 0.05)),
    V1 = exp(rnorm(n, log(10), 0.15)),
    WT = as.numeric(WT),
    AGE = AGE,
    NOISE = NOISE,
    SEX = SEX,
    RACE = RACE,
    stringsAsFactors = FALSE
  )
}

stub_mlcov_result <- function(cov_selected = "WT",
                              pop_param = "CL",
                              cov_continuous = c("WT", "AGE", "NOISE"),
                              cov_factors = c("SEX", "RACE"),
                              n_folds = 5L,
                              log_ebes = TRUE,
                              boruta_algorithm = "lightgbm") {
  result_ML <- data.frame(
    cov_selected = cov_selected,
    stringsAsFactors = FALSE
  )
  rownames(result_ML) <- pop_param
  result_folds <- as.data.frame(
    matrix(cov_selected, nrow = length(pop_param), ncol = n_folds),
    stringsAsFactors = FALSE
  )
  names(result_folds) <- paste0("fold", seq_len(n_folds))
  rownames(result_folds) <- pop_param
  structure(
    list(
      result_ML = result_ML,
      result_5folds = result_folds,
      result_folds = result_folds,
      pop_param = pop_param,
      cov_continuous = cov_continuous,
      cov_factors = cov_factors,
      settings = list(
        use_lasso = TRUE,
        lambda_lasso = "lambda.min",
        boruta_algorithm = boruta_algorithm,
        boruta_pvalue = 0.01,
        n_folds = n_folds,
        vote_threshold = 2L,
        log_ebes = log_ebes,
        boruta_max_runs = 200L,
        seed = 123
      )
    ),
    class = "mlcov_data"
  )
}
