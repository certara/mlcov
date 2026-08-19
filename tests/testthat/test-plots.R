describe("generate_shap_summary_plot", {
  it("errors when the original data argument is missing", {
    result <- stub_mlcov_result()
    expect_error(generate_shap_summary_plot(result), "data")
  })

  it("returns an empty list when no covariates were selected", {
    dat <- synthetic_mlcov_data()
    result <- stub_mlcov_result(cov_selected = NA_character_)
    plots <- generate_shap_summary_plot(result, dat)
    expect_type(plots, "list")
    expect_equal(length(plots), 0)
  })

  it("returns a ggplot that can be built for a single selected covariate", {
    dat <- synthetic_mlcov_data(n = 40)
    result <- stub_mlcov_result()
    plots <- generate_shap_summary_plot(result, dat)
    expect_true("CL" %in% names(plots))
    expect_s3_class(plots$CL, "ggplot")
    expect_silent(ggplot2::ggplot_build(plots$CL))
  })
})

describe("generate_residuals_plot", {
  it("errors when pop_param is not part of the search result", {
    dat <- synthetic_mlcov_data()
    result <- stub_mlcov_result()
    expect_error(
      generate_residuals_plot(dat, result, pop_param = "KA"),
      "pop_param"
    )
  })

  it("returns NULL when no fold selected any covariate", {
    dat <- synthetic_mlcov_data()
    result <- stub_mlcov_result(cov_selected = NA_character_)
    result$result_5folds["CL", ] <- NA_character_
    expect_message(
      out <- generate_residuals_plot(dat, result, pop_param = "CL"),
      "No variables selected"
    )
    expect_null(out)
  })

  it("builds a continuous residual ggplot without ggpmisc", {
    set.seed(1)
    data_plot <- data.frame(Residuals = rnorm(40), cov = rnorm(40))
    built <- build_residual_plot(data_plot, "WT", TRUE, "CL")
    expect_s3_class(built$plot, "ggplot")
    expect_true(is.numeric(built$p_value))
    expect_silent(ggplot2::ggplot_build(built$plot))
  })
})
