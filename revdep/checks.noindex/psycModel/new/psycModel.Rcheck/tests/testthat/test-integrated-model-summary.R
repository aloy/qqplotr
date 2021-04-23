testthat::test_that(desc = "checking model correctness", {
  return_list <- expect_warning(expect_warning(
    integrated_model_summary(
      data = iris,
      response_variable = "Sepal.Length",
      predictor_variable = where(is.numeric),
      two_way_interaction_factor = c(Sepal.Width, Petal.Length),
      interaction_plot = TRUE,
      simple_slope = TRUE, # you can request simple slope
      model_summary = TRUE,
      return_result = TRUE,
      assumption_plot = FALSE,
      quite = TRUE
    )
  ))
  expect_equal(return_list$summary$assumption_plot, NULL)
  expect_false(is.null(return_list$interaction_plot))
  expect_false(is.null(return_list$simple_slope$jn_plot))
})

testthat::test_that(desc = "test quite", {
  quite_output <- capture_output(expect_warning(integrated_model_summary(
    data = iris,
    response_variable = "Sepal.Length",
    predictor_variable = where(is.numeric),
    two_way_interaction_factor = c(Sepal.Width, Petal.Length),
    simple_slope = TRUE,
    interaction_plot = FALSE,
    quite = TRUE
  ), regexp = "Species"))
  expect_equal(quite_output, "")
})

testthat::test_that(desc = "test glm", {
  return_list <- expect_warning(expect_warning(integrated_model_summary(
    response_variable = incidence,
    predictor_variable = period,
    family = "poisson", # or you can enter as poisson(link = 'log'),
    data = lme4::cbpp,
    assumption_plot = TRUE,
    quite = TRUE,
    return_result = TRUE
  )))
  expect_false(is.null(return_list$summary$assumption_plot))
  expect_equal(return_list$simple_slope$jn_plot, NULL)
  expect_equal(return_list$simple_slope$simple_slope_df, NULL)
  expect_equal(return_list$interaction_plot, NULL)
})
