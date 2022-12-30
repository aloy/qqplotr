testthat::test_that("lm_model_summary: lm model", {
  suppressWarnings(summary <- lm_model_summary(
    data = iris[1:4],
    response_variable = "Sepal.Length",
    predictor_variable = c(Sepal.Width, Petal.Width),
    two_way_interaction_factor = c(Sepal.Width, Petal.Width),
    quite = TRUE,
    simple_slope = TRUE,
    assumption_plot = TRUE,
    return_result = TRUE
  ))
  # model
  expect_false(is.null(summary$model))

  # summary
  expect_false(is.null(summary$summary$model_summary))
  expect_false(is.null(summary$summary$model_performance_df))
  expect_false(is.null(summary$summary$assumption_plot))

  # interaction plot
  expect_false(is.null(summary$interaction_plot))

  # simple slope
  expect_false(is.null(summary$simple_slope$simple_slope_df))
  expect_false(is.null(summary$simple_slope$jn_plot))
})

testthat::test_that(desc = "lm_model_summary: glm model", {
  suppressWarnings(summary <- lm_model_summary(
    response_variable = incidence,
    predictor_variable = period,
    family = "poisson",
    data = lme4::cbpp,
    assumption_plot = TRUE,
    quite = TRUE,
    return_result = TRUE
  ))
  # model
  expect_false(is.null(summary$model))

  # summary
  expect_false(is.null(summary$summary$model_summary))
  expect_false(is.null(summary$summary$model_performance_df))
  expect_false(is.null(summary$summary$assumption_plot))

  # interaction plot
  expect_true(is.null(summary$interaction_plot)) # no interaction plot

  # simple slope
  expect_true(is.null(summary$simple_slope$simple_slope_df))
  expect_true(is.null(summary$simple_slope$jn_plot))
})
