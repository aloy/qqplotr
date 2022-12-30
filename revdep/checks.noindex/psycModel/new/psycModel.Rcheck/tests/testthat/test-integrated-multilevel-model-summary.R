testthat::test_that("lme_multilevel_model_summary: lme4 model", {
  suppressWarnings(
    summary <- lme_multilevel_model_summary(
    data = popular,
    response_variable = popular,
    random_effect_factors = extrav,
    non_random_effect_factors = c(sex, texp),
    two_way_interaction_factor = c(sex, extrav),
    id = class,
    use_package = "lme4",
    quite = TRUE,
    assumption_plot = TRUE,
    simple_slope = TRUE,
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

testthat::test_that("lme_multilevel_model_summary: lmerTest model", {
  suppressWarnings(summary <- lme_multilevel_model_summary(
    data = popular,
    response_variable = popular,
    random_effect_factors = extrav,
    non_random_effect_factors = c(sex, texp),
    two_way_interaction_factor = c(sex, extrav),
    id = class,
    use_package = "lmerTest",
    quite = TRUE,
    assumption_plot = TRUE,
    simple_slope = TRUE,
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


testthat::test_that("lme_multilevel_model_summary: nlme model", {
  suppressWarnings(summary <- lme_multilevel_model_summary(
    data = popular,
    response_variable = popular,
    random_effect_factors = extrav,
    non_random_effect_factors = c(sex, texp),
    two_way_interaction_factor = c(sex, extrav),
    id = class,
    use_package = "nlme",
    opt_control = "optim",
    quite = TRUE,
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
  expect_true(is.null(summary$simple_slope$simple_slope_df)) # unable to compute simple slope
  expect_true(is.null(summary$simple_slope$jn_plot))
})
