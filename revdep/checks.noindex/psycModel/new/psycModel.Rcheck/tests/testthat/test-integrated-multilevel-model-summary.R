testthat::test_that(desc = "simple_slope test", {
  testthat::skip_on_cran()
  fit1 <- integrated_multilevel_model_summary(
    data = popular,
    response_variable = popular,
    non_random_effect_factors = c(extrav, texp, sex),
    three_way_interaction_factor = c(extrav, sex, texp), # three-way interaction
    id = class,
    simple_slope = TRUE, # you can request simple slope
    model_summary = TRUE,
    quite = T,
    interaction_plot = FALSE,
    return_result = TRUE
  )

  fit2 <- integrated_multilevel_model_summary(
    data = popular,
    response_variable = popular,
    non_random_effect_factors = c(texp, sex),
    two_way_interaction_factor = c(sex, texp), # three-way interaction
    id = class,
    simple_slope = TRUE, # you can request simple slope
    model_summary = TRUE,
    interaction_plot = FALSE,
    quite = T,
    return_result = TRUE
  )
  expect_false(any(fit1$simple_slope_df[2] == "Mean"))
  expect_true(!is.null(fit1$simple_slope$jn_plot))
})

testthat::test_that(desc = "", {
  testthat::skip_on_cran()
  fit <- integrated_multilevel_model_summary(
    data = popular,
    response_variable = popular,
    non_random_effect_factors = c(texp, sex),
    two_way_interaction_factor = c(sex, texp), # three-way interaction
    id = class,
    simple_slope = TRUE, # you can request simple slope
    model_summary = TRUE,
    interaction_plot = FALSE,
    quite = TRUE,
    return_result = TRUE
  )
  expect_false(any(fit$simple_slope$simple_slope_df[2] == "Mean"))
  expect_true(!is.null(fit$simple_slope$jn_plot))
})

testthat::test_that(desc = "", {
  testthat::skip_on_cran()
  fit <- expect_warning(expect_warning(
    integrated_multilevel_model_summary(
      response_variable = incidence,
      random_effect_factors = period,
      family = "poisson", # or you can enter as poisson(link = 'log')
      id = herd,
      data = lme4::cbpp,
      model_summary = TRUE,
      quite = TRUE,
      return_result = TRUE
    ),
    regexp = "coerced"
  ), regexp = "interaction_plot")

  expect_false(any(fit$simple_slope$simple_slope_df[2] == "Mean"))
  expect_equal(fit$simple_slope$jn_plot, NULL)
})
