testthat::test_that("model_summary: lm model", {
  model <- lm_model(
    data = iris[1:4],
    response_variable = "Sepal.Length",
    predictor_variable = c(Sepal.Width, Petal.Width),
    two_way_interaction_factor = c(Sepal.Width, Petal.Width),
    quite = TRUE
  )
  summary <- model_summary(model,
    return_result = TRUE,
    assumption_plot = TRUE,
    quite = TRUE
  )
  expect_false(is.null(summary$model_summary))
  expect_false(is.null(summary$model_performance_df))
  expect_false(is.null(summary$assumption_plot))
})

testthat::test_that("model_summary: anova model", {
  model = stats::aov(Petal.Length ~ Species,data = iris)
  summary <- model_summary(model,
                           return_result = TRUE,
                           assumption_plot = TRUE,
                           quite = TRUE
  )
  expect_false(is.null(summary$model_summary))
  expect_false(is.null(summary$model_performance_df))
  expect_false(is.null(summary$assumption_plot))
})

# Stop supporting for glm models 
# testthat::test_that("model_summary: glm model", {
#   model <- glm_model(
#     response_variable = incidence,
#     predictor_variable = period,
#     family = "poisson",
#     data = lme4::cbpp,
#     quite = TRUE,
#   )
#   summary <- model_summary(model,
#     return_result = TRUE,
#     assumption_plot = TRUE,
#     quite = TRUE
#   )
#   expect_false(is.null(summary$model_summary))
#   expect_false(is.null(summary$model_performance_df))
#   expect_false(is.null(summary$assumption_plot))
# })

# 
# testthat::test_that(desc = "model_summary: nlme model", {
#   model <- lme_model(
#     data = popular,
#     response_variable = popular,
#     random_effect_factors = sex,
#     non_random_effect_factors = c(extrav, sex, texp),
#     id = class,
#     opt_control = "optim",
#     use_package = "nlme",
#     quite = TRUE
#   )
#   summary <- model_summary(model,
#     return_result = TRUE,
#     assumption_plot = TRUE,
#     quite = TRUE
#   )
#   expect_false(is.null(summary$model_summary))
#   expect_false(is.null(summary$model_performance_df))
#   expect_false(is.null(summary$assumption_plot))
# })

testthat::test_that(desc = "model_summary: lmerTest model", {
  model <- lme_model(
    data = popular,
    response_variable = popular,
    random_effect_factors = c(extrav),
    non_random_effect_factors = c(texp),
    id = class,
    use_package = "lmerTest",
    quite = TRUE
  )
  summary <- suppressWarnings(model_summary(model,
    return_result = TRUE,
    assumption_plot = TRUE,
    quite = TRUE
  ))
  expect_false(is.null(summary$model_summary))
  expect_false(is.null(summary$model_performance_df))
  expect_false(is.null(summary$assumption_plot))
})

testthat::test_that(desc = "model_summary: lme4 model", {
  model <- lme_model(
    data = popular,
    response_variable = popular,
    random_effect_factors = c(extrav),
    non_random_effect_factors = c(texp),
    id = class,
    use_package = "lme4",
    quite = TRUE
  )
  summary <- suppressWarnings(model_summary(model,
    return_result = TRUE,
    assumption_plot = TRUE,
    quite = TRUE
  ))
  expect_false(is.null(summary$model_summary))
  expect_false(is.null(summary$model_performance_df))
  expect_false(is.null(summary$assumption_plot))
})
