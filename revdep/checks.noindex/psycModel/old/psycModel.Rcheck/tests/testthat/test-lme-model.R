
testthat::test_that(desc = "check use_package = nlme", {
  fit1 <- expect_warning(lme_model(
    data = popular,
    response_variable = popular,
    random_effect_factors = sex,
    non_random_effect_factors = c(extrav, sex, texp),
    id = class,
    use_package = "nlme",
    quite = T
  ), regexp = "optim")
  expect_equal(class(fit1), "lme")
})


testthat::test_that(desc = "check use_package = lme4", {
  fit1 <- lme_model(
    data = popular,
    response_variable = popular,
    non_random_effect_factors = c(extrav, sex),
    id = class,
    use_package = "lme4",
    quite = T
  )
  expect_equal(class(fit1)[1], "lmerMod")
})
