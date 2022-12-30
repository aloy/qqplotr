testthat::test_that(desc = "lme_model: lmerTest (specify model)", {
  fit <- lme_model(
    data = popular,
    model = "popular ~ extrav + sex + (1 | class)",
    use_package = "lmerTest"
  )
  expect_equal(class(fit)[1], "lmerModLmerTest")
})
