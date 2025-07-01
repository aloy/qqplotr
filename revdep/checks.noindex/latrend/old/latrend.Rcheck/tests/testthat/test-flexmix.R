skip_if_not_installed('flexmix')
skip_on_cran()
rngReset()
tests = setdiff(DEFAULT_LATREND_TESTS, 'data-na')

make.flexmix = function(response, ...) {
  lcMethodFlexmix(formula = as.formula(sprintf('%s ~ 1', response)), ...)
}

make.gbtm = function(response, ...) {
  lcMethodFlexmixGBTM(
    formula = as.formula(sprintf('%s ~ 1', response)),
    ...,
    control = list(iter.max = 1, tolerance = 1e-3),
    seed = 1
  )
}


test_that('default', {
  expect_true({
    test.latrend('lcMethodFlexmix', instantiator = make.flexmix, tests = tests)
  })
})


test_that('model spec', {
  expect_true({
    test.latrend(
      'lcMethodFlexmix',
      instantiator = make.flexmix,
      tests = tests,
      args = list(
        model = flexmix::FLXMRglm(formula = ~ 1)
      )
    )
  })
})


test_that('gbtm', {
  skip('convergence stability problem')
  expect_true({
    test.latrend('lcMethodFlexmix', instantiator = make.gbtm, tests = tests)
  })
})
