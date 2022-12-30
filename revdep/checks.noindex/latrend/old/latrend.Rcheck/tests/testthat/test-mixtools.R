context('mixtools')
skip_if_not_installed('mixtools')
skip_on_cran()
rngReset()
tests = setdiff(DEFAULT_LATREND_TESTS, c('data-na', 'data-varlen'))

make.gmm = function(id, time, response, ...) {
  lcMethodMixtoolsGMM(
    formula = as.formula(sprintf('%s ~ 1 + (1 | %s)', response, id)),
    id = id,
    time = time,
    ...
  )
}

test_that('default np', {
  expect_true({
    test.latrend(
      'lcMethodMixtoolsNPRM',
      args = list(maxiter = 10, eps = 1e-4, seed = 1),
      tests = tests
    )
  })
})


# disabled due to taking a very long time
# test_that('default gmm', {
#   skip_on_cran()
#   expect_true({
#     test.latrend(
#       'lcMethodMixtoolsGMM',
#       instantiator = make.gmm,
#       args = list(eps = 1e-1, seed = 1),
#       tests = tests
#     )
#   })
# })
