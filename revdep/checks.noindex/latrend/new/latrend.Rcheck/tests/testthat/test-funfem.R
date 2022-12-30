context('funfem')
skip_if_not_installed('funFEM')
skip_on_cran()
rngReset()
# funFEM does not support K = 1
tests = setdiff(DEFAULT_LATREND_TESTS, c('cluster-single', 'data-na', 'data-varlen'))

library(funFEM)
# data(CanadianWeather)
# femData = CanadianWeather$dailyAv[,,'Temperature.C'] %>% t()

test_that('default', {
  expect_true({
    test.latrend(
      'lcMethodFunFEM',
      tests = tests,
      clusterRecovery = 'skip',
      args = list(basis = function(time) fda::create.bspline.basis(time, nbasis = 4L, norder = 2L), seed = 1L)
    )
  })
})
