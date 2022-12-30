context('dtwclust')
skip_if_not_installed('dtwclust')
rngReset()
# dtwclust does not seem to support fitting 1 cluster
tests = setdiff(DEFAULT_LATREND_TESTS, c('cluster-single', 'data-na', 'data-varlen'))

test_that('default', {
  expect_true({
    test.latrend(
      'lcMethodDtwclust',
      tests = tests,
      clusterRecovery = 'skip',
      args = list(seed = 1)
    )
  })
})

test_that('fuzzy default', {
  expect_true({
    test.latrend(
      'lcMethodDtwclust',
      tests = tests,
      args = list(type = 'fuzzy', seed = 1),
      clusterRecovery = 'skip'
    )
  })
})
