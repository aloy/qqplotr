context('mclust models')
skip_if_not_installed('mclust')
rngReset()
tests = setdiff(DEFAULT_LATREND_TESTS, c('data-na', 'data-varlen'))

test_that('default', {
  expect_true({
    test.latrend('lcMethodMclustLLPA', tests = tests)
  })
})
