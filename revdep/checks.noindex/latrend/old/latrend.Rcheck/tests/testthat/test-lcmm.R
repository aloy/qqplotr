context('LCMM models')
skip_if_not_installed('lcmm')
rngReset()

test_that('default gmm', {
  m = lcMethodTestLcmmGMM()
  model = latrend(m, testLongData) %>%
    expect_silent()
  expect_valid_lcModel(model)
})

test_that('gmm with single cluster', {
  latrend(lcMethodTestLcmmGMM(), testLongData, nClusters=1) %>%
    expect_valid_lcModel()
})

test_that('gmm with empty cluster', {
  latrend(lcMethodTestLcmmGMM(), testLongData, nClusters=5) %>%
    expect_valid_lcModel()
})

test_that('default gbtm', {
  m = lcMethodTestLcmmGBTM()
  model = latrend(m, testLongData) %>%
    expect_silent()
  expect_valid_lcModel(model)
})

test_that('gbtm with nclusters', {
  latrend(lcMethodTestLcmmGBTM(), testLongData, nClusters=1) %>%
    expect_valid_lcModel()
})
