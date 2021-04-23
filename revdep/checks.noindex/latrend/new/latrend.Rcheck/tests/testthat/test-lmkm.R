context('LM-KM')
rngReset()

test_that('default', {
  set.seed(1)
  m = lcMethodTestLMKM()
  model = latrend(m, testLongData)
  expect_valid_lcModel(model)
})

test_that('single cluster', {
  m = lcMethodTestLMKM(nClusters=1)
  model = latrend(m, testLongData)
  expect_valid_lcModel(model)
})
