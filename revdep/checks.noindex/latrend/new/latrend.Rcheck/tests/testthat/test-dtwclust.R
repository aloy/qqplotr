context('dtwclust')
skip_if_not_installed('dtwclust')
rngReset()

test_that('default', {
  m = lcMethodDtwclust(response = 'Value')
  model = latrend(m, testLongData)
  expect_valid_lcModel(model)
})

test_that('fuzzy default', {
  m = lcMethodDtwclust(response = 'Value', type = 'fuzzy')
  model = latrend(m, testLongData)
  expect_valid_lcModel(model)
})
