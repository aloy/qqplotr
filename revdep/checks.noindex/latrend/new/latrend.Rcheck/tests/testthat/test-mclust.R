context('mclust models')
skip_if_not_installed('mclust')
rngReset()

test_that('default llpa', {
  m = lcMethodTestMclustLLPA()
  model = latrend(m, testLongData) %>%
    expect_silent()
  expect_valid_lcModel(model)
})
