context('MixTVEM')
skip_if(!exists('TVEMMixNormal'), message = 'skipping MixTVEM tests because the TVEMMixNormal() function is not loaded')
rngReset()

test_that('default', {
  method = lcMethodTestMixTVEM()
  model = latrend(method, testLongData)
  expect_valid_lcModel(model)
})
