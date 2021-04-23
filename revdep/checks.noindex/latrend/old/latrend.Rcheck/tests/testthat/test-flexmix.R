context('flexmix')
skip_if_not_installed('flexmix')
rngReset()

test_that('default', {
  model = latrend(lcMethodTestFlexmix(), data=testLongData)
  expect_valid_lcModel(model)
})

test_that('one cluster', {
  model = latrend(lcMethodTestFlexmix(), data=testLongData, nClusters=1)
  expect_valid_lcModel(model)
})

test_that('empty cluster', {
  suppressWarnings({
    model = latrend(lcMethodTestFlexmix(), data=testLongData, nClusters=5)
  })
  expect_valid_lcModel(model)
})

test_that('model spec', {
  com = flexmix::FLXMRglm(formula=~Assessment)
  model = latrend(lcMethodTestFlexmix(), data=testLongData, model=com)
  expect_valid_lcModel(model)
})

test_that('gbtm', {
  model = latrend(lcMethodTestFlexmixGBTM(), data=testLongData)
  model@model@converged = TRUE
  expect_valid_lcModel(model)
})
