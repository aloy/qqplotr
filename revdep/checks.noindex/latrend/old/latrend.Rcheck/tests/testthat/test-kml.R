context('KML model')
skip_if_not_installed('kml')
rngReset()

test_that('method', {
  kml = lcMethodTestKML()
  expect_output(print(kml))
})

test_that('default', {
  m = lcMethodTestKML()
  model = latrend(m, testLongData) %>%
    expect_silent()
  expect_valid_lcModel(model)
})

test_that('BIC', {
  methods = lcMethods(lcMethodTestKML(), nClusters=c(1, 5))
  models = latrendBatch(methods, testLongData)

  expect_is(BIC(models[[1]]), 'numeric')
  expect_is(BIC(models[[2]]), 'numeric')
})

test_that('nclusters', {
  methods = lcMethods(lcMethodTestKML(), nClusters=c(1, 5))
  models = latrendBatch(methods, testLongData)

  expect_valid_lcModel(models[[1]])
  expect_valid_lcModel(models[[2]])
})

test_that('predictPostprob', {
  model = latrend(lcMethodTestKML(), testLongData)
  testData = testLongData[Traj %in% unique(Traj)[1:3]]
  pp = predictPostprob(model, newdata = testData)
  expect_true(is_valid_postprob(pp, model))
})

test_that('cld.Rdata not present', {
  expect_false(file.exists('cld.Rdata'))
})
