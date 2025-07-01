context('KML model')
skip_if_not_installed('kml')
rngReset()
tests = DEFAULT_LATREND_TESTS

test_that('default', {
  expect_true({
    test.latrend('lcMethodKML', tests = tests, args = list(nbRedrawing = 1, maxIt = 10, seed = 1))
  })
})

# test_that('predictPostprob', {
#   model = latrend(lcMethodTestKML(), testLongData)
#   testData = testLongData[id %in% unique(id)[1:3]]
#   pp = predictPostprob(model, newdata = testData)
#   expect_true(is_valid_postprob(pp, model))
# })

test_that('cld.Rdata not present', {
  expect_false(file.exists('cld.Rdata'))
})
