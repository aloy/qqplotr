context('latrendBoot')
rngReset()

test_that('latrendBoot', {
  expect_silent({
    models = latrendBoot(mRandom, data = testLongData, samples = 3, seed = 1)
  })

  expect_is(models, 'lcModels')
  expect_length(models, 3)

  # test if data bootstrap sample calls are correct
  expect_equal(deparse(getCall(models[[1]])$data), 'bootSample(testLongData, "id", 1140350788L)')
  expect_equal(deparse(getCall(models[[2]])$data), 'bootSample(testLongData, "id", 312928385L)')
  expect_equal(deparse(getCall(models[[3]])$data), 'bootSample(testLongData, "id", 866248189L)')
})

test_that('latrendBoot without seed', {
  models = latrendBoot(mRandom, data = testLongData, samples = 2)

  expect_is(models, 'lcModels')
  expect_length(models, 2)
})

test_that('latrendBoot with method var', {
  models = latrendBoot(mRandom, data = testLongData, samples = 2)

  expect_is(models, 'lcModels')
  expect_length(models, 2)
})

test_that('latrendBoot with single sample', {
  models = latrendBoot(mRandom, data = testLongData, samples = 1)

  expect_is(models, 'lcModels')
  expect_length(models, 1)
})
