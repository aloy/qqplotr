context('latrendBoot')
rngReset()

test_that('latrendBoot', {
  models = latrendBoot(lcMethodTestKML(), data=testLongData, samples=3, seed=1) %>%
    expect_is('lcModels') %>%
    expect_length(3)

  # test if data bootstrap sample calls are correct
  expect_equal(deparse(getCall(models[[1]])$data), 'bootSample(testLongData, "Traj", 1140350788L)')
  expect_equal(deparse(getCall(models[[2]])$data), 'bootSample(testLongData, "Traj", 312928385L)')
  expect_equal(deparse(getCall(models[[3]])$data), 'bootSample(testLongData, "Traj", 866248189L)')
})

test_that('latrendBoot without seed', {
  latrendBoot(lcMethodTestKML(), data=testLongData, samples=2) %>%
    expect_is('lcModels') %>%
    expect_length(2)
})

test_that('latrendBoot with method var', {
  kml = lcMethodTestKML()
  latrendBoot(kml, data=testLongData, samples=2) %>%
    expect_is('lcModels') %>%
    expect_length(2)
})

test_that('latrendBoot with single sample', {
  latrendBoot(lcMethodTestKML(), data=testLongData, samples=1) %>%
    expect_is('lcModels') %>%
    expect_length(1)
})
