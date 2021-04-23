context('random')
rngReset()

test_that('default', {
  model = latrend(lcMethodTestRandom(alpha=1, nClusters=3), data=testLongData, seed=1)
  expect_valid_lcModel(model)

  expect_equal(nClusters(model), 3)
  expect_equivalent(clusterProportions(model), c(.06, .48, .46))
})

test_that('uniform groups', {
  model = latrend(lcMethodTestRandom(alpha=1e3, nClusters=8), data=testLongData, seed=1)
  expect_valid_lcModel(model)

  expect_equal(nClusters(model), 8)
  expect_true(all(clusterProportions(model) >= .1))
})

test_that('single group', {
  model = latrend(lcMethodTestRandom(alpha=1, nClusters=1), data=testLongData, seed=1)
  expect_valid_lcModel(model)

  expect_equal(nClusters(model), 1)
  expect_equivalent(clusterProportions(model), 1)
})
