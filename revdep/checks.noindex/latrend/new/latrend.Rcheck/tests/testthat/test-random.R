context('random')
rngReset()
tests = DEFAULT_LATREND_TESTS

test_that('default', {
  model = latrend(lcMethodTestRandom(alpha = 1, nClusters = 3), data = testLongData, seed = 1)

  expect_equal(nClusters(model), 3)
  expect_equivalent(clusterProportions(model), c(.06, .48, .46))
})

test_that('uniform groups', {
  model = latrend(lcMethodTestRandom(alpha = 1e3, nClusters = 8), data = testLongData, seed = 1)

  expect_equal(nClusters(model), 8)
  expect_true(all(clusterProportions(model) >= .1))
})

test_that('single group', {
  model = latrend(lcMethodTestRandom(alpha = 1, nClusters = 1), data = testLongData, seed = 1)

  expect_equal(nClusters(model), 1)
  expect_equivalent(clusterProportions(model), 1)
})

test_that('which.weight', {
  expect_equal(which.weight(1), 1)
  expect_equal(which.weight(.1), 1)
  expect_equal(which.weight(c(0, 1, 0)), 2)
  expect_equal(which.weight(c(0, 10, 0)), 2)
  expect_true(which.weight(c(.5, 0, .5)) != 2)
  expect_true(which.weight(c(3, 0, 3)) != 2)

  expect_error(which.weight(0))
  expect_error(which.weight(c(1, -1)))
  expect_error(which.weight(c(1, NA)))
  expect_error(which.weight(c(1, Inf)))
})

test_that('test.latrend', {
  expect_true({
    test.latrend('lcMethodRandom', tests = tests, clusterRecovery = 'skip')
  })
})
