context('latrendBatch')
rngReset()


test_that('single method and data, cartesian=TRUE', {
  models = latrendBatch(mTest, testLongData, cartesian = TRUE)
  expect_is(models, 'lcModels')
  expect_length(models, 1)

  expect_equal(deparse(getCall(models[[1]])$data), 'testLongData')
})


test_that('single method and data, cartesian=FALSE', {
  models = latrendBatch(mTest, testLongData, cartesian = FALSE)
  expect_is(models, 'lcModels')
  expect_length(models, 1)

  expect_equal(deparse(getCall(models[[1]])$data), 'testLongData')
})


test_that('multiple datasets', {
  methods = lcMethods(mTest, nClusters = 1:2)

  models = latrendBatch(
    methods,
    data = .(testLongData,
      testLongData[time < .5],
      testLongData[time >= .5]),
    cartesian = TRUE)

  expect_is(models, 'lcModels')
  expect_length(models, 2*3)

  expect_equal(deparse(getCall(models[[1]])$data), 'testLongData')
  expect_equal(deparse(getCall(models[[2]])$data), 'testLongData[time < 0.5]')
  expect_equal(deparse(getCall(models[[3]])$data), 'testLongData[time >= 0.5]')
})


test_that('single entry from a datasets list', {
  datasets = list(testLongData, testLongData)
  models = latrendBatch(mTest, data = datasets[[1]])
  expect_length(models, 1)
  expect_equal(deparse(getCall(models[[1]])$data), 'datasets[[1]]')
})


test_that('datasets list subset', {
  datasets = list(testLongData, testLongData, testLongData)
  models = latrendBatch(mTest, data = datasets[c(2,3)])
  expect_length(models, 2)
  expect_equal(deparse(getCall(models[[1]])$data), 'datasets[c(2, 3)][[1]]')
  expect_equal(deparse(getCall(models[[2]])$data), 'datasets[c(2, 3)][[2]]')
})


test_that('datasets list', {
  methods = lcMethods(mTest, nClusters = 1:2)
  dataList = list(
    testLongData,
    testLongData[time < .5],
    testLongData[time >= .5]
  )

  models = latrendBatch(methods, data = dataList, cartesian = TRUE)
  expect_is(models, 'lcModels')
  expect_length(models, 2*3)

  expect_equal(deparse(getCall(models[[1]])$data), 'dataList[[1]]')
  expect_equal(deparse(getCall(models[[2]])$data), 'dataList[[2]]')
  expect_equal(deparse(getCall(models[[3]])$data), 'dataList[[3]]')
})


test_that('datasets list, cartesian=FALSE', {
  methods = lcMethods(mTest, nClusters = 1:3)
  dataList = list(
    testLongData,
    testLongData[time < .5],
    testLongData[time >= .5]
  )
  models = latrendBatch(methods, data = dataList, cartesian = FALSE)

  expect_is(models, 'lcModels')
  expect_length(models, 3)

  expect_equal(deparse(getCall(models[[1]])$data), 'dataList[[1]]')
  expect_equal(deparse(getCall(models[[2]])$data), 'dataList[[2]]')
  expect_equal(deparse(getCall(models[[3]])$data), 'dataList[[3]]')
})


test_that('single method, multiple datasets', {
  dataList = list(
    testLongData,
    testLongData[time < .5],
    testLongData[time >= .5]
  )

  models = latrendBatch(mTest, data = dataList, cartesian = TRUE)

  expect_is(models, 'lcModels')
  expect_length(models, 3)

  expect_equal(deparse(getCall(models[[1]])$data), 'dataList[[1]]')
  expect_equal(deparse(getCall(models[[2]])$data), 'dataList[[2]]')
  expect_equal(deparse(getCall(models[[3]])$data), 'dataList[[3]]')
})


test_that('stop on error', {
  methods = list(mTest, mError)
  expect_error({
    latrendBatch(methods, data = testLongData)
  })
})

test_that('error removal', {
  methods = list(mTest, mError)
  models = latrendBatch(methods, data = testLongData, errorHandling = 'remove')

  expect_is(models, 'lcModels')
  expect_length(models, 1)
})


test_that('error passing', {
  methods = list(mTest, mError)
  expect_warning({
    models = latrendBatch(methods, data = testLongData, errorHandling = 'pass')
  })

  expect_is(models, 'list')
  expect_length(models, 2)
})

test_that('unique default seeds assigned to method specs', {
  methods = list(mRandom, mRandom)
  models = latrendBatch(methods, data = testLongData)
  seeds = lapply(models, getLcMethod) %>%
    vapply('[[', 'seed', FUN.VALUE = 0)

  expect_equal(uniqueN(seeds), length(methods))
})

test_that('method seeds are preserved in method specs', {
  methods = list(mTest, mRandom, update(mTest, seed = 3))
  models = latrendBatch(methods, data = testLongData)

  seeds = lapply(models, getLcMethod) %>%
    vapply('[[', 'seed', FUN.VALUE = 0)

  expect_equivalent(seeds[1], methods[[1]]$seed)
  expect_false(seeds[2] == seeds[1] || seeds[2] == seeds[3])
  expect_equivalent(seeds[3], methods[[3]]$seed)
})


test_that('model calls can be used to refit, with identical result', {
  models = latrendBatch(replicate(2, mRandom), data = testLongData)

  refits = list(
    eval(getCall(models[[1]])),
    eval(getCall(models[[2]]))
  )

  expect_is(refits[[1]], 'lcModel')
  expect_is(refits[[2]], 'lcModel')

  expect_equivalent(getLcMethod(models[[1]])$seed, getLcMethod(refits[[1]])$seed)
  expect_equivalent(getLcMethod(models[[2]])$seed, getLcMethod(refits[[2]])$seed)
})


test_that('repeated probabilistic method calls yield different results', {
  method = lcMethodTestRandom(alpha = 1, nClusters = 3)
  models = latrendBatch(replicate(2, method), data = testLongData)

  expect_true(!isTRUE(all.equal(trajectoryAssignments(models[[1]]), trajectoryAssignments(models[[2]]))))
})


test_that('original method seeds have effect', {
  method = lcMethodTestRandom(alpha = 1, nClusters = 3, seed = 2)

  refModel = latrend(method, data = testLongData)
  batchModels = latrendBatch(list(mTest, method), data = testLongData)

  expect_equivalent(trajectoryAssignments(refModel), trajectoryAssignments(batchModels[[2]]))
})
