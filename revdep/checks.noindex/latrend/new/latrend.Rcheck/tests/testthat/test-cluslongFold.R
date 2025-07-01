context('latrendFold')
skip_if_not_installed('caret')
rngReset()

test_that('2 folds', {
  expect_silent({
    models = latrendCV(mRandom, testLongData, folds = 2, seed = 1)
  })

  expect_is(models, 'lcModels')
  expect_length(models, 2)
})

test_that('3 folds', {
  models = latrendCV(mRandom, testLongData, folds = 3, seed = 1)

  expect_is(models, 'lcModels')
  expect_length(models, 3)

  expect_equal(deparse(getCall(models[[1]])$data), 'trainFold(testLongData, fold = 1, "id", 3, 1)')
  expect_equal(deparse(getCall(models[[2]])$data), 'trainFold(testLongData, fold = 2, "id", 3, 1)')
  expect_equal(deparse(getCall(models[[3]])$data), 'trainFold(testLongData, fold = 3, "id", 3, 1)')
})

test_that('without seed', {
  models = latrendCV(mRandom, testLongData, folds = 2)

  expect_is(models, 'lcModels')
  expect_length(models, 2)
})

test_that('data subset', {
  models = latrendCV(mRandom, testLongData[time < .5], folds = 2, seed = 1)

  expect_is(models, 'lcModels')
  expect_length(models, 2)

  expect_equal(
    deparse(getCall(models[[1]])$data, width.cutoff = 500),
    'trainFold(testLongData[time < 0.5], fold = 1, "id", 2, 1)'
  )
  expect_equal(
    deparse(getCall(models[[2]])$data, width.cutoff = 500),
    'trainFold(testLongData[time < 0.5], fold = 2, "id", 2, 1)'
  )
})

test_that('method var', {
  models = latrendCV(mRandom, testLongData, folds = 2, seed = 1)

  expect_is(models, 'lcModels')
  expect_length(models, 2)
})

test_that('createTrainDataFolds', {
  trainDataList = createTrainDataFolds(testLongData, folds = 2, seed = 1)
  expect_is(trainDataList, 'list')
  expect_length(trainDataList, 2)
  expect_is(trainDataList[[1]], 'data.frame')
  expect_is(trainDataList[[2]], 'data.frame')
  expect_length(
    union(unique(trainDataList[[1]]$id), unique(trainDataList[[2]]$id)),
    uniqueN(testLongData$id)
  )
})

test_that('createTestDataFold', {
  trainDataList = createTrainDataFolds(testLongData, folds = 2, seed = 1)
  testData = createTestDataFold(testLongData, trainDataList[[1]])
  expect_length(intersect(unique(testData$id), unique(trainDataList[[1]]$id)), 0)
})

test_that('createTestDataFolds', {
  trainDataList = createTrainDataFolds(testLongData, folds = 2, seed = 1)
  testDataList = createTestDataFolds(testLongData, trainDataList)
  expect_is(testDataList, 'list')
  expect_length(testDataList, length(trainDataList))

  for(i in seq_along(testDataList)) {
    expect_is(testDataList[[i]], 'data.frame')
    expect_length(intersect(unique(testDataList[[i]]$id), unique(trainDataList[[i]]$id)), 0)
  }
})
