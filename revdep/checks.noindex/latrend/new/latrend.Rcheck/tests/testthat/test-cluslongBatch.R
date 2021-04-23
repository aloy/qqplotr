context('latrendBatch')
rngReset()

test_that('single method and data, cartesian=TRUE', {
  models = latrendBatch(lcMethodTestKML(), testLongData, cartesian=TRUE) %>%
    expect_is('lcModels') %>%
    expect_length(1)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData')}
})

test_that('single method and data, cartesian=FALSE', {
  models = latrendBatch(lcMethodTestKML(), testLongData, cartesian=FALSE) %>%
    expect_is('lcModels') %>%
    expect_length(1)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData')}
})

test_that('multiple datasets', {
  methods = lcMethods(lcMethodTestKML(), nClusters=1:2)

  models = latrendBatch(methods, data=.(testLongData, testLongData[Assessment < .5], testLongData[Assessment >= .5]), cartesian=TRUE) %>%
    expect_is('lcModels') %>%
    expect_length(2*3)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData')}
  getCall(models[[2]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData[Assessment < 0.5]')}
  getCall(models[[3]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData[Assessment >= 0.5]')}
})

test_that('datasets list', {
  methods = lcMethods(lcMethodTestKML(), nClusters=1:2)
  dataList = list(testLongData, testLongData[Assessment < .5], testLongData[Assessment >= .5])

  models = latrendBatch(methods, data=dataList, cartesian=TRUE) %>%
    expect_is('lcModels') %>%
    expect_length(2*3)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[1]]')}
  getCall(models[[2]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[2]]')}
  getCall(models[[3]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[3]]')}
})

test_that('datasets list, cartesian=FALSE', {
  methods = lcMethods(lcMethodTestKML(), nClusters=1:3)
  dataList = list(testLongData, testLongData[Assessment < .5], testLongData[Assessment >= .5])
  models = latrendBatch(methods, data=dataList, cartesian=FALSE) %>%
    expect_is('lcModels') %>%
    expect_length(3)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[1]]')}
  getCall(models[[2]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[2]]')}
  getCall(models[[3]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[3]]')}
})

test_that('single method, multiple datasets', {
  dataList = list(testLongData, testLongData[Assessment < .5], testLongData[Assessment >= .5])

  models = latrendBatch(lcMethodTestKML(), data=dataList, cartesian=TRUE) %>%
    expect_is('lcModels') %>%
    expect_length(3)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[1]]')}
  getCall(models[[2]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[2]]')}
  getCall(models[[3]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[3]]')}
})
