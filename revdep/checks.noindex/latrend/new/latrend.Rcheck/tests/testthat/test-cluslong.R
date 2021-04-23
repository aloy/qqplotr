context('latrend')
rngReset()

test_that('default', {
  model = latrend(lcMethodTestKML(), data=testLongData) %>%
    expect_is('lcModel')

  expect_equal(deparse(getCall(model)$data), 'testLongData')
  expect_equal(deparse(getCall(model)$envir), 'NULL')
})

test_that('method var', {
  kml = lcMethodTestKML()
  latrend(kml, data=testLongData) %>%
    expect_is('lcModel')
})

test_that('overwritten argument', {
  model = latrend(lcMethodTestKML(), data=testLongData, nClusters=1)

  expect_equal(nClusters(model), 1)
  expect_equal(getLcMethod(model)$nClusters, 1)
  expect_equal(getCall(model)$method$nClusters, 1)
})

test_that('method var with overwritten argument', {
  kml = lcMethodTestKML()
  model = latrend(kml, data=testLongData, nClusters=1)

  expect_equal(nClusters(model), 1)
  expect_equal(getLcMethod(model)$nClusters, 1)
  expect_equal(getCall(model)$method$nClusters, 1)
})

test_that('new method arguments', {
  model = latrend(lcMethodTestKML(), data=testLongData, test=2)

  expect_equal(getLcMethod(model)$test, 2)
})

test_that('subset', {
  model = latrend(lcMethodTestKML(), data=testLongData[Assessment < .5]) %>%
    expect_is('lcModel')

  expect_equal(deparse(getCall(model)$data), 'testLongData[Assessment < 0.5]')
})

test_that('data call', {
  model = latrend(lcMethodTestKML(), data=as.data.table(testLongData)) %>%
    expect_is('lcModel')

  expect_equal(deparse(getCall(model)$data), 'as.data.table(testLongData)')
})

test_that('specify id and time with matrix input', {
  mat = dcastRepeatedMeasures(testLongData, response = 'Value')
  model = latrend(lcMethodTestKML(), id='Device', time='Observation', data=mat) %>%
    expect_is('lcModel')

  expect_equal(deparse(getCall(model)$data), 'mat')
})

test_that('envir', {
  kml = lcMethodKML(nClusters=a, response = 'Value', nbRedrawing=1, maxIt=10)
  e = list2env(list(a = 1))

  model = latrend(kml, data=testLongData, envir=e) %>%
    expect_is('lcModel')

  expect_equal(nClusters(model), 1)
})

test_that('data.frame input', {
  df = as.data.frame(testLongData)
  model = latrend(lcMethodTestKML(), data=df) %>%
    expect_is('lcModel')
})

test_that('matrix input', {
  mat = dcastRepeatedMeasures(testLongData, response = 'Value')
  model = latrend(lcMethodTestKML(), data=mat) %>%
    expect_is('lcModel')
})

test_that('custom id and time', {
  nameData = copy(testLongData) %>%
    setnames(c('Traj', 'Assessment'), c('Device', 'Observation'))
  model = latrend(lcMethodTestKML(), id='Device', time='Observation', data=nameData) %>%
    expect_is('lcModel')

  expect_equal(deparse(getCall(model)$data), 'nameData')
})

test_that('id with NA', {
  set.seed(1)
  naData = copy(testLongData) %>%
    .[sample(.N, 10), Traj := NA]

  expect_error(latrend(lcMethodTestKML(), data=naData))
})

test_that('factor id', {
  facData = copy(testLongData) %>%
    .[, Traj := factor(Traj)]

  model = latrend(lcMethodTestKML(), data=facData) %>%
    expect_is('lcModel')

  expect_equal(ids(model), levels(facData$Traj))
})

test_that('factor id, out of order', {
  facData = copy(testLongData) %>%
    .[, Traj := factor(Traj, levels=rev(unique(Traj)))]

  model = latrend(lcMethodTestKML(), data=facData) %>%
    expect_is('lcModel')

  expect_equal(ids(model), levels(facData$Traj))
})

test_that('factor id with empty levels', {
  facData = copy(testLongData) %>%
    .[, Traj := factor(Traj, levels=seq(0, uniqueN(Traj) + 1))]

  model = latrend(lcMethodTestKML(), data=facData) %>%
    expect_is('lcModel')
})

test_that('id with NA', {
  naData = copy(testLongData) %>%
    .[Traj == 1, Traj := NA]

  expect_error(latrend(lcMethodTestKML(), data=naData))
})

test_that('shuffled data', {
  set.seed(1)
  shufData = copy(testLongData) %>%
    .[sample(.N)]

  latrend(lcMethodTestKML(), data=shufData) %>%
    expect_is('lcModel')
})

test_that('data with NA observations', {
  set.seed(1)
  naData = copy(testLongData) %>%
    .[sample(.N, 10), Value := NA]

  latrend(lcMethodTestKML(), data=naData) %>%
    expect_is('lcModel')
})

test_that('data with Inf observations', {
  set.seed(1)
  infData = copy(testLongData) %>%
    .[sample(.N, 10), Value := Inf]

  expect_error(latrend(lcMethodTestKML(), data=infData))
})

test_that('data with missing observations', {
  set.seed(1)
  naData = copy(testLongData) %>%
    .[sample(.N, 100)]

  latrend(lcMethodTestGCKM(), data=naData) %>%
    expect_is('lcModel')

  expect_error(latrend(lcMethodTestKML(), data=naData))
})

