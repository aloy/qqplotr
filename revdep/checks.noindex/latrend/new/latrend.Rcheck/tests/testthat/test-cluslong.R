context('latrend')
rngReset()

test_that('default', {
  model = latrend(lcMethodTestLMKM(), data = testLongData)

  expect_is(model, 'lcModel')
  expect_equal(deparse(getCall(model)$data), 'testLongData')
  expect_equal(deparse(getCall(model)$envir), 'NULL')

  expect_error(latrend(), 'missing')
  expect_error(latrend(mTest), 'missing')
})

test_that('method var', {
  model = latrend(mTest, data = testLongData)
  expect_is(model, 'lcModel')
})

test_that('method name', {
  refMethod = lcMethodLMKM(formula = Value ~ Assessment, seed = 1)
  model = latrend('lcMethodLMKM', formula = Value ~ Assessment, seed = 1, data = testLongData)

  newMethod = getLcMethod(model)
  expect_equal(newMethod$seed, refMethod$seed)
})

test_that('overwritten argument', {
  model = latrend(lcMethodTestLMKM(), data = testLongData, nClusters = 1)

  expect_equal(nClusters(model), 1)
  expect_equal(getLcMethod(model)$nClusters, 1)
  expect_equal(getCall(model)$method$nClusters, 1)
})

test_that('method var with overwritten argument', {
  model = latrend(mTest, data = testLongData, nClusters = 1)

  expect_equal(nClusters(model), 1)
  expect_equal(getLcMethod(model)$nClusters, 1)
  expect_equal(getCall(model)$method$nClusters, 1)
})

test_that('new method arguments', {
  model = latrend(mTest, data = testLongData, test = 2)

  expect_equal(getLcMethod(model)$test, 2)
})

test_that('subset', {
  model = latrend(mTest, data = testLongData[Assessment < .5])

  expect_is(model, 'lcModel')
  expect_equal(deparse(getCall(model)$data), 'testLongData[Assessment < 0.5]')
})

test_that('data call', {
  model = latrend(mTest, data = as.data.table(testLongData))

  expect_is(model, 'lcModel')
  expect_equal(deparse(getCall(model)$data), 'as.data.table(testLongData)')
})

test_that('specify id and time with matrix input', {
  skip_if_not_installed('kml')

  mat = tsmatrix(testLongData, response = 'Value')
  model = latrend(lcMethodTestKML(), id = 'Device', time = 'Observation', data = mat)

  expect_is(model, 'lcModel')
  expect_equal(deparse(getCall(model)$data), 'mat')
})

test_that('envir', {
  method = lcMethodLMKM(nClusters = a, formula = Value ~ Assessment)
  env = list2env(list(a = 1))

  model = latrend(method, data = testLongData, envir = env)

  expect_is(model, 'lcModel')
  expect_equal(nClusters(model), 1)
})

test_that('data.frame input', {
  df = as.data.frame(testLongData)
  model = latrend(mTest, data = df)

  expect_is(model, 'lcModel')
})

test_that('matrix input', {
  mat = tsmatrix(testLongData, response = 'Value')
  model = latrend(mTest, data = mat)

  expect_is(model, 'lcModel')
})

test_that('custom id and time', {
  nameData = copy(testLongData) %>%
    setnames(c('Traj', 'Assessment'), c('Device', 'Observation'))
  method = lcMethodLMKM(Value ~ Observation, id = 'Device', time = 'Observation')
  model = latrend(method, data = nameData)

  expect_is(model, 'lcModel')
  expect_equal(deparse(getCall(model)$data), 'nameData')
})

test_that('id with NA', {
  set.seed(1)
  naData = copy(testLongData) %>%
    .[sample(.N, 10), Traj := NA]

  expect_error(latrend(mTest, data = naData))
})

test_that('factor id', {
  facData = copy(testLongData) %>%
    .[, Traj := factor(Traj)]

  model = latrend(mTest, data = facData)

  expect_is(model, 'lcModel')
  expect_equal(ids(model), levels(facData$Traj))
})

test_that('factor id, out of order', {
  facData = copy(testLongData) %>%
    .[, Traj := factor(Traj, levels = rev(unique(Traj)))]

  model = latrend(mTest, data = facData)

  expect_is(model, 'lcModel')
  expect_equal(ids(model), levels(facData$Traj))
})

test_that('factor id with empty levels', {
  facData = copy(testLongData) %>%
    .[, Traj := factor(Traj, levels = seq(0, uniqueN(Traj) + 1))]

  expect_warning({
    model = latrend(mTest, data = facData)
  }, regexp = 'mpty traj')

  expect_is(model, 'lcModel')
})

test_that('id with NA', {
  naData = copy(testLongData) %>%
    .[Traj == 1, Traj := NA]

  expect_error(latrend(mTest, data = naData))
})

test_that('shuffled data', {
  set.seed(1)
  shufData = copy(testLongData) %>%
    .[sample(.N)]

  model = latrend(mTest, data = shufData)

  expect_is(model, 'lcModel')
})

test_that('data with NA observations', {
  set.seed(1)
  naData = copy(testLongData) %>%
    .[sample(.N, 10), Value := NA]

  model = latrend(mTest, data = naData)

  expect_is(model, 'lcModel')
})

test_that('data with Inf observations', {
  set.seed(1)
  infData = copy(testLongData) %>%
    .[sample(.N, 10), Value := Inf]

  expect_error(latrend(mTest, data = infData))
})

test_that('running the same probabilistic method twice without seed yields different results', {
  method = lcMethodTestRandom(alpha = 1, nClusters = 2)
  model1 = latrend(method, data = testLongData)
  model2 = latrend(method, data = testLongData)

  expect_true(
    !isTRUE(
      all.equal(
        trajectoryAssignments(model1),
        trajectoryAssignments(model2)
      )
    )
  )
})


test_that('setting seed', {
  method = lcMethodTestRandom(alpha = 1, nClusters = 2)
  model1 = latrend(method, data = testLongData, seed = 1)
  model2 = latrend(method, data = testLongData, seed = 1)

  expect_equivalent(
    trajectoryAssignments(model1),
    trajectoryAssignments(model2)
  )
})


test_that('setting different seeds yields different result', {
  method = lcMethodTestRandom(alpha = 1, nClusters = 2)
  model1 = latrend(method, data = testLongData, seed = 1)
  model2 = latrend(method, data = testLongData, seed = 2)

  expect_true(getLcMethod(model1)$seed == 1)
  expect_true(getLcMethod(model2)$seed == 2)
  expect_true(!isTRUE(all.equal(trajectoryAssignments(model1), trajectoryAssignments(model2))))
})

test_that('trajectory length warning', {
  options(latrend.warnTrajectoryLength = 1e3)
  expect_warning(latrend(mTest, data = testLongData), regexp = 'warnTrajectoryLength')
  options(latrend.warnTrajectoryLength = 0)
})
