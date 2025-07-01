context('lcModel')
rngReset()

model = testModel

test_that('trajectories', {
  ref = model.data(model) %>%
    subset(select = c('id', 'time', 'Value', 'Cluster')) %>%
    as.data.table(key = 'id') %>%
    setnames('Cluster', 'cluster')

  pred = trajectories(model, cluster = 'cluster') %>%
    as.data.table()

  expect_equal(
    as.data.frame(ref),
    as.data.frame(pred)
  )
})


test_that('clusterTrajectories', {
  times = time(model)
  pred = clusterTrajectories(model)
  expect_equal(nrow(pred), length(times) * 2)
})

test_that('clusterTrajectories with invalid data format', {
  expect_error(clusterTrajectories(model, at = 'a'))
})

test_that('trajectoryAssignments', {
  trajClus = trajectoryAssignments(model)
  expect_is(trajClus, 'factor')
  expect_equal(nlevels(trajClus), nClusters(model))
})

test_that('make.trajectoryAssignments', {
  refFac = trajectoryAssignments(model)

  make.trajectoryAssignments(model, refFac) %>%
    expect_equal(refFac)

  make.trajectoryAssignments(model, as.integer(refFac)) %>%
    expect_equal(refFac)

  make.trajectoryAssignments(model, as.numeric(refFac)) %>%
    expect_equal(refFac)

  make.trajectoryAssignments(model, as.character(refFac)) %>%
    expect_equal(refFac)

  make.trajectoryAssignments(model, factor(refFac, levels=rev(levels(refFac)))) %>%
    expect_equal(refFac)

  expect_error(make.trajectoryAssignments(model, NULL))
  expect_error(make.trajectoryAssignments(model, Sys.time()))
})

test_that('make.clusterIndices', {
  refFac = trajectoryAssignments(model)
  refIdx = as.integer(refFac)

  make.clusterIndices(model, refFac) %>%
    expect_equal(refIdx)

  make.clusterIndices(model, as.integer(refFac)) %>%
    expect_equal(refIdx)

  make.clusterIndices(model, as.numeric(refFac)) %>%
    expect_equal(refIdx)

  make.clusterIndices(model, as.character(refFac)) %>%
    expect_equal(refIdx)

  make.clusterIndices(model, factor(refFac, levels=rev(levels(refFac)))) %>%
    expect_equal(refIdx)

  expect_error(make.clusterIndices(model, NULL))
  expect_error(make.clusterIndices(model, Sys.time()))
})

test_that('make.clusterNames', {
  opts = getOption('latrend.clusterNames', LETTERS)
  expect_gt(length(opts), 0)

  expect_length(make.clusterNames(1), 1)
  expect_length(make.clusterNames(4), 4)
  expect_is(make.clusterNames(4), 'character')
  expect_length(make.clusterNames(4), 4)

  options(latrend.clusterNames = function(n) LETTERS[1:n])
  expect_length(make.clusterNames(4), 4)

  options(latrend.clusterNames = character())
  expect_warning(make.clusterNames(2))

  options(latrend.clusterNames = opts)
  expect_warning(make.clusterNames(1e2))

  expect_error(make.clusterNames(0))
  expect_error(make.clusterNames(-1))
  expect_error(make.clusterNames(1.1))
  expect_error(make.clusterNames(NA))
})

test_that('metrics', {
  value = metric(model, 'BIC')
  expect_is(value, 'numeric')
  expect_named(value, 'BIC')

  expect_warning({
    value = metric(model, '@undefined')
  })
  expect_is(value, 'numeric')
  expect_named(value, '@undefined')
  expect_equal(value, c('@undefined'=NA*0))

  expect_warning({
    value = metric(model, c('AIC', '@undefined', 'BIC'))
  })
  expect_is(value, 'numeric')
  expect_named(value, c('AIC', '@undefined', 'BIC'))
  expect_equal(unname(value[2]), NA*0)
})

test_that('metrics (deps)', {
  skip_if_not_installed('clusterCrit')
  value = externalMetric(model, model, 'Jaccard')
  expect_is(value, 'numeric')
  expect_named(value, 'Jaccard')
})

test_that('update', {
  m = update(model, nClusters = 3)
  expect_is(m, 'lcModel')
  expect_equal(nClusters(m), 3)
})

test_that('clusterNames', {
  expect_equal(clusterNames(model), LETTERS[1:2])
})

test_that('clusterNames<-', {
  x = update(model, nClusters = 3)
  oldNames = LETTERS[1:3]
  newNames = c('Z', 'Y', 'X')
  expect_equal(clusterNames(x), oldNames)
  clusterNames(x) = newNames
  expect_equal(clusterNames(x), newNames)
})

test_that('consistency between predict() and predict(cluster)', {
  allPreds = predict(model, newdata = data.frame(time = c(0, 1)))
  dfPredA = predict(model, newdata = data.frame(time = c(0, 1), Cluster = 'A'))
  dfPredB = predict(model, newdata = data.frame(time = c(0, 1), Cluster = 'B'))

  expect_equal(allPreds$A$Fit, dfPredA$Fit)
  expect_equal(allPreds$B$Fit, dfPredB$Fit)
})

test_that('estimationTime', {
  expect_equivalent(estimationTime(model), model@estimationTime)
})

test_that('estimationTime in days', {
  expect_equivalent(estimationTime(model, unit = 'days'), estimationTime(model) / 86400)
})

test_that('confusionMatrix', {
  cfMat = confusionMatrix(model)
  expect_is(cfMat, 'matrix')
  expect_true(is_valid_postprob(cfMat))
  expect_true(is_valid_postprob(confusionMatrix(model, strategy = NULL)))
})

test_that('APPA', {
  appa = APPA(model)
  expect_is(appa, 'numeric')
  expect_length(appa, nClusters(model))
  expect_true(all(appa >= 0))
  expect_true(all(appa <= 1))
})

test_that('OCC', {
  occ = OCC(model)
  expect_is(occ, 'numeric')
  expect_length(occ, nClusters(model))
  expect_true(all(occ >= 0))
})

test_that('plotFittedTrajectories', {
  skip_if_not_installed('ggplot2')
  expect_is(plotFittedTrajectories(model), 'ggplot')
})

test_that('predictForCluster', {
  predA = predictForCluster(model, cluster = 'A')
  expect_is(predA, 'numeric')
  expect_length(predA, nobs(model))

  predB = predictForCluster(model, cluster = 'B')
  expect_is(predB, 'numeric')
  expect_length(predB, nobs(model))

  expect_error(predictForCluster(model, cluster = '.'))
})

test_that('predictForCluster with newdata', {
  newdata = data.frame(time = 0)
  predA = predictForCluster(model, cluster = 'A', newdata = newdata)
  expect_is(predA, 'numeric')
  expect_length(predA, nrow(newdata))

  expect_error(predictForCluster(model, cluster = 'A', newdata = data.frame(Zzz = 0)), timeVariable(model))
  suppressWarnings(expect_error(predictForCluster(model, cluster = 'A', newdata = data.frame())))
})

test_that('predictForCluster with newdata and Cluster column', {
  newdata = data.table(time = 0)
  newdataClus = copy(newdata)[, Cluster := 'B']

  refpred = predictForCluster(model, cluster = 'A', newdata = newdata)
  options(latrend.warnNewDataClusterColumn = TRUE)
  expect_warning(pred <- predictForCluster(model, cluster = 'A', newdata = newdataClus))
  options(latrend.warnNewDataClusterColumn = FALSE)
  expect_equal(pred, refpred)
})
