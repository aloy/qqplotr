context('lcModel')
rngReset()

model = latrend(lcMethodTestKML(), data=testLongData)

test_that('default predict', {
  suppressWarnings({
    setClass('lcModelTest', contains='lcModel')
    testModel = model
    class(testModel) = 'lcModelTest'
    dfpred = predict(testModel, newdata=data.frame(Assessment=1))
    expect_is(dfpred, 'list')
    expect_is(dfpred$A$Fit, 'numeric')
    expect_equivalent(nrow(dfpred$A), 1)
  })
})

test_that('default fitted', {
  suppressWarnings({
    setClass('lcModelTest', contains='lcModel')
    testModel = model
    class(testModel) = 'lcModelTest'
    expect_is(fitted(testModel), 'numeric')
  })
})

test_that('trajectoryAssignments', {
  trajectoryAssignments(model) %>%
    expect_is('factor') %T>%
    {expect_equal(nlevels(.), nClusters(model))}
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
})

test_that('metrics', {
  expect_length(metric(model, character()), 0)

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
  allPreds = predict(model, newdata = data.frame(Assessment = c(0, 1)))
  dfPredA = predict(model, newdata = data.frame(Assessment = c(0, 1), Cluster = 'A'))
  dfPredB = predict(model, newdata = data.frame(Assessment = c(0, 1), Cluster = 'B'))

  expect_equal(allPreds$A$Fit, dfPredA$Fit)
  expect_equal(allPreds$B$Fit, dfPredB$Fit)
})
