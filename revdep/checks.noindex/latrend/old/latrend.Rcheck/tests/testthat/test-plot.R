skip_if_not_installed('ggplot2')
library('ggplot2')
rngReset()

# Data.frame ####
test_that('plotTrajectories.data.frame', {
  p = plotTrajectories(testLongData, response = 'Value')
  expect_true(is.ggplot(p))
})

test_that('plotTrajectories.data.frame with expression', {
  p = plotTrajectories(testLongData, response = quote(exp(Value)))
  expect_true(is.ggplot(p))
})

test_that('plotTrajectories.data.frame without response', {
  expect_message({p = plotTrajectories(testLongData)}, regexp = 'Value')
  expect_true(is.ggplot(p))
})

test_that('plotClusterTrajectories.data.frame', {
  p = plotClusterTrajectories(testLongData, response = 'Value', cluster = 'Class')
  expect_true(is.ggplot(p))
})

test_that('plotClusterTrajectories.data.frame with labeler', {
  p = plotClusterTrajectories(
    testLongData, clusterLabeler = make.clusterPropLabels, response = 'Value', cluster = 'Class'
  )
  expect_true(is.ggplot(p))
})

test_that('plotClusterTrajectories.data.frame with labeler', {
  p = plotClusterTrajectories(
    testLongData, clusterLabeler = make.clusterSizeLabels, response = 'Value', cluster = 'Class'
  )
  expect_true(is.ggplot(p))
})

test_that('plotClusterTrajectories.data.frame with trajectories', {
  p = plotClusterTrajectories(testLongData, response = 'Value', cluster = 'Class', trajectories = TRUE)
  expect_true(is.ggplot(p))

  p = plotClusterTrajectories(testLongData, response = 'Value', cluster = 'Class', trajectories = 'TRUE')
  expect_true(is.ggplot(p))
})

test_that('plotClusterTrajectories.data.frame with trajectories = sd', {
  p = plotClusterTrajectories(testLongData, response = 'Value', cluster = 'Class', trajectories = 'sd')
  expect_true(is.ggplot(p))
})

test_that('plotClusterTrajectories.data.frame with trajectories = se', {
  p = plotClusterTrajectories(testLongData, response = 'Value', cluster = 'Class', trajectories = 'se')
  expect_true(is.ggplot(p))
})

test_that('plotClusterTrajectories.data.frame with trajectories = range', {
  p = plotClusterTrajectories(testLongData, response = 'Value', cluster = 'Class', trajectories = 'range')
  expect_true(is.ggplot(p))
})

test_that('plotClusterTrajectories.data.frame with trajectories = pct', {
  p = plotClusterTrajectories(testLongData, response = 'Value', cluster = 'Class', trajectories = '1pct')
  expect_true(is.ggplot(p))
  p = plotClusterTrajectories(testLongData, response = 'Value', cluster = 'Class', trajectories = '99pct')
  expect_true(is.ggplot(p))

  expect_error(
    plotClusterTrajectories(testLongData, response = 'Value', cluster = 'Class', trajectories = '100pct')
  )

  expect_error(
    plotClusterTrajectories(testLongData, response = 'Value', cluster = 'Class', trajectories = '-1pct')
  )
})

# Model-specific ####
test_that('plotTrajectories.lcModel', {
  p1 = plotTrajectories(testModel1)
  expect_true(is.ggplot(p1))

  p3 = plotTrajectories(testModel3)
  expect_true(is.ggplot(p3))
})


test_that('plotClusterTrajectories.lcModel', {
  p1 = plotClusterTrajectories(testModel1)
  expect_true(is.ggplot(p1))

  p3 = plotClusterTrajectories(testModel3)
  expect_true(is.ggplot(p3))
})


test_that('plotClusterTrajectories.lcModel with labeler', {
  p1 = plotClusterTrajectories(testModel1, clusterLabeler = make.clusterSizeLabels)
  expect_true(is.ggplot(p1))

  p3 = plotClusterTrajectories(testModel3, clusterLabeler = make.clusterSizeLabels)
  expect_true(is.ggplot(p3))
})


test_that('plotClusterTrajectories.lcModel with ordening', {
  p3 = plotClusterTrajectories(testModel3, clusterOrder = clusterNames(testModel3)[1:2])
  expect_true(is.ggplot(p3))
})


test_that('plot.lcModel', {
  p1 = plot(testModel1)
  expect_true(is.ggplot(p1))

  p3 = plot(testModel3)
  expect_true(is.ggplot(p3))
})


test_that('plot.lcModel with options', {
  p1 = plot(testModel1, linewidth = 1)
  expect_true(is.ggplot(p1))

  p3 = plot(testModel3, alpha = 1)
  expect_true(is.ggplot(p3))
})

