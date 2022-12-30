context('plot')
skip_if_not_installed('ggplot2')
library('ggplot2')
rngReset()

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
