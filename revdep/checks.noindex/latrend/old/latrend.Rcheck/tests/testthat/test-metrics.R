context('metrics')
skip_if_not_installed('mclust')
skip_if_not_installed('psych')
skip_if_not_installed('igraph')
rngReset()

internalMetrics = getInternalMetricNames() %>%
  setdiff(c('deviance', 'SED.fit', 'ED.fit'))

test_that('two clusters', {
  for (name in internalMetrics) {
    value = metric(testModel2, name = name)
    expect_is(value, 'numeric')
    expect_true(is.finite(value), info = name)
  }
})

test_that('single cluster', {
  for (name in internalMetrics) {
    value = metric(testModel1, name = name)
    expect_is(value, 'numeric')
    expect_length(value, 1)
  }
})

test_that('error on empty name', {
  expect_error(metric(testModel1, name = NULL))
})

test_that('default names', {
  # if this test fails then the documentation needs to be updated
  out = metric(testModel2)
  expect_named(out, c('WRSS', 'APPA.mean'))
})

test_that('MAE', {
  out = metric(testModel2, 'MAE')

  expect_gt(out, 0)
  expect_equivalent(out, mean(abs(residuals(testModel2, clusters = trajectoryAssignments(testModel2)))))
})

test_that('MSE', {
  out = metric(testModel2, 'MSE')

  expect_gt(out, 0)
  expect_equivalent(out, mean(residuals(testModel2, clusters = trajectoryAssignments(testModel2))^2))
})

test_that('WMAE', {
  wmae = metric(testModel2, 'WMAE')
  mae = metric(testModel2, 'MAE')

  # for kml, WMAE and MAE should yield same result due to modal assignment
  expect_equivalent(wmae, mae)

  wmaeFuzzy = metric(testModel2, 'WMAE') # TODO use fuzzy model
  maeFuzzy = metric(testModel2, 'MAE')

  expect_gte(wmaeFuzzy, maeFuzzy)
})

test_that('Mahalanobis', {
  expect_true('Mahalanobis' %in% getInternalMetricNames())
})

test_that('missing metric', {
  expect_warning(met <- metric(testModel2, '.MISSING'))
  expect_true(is.na(met))
})

test_that('metric definition', {
  expect_true(is.function(getInternalMetricDefinition('MAE')))
})

test_that('missing metric definition', {
  expect_error(getInternalMetricDefinition('.MISSING'))
})

test_that('define metric', {
  defineInternalMetric('.NEW', force)
  expect_warning(defineInternalMetric('.NEW', force))

  expect_equal(getInternalMetricDefinition('.NEW'), force)
})

test_that('plot single model', {
  expect_is(
    plotMetric(testModel, 'WMAE'),
    'gg'
  )
})

test_that('plot single model, multiple metrics', {
  expect_is(
    plotMetric(testModel, c('WMAE', 'RMSE')),
    'gg'
  )
})
