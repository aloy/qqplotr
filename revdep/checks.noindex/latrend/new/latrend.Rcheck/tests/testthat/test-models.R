context('lcModels')
m1 = testModel2
m2 = rngModel2
models = lcModels(group = c(testModel2, testModel3), rngModel2, testModel4)

test_that('as', {
  as.lcModels(NULL) %>%
    expect_is('lcModels') %>%
    expect_length(0)

  as.lcModels(m1) %>%
    expect_is('lcModels') %>%
    expect_length(1)

  as.lcModels(c(m1, m2)) %>%
    expect_is('lcModels') %>%
    expect_length(2)

  as.lcModels(c(A=m1, B=m2)) %>%
    expect_length(2) %>%
    expect_named()

  expect_error(as.lcModels(1:2))
})

test_that('create', {
  lcModels() %>%
    expect_is('lcModels') %>%
    expect_length(0)

  lcModels(m1) %>%
    expect_is('lcModels') %>%
    expect_length(1)

  lcModels(a=m1) %>%
    expect_is('lcModels') %>%
    expect_length(1) %>%
    expect_named('a')

  lcModels(a=m1, b=m2) %>%
    expect_is('lcModels') %>%
    expect_length(2) %>%
    expect_named(c('a', 'b'))

  lcModels(c(a=m1, b=m2)) %>%
    expect_is('lcModels') %>%
    expect_length(2) %>%
    expect_named(c('a', 'b'))

  lcModels(a=c(a=m1, b=m2)) %>%
    expect_is('lcModels') %>%
    expect_length(2) %>%
    expect_named(c('a.a', 'a.b'))

  lcModels(a=c(a=m1, b=m2), b=m1) %>%
    expect_is('lcModels') %>%
    expect_length(3) %>%
    expect_named(c('a.a', 'a.b', 'b'))
})

test_that('as.data.frame', {
  lcModels() %>%
    as.data.frame() %>%
    expect_named(c('.name', '.method', 'data')) %T>%
    {expect_equal(nrow(.), 0)}

  lcModels(m1) %>%
    as.data.frame() %>%
    expect_length(length(getLcMethod(m1)) + 3) %>%
    expect_named(c('.name', '.method', 'data', names(getLcMethod(m1))))

  lcModels(m1) %>%
    as.data.frame() %>%
    expect_length(length(getLcMethod(m1)) + 3) %>%
    expect_named(c('.name', '.method', 'data', names(getLcMethod(m1))))
})

test_that('subset', {
  subset(lcModels(), .method == 'lmkm') %>%
    expect_is('lcModels') %>%
    expect_length(0)

  subset(models, nClusters > 2) %>%
    expect_is('lcModels') %>%
    expect_length(2)

  subset(models, nClusters > Inf) %>%
    expect_is('lcModels') %>%
    expect_length(0)

  subset(models, ) %>%
    expect_is('lcModels') %>%
    expect_length(4)

  subset(models, .method == 'part') %>% #TODO should be .method = 'random'
    expect_is('lcModels') %>%
    expect_length(1)

  subset(models, .method == 'part') %>% #TODO should be .method = 'random'
    expect_length(1)

  subset(models, .method == 'lmkm') %>%
    expect_length(3)

  subset(models, .name == 'group1') %>%
    expect_length(1)

  subset(models, .method == 'lmkm' & nClusters > 2) %>%
    expect_length(2)

  subset(models, nClusters == 4, drop = TRUE) %>%
    expect_is('lcModel')
})

test_that('single metric', {
  metric(lcModels(), 'BIC') %>%
    expect_is('numeric') %>%
    expect_length(0)

  metric(models, 'BIC') %>%
    expect_is('numeric')

  x = metric(models, 'BIC', drop = FALSE)
  expect_is(x, 'data.frame')
  expect_true(length(x) == 1)
  expect_true(names(x) == 'BIC')
})

test_that('single metric for empty list', {
  dfOut = metric(list(), 'BIC', drop = FALSE)
  expect_is(dfOut, 'data.frame')
  expect_true(nrow(dfOut) == 0)
})

test_that('multiple metrics', {
  metric(lcModels(), c('AIC', 'BIC')) %>%
    expect_is('numeric') %>%
    expect_length(0)

  x = metric(models, c('AIC', 'BIC'))
  expect_is(x, 'data.frame')
  expect_equivalent(names(x), c('AIC', 'BIC'))

  as.list(models) %>%
    metric(c('AIC', 'BIC')) %>%
    expect_is('data.frame')
})

test_that('min', {
  expect_is(min(models, 'WRSS'), 'lcModel')
})

test_that('min for empty list', {
  expect_error(min(as.lcModels(NULL), 'WRSS'))
})

test_that('max', {
  expect_is(max(models, 'WRSS'), 'lcModel')
})

test_that('max for empty list', {
  expect_error(max(as.lcModels(NULL), 'WRSS'))
})

test_that('plot', {
  skip_if_not_installed('ggplot2')

  p = plot(models)
  expect_is(p, 'grob')
})

test_that('plot empty list', {
  skip_if_not_installed('ggplot2')
  expect_warning({
    p = plot(as.lcModels(list()))
  })
})

test_that('plot subset', {
  skip_if_not_installed('ggplot2')

  p = plot(models, subset = nClusters == 2)
  expect_is(p, 'grob')
})

test_that('plot subset with no results', {
  skip_if_not_installed('ggplot2')

  expect_warning({
    p = plot(models, subset = nClusters < 1)
  })
})

test_that('plotMetric', {
  skip_if_not_installed('ggplot2')

  plotMetric(models, name = 'BIC', subset = .method == 'lmkm') %>%
    expect_is('gg')

  plotMetric(models, name = c('logLik', 'BIC'), subset = .method == 'lmkm') %>%
    expect_is('gg')

  plotMetric(models, name = c('logLik', 'BIC'), by = 'nClusters', group = character()) %>%
    expect_is('gg')

  plotMetric(models, name = c('WMAE', 'RMSE', 'BIC'), by = 'nClusters', group = '.method') %>%
    expect_is('gg')
})

test_that('pairwise externalMetric of list', {
  skip_if_not_installed('mclustcomp')

  d = externalMetric(models, 'adjustedRand')
  expect_is(d, 'dist')
})

test_that('externalMetric against model', {
  skip_if_not_installed('mclustcomp')

  numOut = externalMetric(models, models[[1]], name = 'adjustedRand')
  expect_is(numOut, 'numeric')
  expect_length(numOut, length(models))

  dfOut = externalMetric(models, models[[1]], name = 'adjustedRand', drop = FALSE)
  expect_is(dfOut, 'data.frame')
  expect_true(nrow(dfOut) == length(models))
})

test_that('externalMetric single model against model', {
  skip_if_not_installed('mclustcomp')

  numOut = externalMetric(models[1], models[[1]], name = 'adjustedRand', drop = TRUE)
  expect_is(numOut, 'numeric')
  expect_length(numOut, 1)

  dfOut = externalMetric(models[1], models[[1]], name = 'adjustedRand', drop = FALSE)
  expect_is(dfOut, 'data.frame')
  expect_true(nrow(dfOut) == 1)
})

test_that('externalMetric empty list against model', {
  skip_if_not_installed('mclustcomp')

  numOut = externalMetric(list(), models[[1]], name = 'adjustedRand', drop = TRUE)
  expect_is(numOut, 'numeric')
  expect_length(numOut, 0)

  dfOut = externalMetric(list(), models[[1]], name = 'adjustedRand', drop = FALSE)
  expect_is(dfOut, 'data.frame')
  expect_true(nrow(dfOut) == 0)
})

test_that('estimationTime', {
  secs = estimationTime(models)
  expect_is(secs, 'numeric')
  expect_length(secs, 1)

  secs2 = estimationTime(as.list(models))
  expect_equal(secs, secs2)
})

test_that('estimationTime with unit argument', {
  secs = estimationTime(models)
  mins = estimationTime(models, unit = 'mins')
  days = estimationTime(models, unit = 'days')

  expect_equal(secs / 60, mins, tolerance = .01)
  expect_equal(mins / 60 / 24, days, tolerance = .01)
})

test_that('print', {
  expect_output(print(models, summary = FALSE))
})

test_that('print summary', {
  expect_output(print(models, summary = TRUE))
})
