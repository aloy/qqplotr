context('lcModels')

kml2 = m1 = latrend(lcMethodTestKML(nClusters=2), testLongData)
kml3 = latrend(lcMethodTestKML(nClusters=3), testLongData)
kml4 = latrend(lcMethodTestKML(nClusters=4), testLongData)
gmm = m2 = latrend(lcMethodTestLMKM(nClusters=2), testLongData)
models = lcModels(group=c(kml2, gmm), kml3, kml4)

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
  subset(lcModels(), .method == 'kml') %>%
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

  subset(models, .method == 'glmkm') %>%
    expect_is('lcModels') %>%
    expect_length(1)

  subset(models, .method == 'glmkm') %>%
    expect_length(1)

  subset(models, .method == 'kml') %>%
    expect_length(3)

  subset(models, .name == 'group1') %>%
    expect_length(1)

  subset(models, .method == 'kml' & nClusters > 2) %>%
    expect_length(2)

  subset(models, nClusters == 4, drop=TRUE) %>%
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
  min(models, 'WRSS') %>%
    expect_is('lcModel')
})

test_that('max', {
  max(models, 'WRSS') %>%
    expect_is('lcModel')
})

test_that('plotMetric', {
  plotMetric(models, name='BIC', subset=.method == 'kml') %>%
    expect_is('gg')

  plotMetric(models, name=c('logLik', 'BIC'), subset=.method == 'kml') %>%
    expect_is('gg')

  plotMetric(models, name=c('logLik', 'BIC'), by='nClusters', group=character()) %>%
    expect_is('gg')
})

test_that('externalMetric of list', {
  d = externalMetric(models, name = 'adjustedRand')
  expect_is(d, 'dist')
})

test_that('externalMetric against model', {
  externalMetric(models, models[[1]], name = 'adjustedRand') %>%
    expect_is('numeric') %>%
    expect_length(length(models))
})
