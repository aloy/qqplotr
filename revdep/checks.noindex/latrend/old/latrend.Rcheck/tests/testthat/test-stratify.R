context('stratify')
rngReset()

test_that('expression', {
  m = lcMethodTestStratify(stratify = mean(Value) > 0)
  model = latrend(m, data = testLongData)
  expect_valid_lcModel(model)

  expect_equal(nClusters(model), 2)
  clusTrajs = clusterTrajectories(model) %>% as.data.table()
  expect_lt(clusTrajs[Cluster == 'A', mean(Value)], 0)
  expect_gt(clusTrajs[Cluster == 'B', mean(Value)], 0)
})

test_that('function', {
  stratfun = function(data) mean(data$Value) < 0
  m = lcMethodTestStratify(stratify = stratfun)

  model = latrend(m, data = testLongData)
  expect_valid_lcModel(model)
  expect_equal(nClusters(model), 2)
  clusTrajs = clusterTrajectories(model) %>% as.data.table()
  expect_gt(clusTrajs[Cluster == 'A', mean(Value)], 0)
  expect_lt(clusTrajs[Cluster == 'B', mean(Value)], 0)
})

test_that('in-line function', {
  m = lcMethodTestStratify(stratify = function(data) mean(data$Value) > 0)

  model = latrend(m, data = testLongData)
  expect_valid_lcModel(model)
  expect_equal(nClusters(model), 2)
})

test_that('single logical cluster assignment', {
  stratfun = function(data) FALSE
  m = lcMethodTestStratify(stratify = stratfun)

  model = latrend(m, data = testLongData)
  expect_valid_lcModel(model)
  expect_equal(nClusters(model), 1)
})

test_that('single numeric cluster assignment', {
  stratfun = function(data) 1
  m = lcMethodTestStratify(stratify = stratfun)

  model = latrend(m, data = testLongData)
  expect_valid_lcModel(model)
  expect_equal(nClusters(model), 1)
})

test_that('logical cluster assignment with empty cluster', {
  stratfun = function(data) TRUE
  m = lcMethodTestStratify(stratify = stratfun)
  expect_warning({model = latrend(m, data = testLongData)})
  expect_equal(nClusters(model), 2)
  expect_warning({clusterTrajectories(model)}, regexp = '"A"')
  suppressWarnings({
    expect_valid_lcModel(model)
  })
})

test_that('multiple cluster factor expression', {
  m = lcMethodTestStratify(
    stratify = cut(mean(Value), c(-Inf, -.5, 0, Inf), labels = c('C', 'B', 'A'))
  )
  expect_warning({
    model = latrend(m, data = testLongData)
  })
  expect_equal(nClusters(model), 3)
  expect_equivalent(clusterNames(model), c('C', 'B', 'A'))
  suppressWarnings({
    expect_valid_lcModel(model)
  })
})

test_that('multiple cluster numeric expression', {
  skip_on_cran()
  m = lcMethodTestStratify(stratify = as.numeric(cut(mean(Value), c(-Inf, -.5, 0, Inf))))
  expect_warning({
    model = latrend(m, data = testLongData)
  })
  expect_equal(nClusters(model), 3)
  expect_equivalent(clusterNames(model), c('A', 'B', 'C'))
})

test_that('multiple cluster expression with cluster names', {
  skip_on_cran()
  m = lcMethodTestStratify(
    stratify = as.numeric(cut(mean(Value), c(-Inf, -.5, 0, Inf))),
    clusterNames = LETTERS[3:1]
  )
  expect_warning({
    model = latrend(m, data = testLongData)
  })
  expect_equal(nClusters(model), 3)
  expect_equivalent(clusterNames(model), c('C', 'B', 'A'))
})
