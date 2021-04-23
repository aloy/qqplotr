context('lcMethods')

test_that('as.lcMethods non-list', {
  out = as.lcMethods(lcMethodTestKML())
  expect_is(out, 'lcMethods')
  expect_length(out, 1)
})

test_that('as.lcMethods', {
  methods = list(lcMethodTestLMKM(), lcMethodTestKML())
  out = as.lcMethods(methods)
  expect_is(out, 'lcMethods')
  expect_length(out, 2)
})

test_that('as.data.frame single-row', {
  methods = as.lcMethods(lcMethodTestKML())
  df = as.data.frame(methods)
  expect_is(df, 'data.frame')
  expect_equal(nrow(df), 1)
})

test_that('as.data.frame multi-row', {
  methods = as.lcMethods(list(lcMethodTestKML(nClusters = 2), lcMethodTestKML(nClusters = 3)))
  df = as.data.frame(methods)
  expect_is(df, 'data.frame')
  expect_equal(nrow(df), 2)
  expect_equal(df$nClusters, 2:3)
})

test_that('as.data.frame multi-row different classes', {
  methods = as.lcMethods(list(lcMethodTestLMKM(), lcMethodTestKML()))
  df = as.data.frame(methods)
  expect_is(df, 'data.frame')
  expect_equal(nrow(df), 2)
})

test_that('vector argument', {
  methods = lcMethods(lcMethodTestKML(), nClusters=1:3)
  expect_is(methods, 'list')
  expect_length(methods, 3)
})

test_that('default', {
  methods = lcMethods(lcMethodTestKML())
  expect_is(methods, 'list')
  expect_length(methods, 1)
})

test_that('var', {
  kml = lcMethodTestKML()
  methods = lcMethods(kml)
  expect_is(methods, 'list')
  expect_length(methods, 1)
})


test_that('scalar argument', {
  methods = lcMethods(lcMethodTestKML(), nClusters=2)
  expect_is(methods, 'list')
  expect_length(methods, 1)
  expect_equal(methods[[1]]$nClusters, 2)
})


test_that('char argument', {
  methods = lcMethods(lcMethodTestKML(), nClusters=2, test = 'a')
  expect_is(methods, 'list')
  expect_length(methods, 1)
  expect_equal(methods[[1]]$nClusters, 2)
  expect_equal(methods[[1]]$test, 'a')
})


test_that('var with vector argument', {
  kml = lcMethodTestKML()
  methods = lcMethods(kml, nClusters=1:3)
  expect_is(methods, 'list')
  expect_length(methods, 3)
})

test_that('.() argument', {
  a = 1
  b = 2
  methods = lcMethods(lcMethodTestKML(), nClusters=.(a,b))
  expect_is(methods, 'list')
  expect_length(methods, 2)

  expect_equal(deparse(methods[[1]][['nClusters', eval=FALSE]]), 'a')
  expect_equal(deparse(methods[[2]][['nClusters', eval=FALSE]]), 'b')
})


test_that('cartesian', {
  a = 1
  b = 2
  methods = lcMethods(lcMethodTestKML(), maxIt = 10, seed = .(a, b), nClusters=1:3)
  expect_is(methods, 'list')
  expect_length(methods, 1 * 2 * 3)

  df = as.data.frame(methods)
  expect_true(all(df$maxIt == 10))
  expect_equal(df$seed, rep(c('a', 'b'), each = 3))
  expect_equal(df$nClusters, rep(1:3, 2))
})

test_that('do.call', {
  m = lcMethodTestKML() %>% evaluate()
  methods = do.call(lcMethods, list(m, maxIt = 10, seed = 1:3, nClusters=2))
  expect_is(methods, 'list')
  expect_length(methods, 1 * 3 * 1)
})

test_that('unnamed argument', {
  expect_error({
    lcMethods(lcMethodTestKML(), 2)
  })
})
