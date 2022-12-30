context('lcMethods')

test_that('as.lcMethods non-list', {
  out = as.lcMethods(mTest)
  expect_is(out, 'lcMethods')
  expect_length(out, 1)
})

test_that('as.lcMethods', {
  methods = list(mTest, mRandom)
  out = as.lcMethods(methods)
  expect_is(out, 'lcMethods')
  expect_length(out, 2)
})

test_that('as.data.frame single-row', {
  methods = as.lcMethods(mTest)
  df = as.data.frame(methods)
  expect_is(df, 'data.frame')
  expect_equal(nrow(df), 1)
})

test_that('as.data.frame multi-row', {
  methods = as.lcMethods(list(mTest2, mTest3))
  df = as.data.frame(methods)
  expect_is(df, 'data.frame')
  expect_equal(nrow(df), 2)
  expect_equal(df$nClusters, 2:3)
})

test_that('as.data.frame multi-row different classes', {
  methods = as.lcMethods(list(mTest, mRandom))
  df = as.data.frame(methods)
  expect_is(df, 'data.frame')
  expect_equal(nrow(df), 2)
})

test_that('vector argument', {
  methods = lcMethods(lcMethodTestLMKM(), nClusters = 1:3)
  expect_is(methods, 'list')
  expect_length(methods, 3)
})

test_that('vector variable argument', {
  nclus = 1:3
  methods = lcMethods(lcMethodTestLMKM(), nClusters = nclus)
  expect_is(methods, 'list')
  expect_length(methods, 3)
})

test_that('error argument', {
  expect_error(lcMethods(lcMethodTestLMKM(), fail = stop('no')), 'fail')
})

test_that('default', {
  methods = lcMethods(lcMethodTestLMKM())
  expect_is(methods, 'list')
  expect_length(methods, 1)
})

test_that('var', {
  methods = lcMethods(mTest)
  expect_is(methods, 'list')
  expect_length(methods, 1)
})


test_that('scalar argument', {
  methods = lcMethods(mTest, nClusters = 2)
  expect_is(methods, 'list')
  expect_length(methods, 1)
  expect_equal(methods[[1]]$nClusters, 2)
})


test_that('char argument', {
  methods = lcMethods(mTest, nClusters = 2, test = 'a')
  expect_is(methods, 'list')
  expect_length(methods, 1)
  expect_equal(methods[[1]]$nClusters, 2)
  expect_equal(methods[[1]]$test, 'a')
})


test_that('var with vector argument', {
  methods = lcMethods(mTest, nClusters = 1:3)
  expect_is(methods, 'list')
  expect_length(methods, 3)
})

test_that('.() argument', {
  a = 1
  b = 2
  methods = lcMethods(mTest, nClusters = .(a,b))
  expect_is(methods, 'list')
  expect_length(methods, 2)

  expect_equal(deparse(methods[[1]][['nClusters', eval = FALSE]]), 'a')
  expect_equal(deparse(methods[[2]][['nClusters', eval = FALSE]]), 'b')
})


test_that('cartesian', {
  a = 1
  b = 2
  methods = lcMethods(mTest, maxIt = 10, seed = .(a, b), nClusters = 1:3)
  expect_is(methods, 'list')
  expect_length(methods, 1 * 2 * 3)

  df = as.data.frame(methods, eval = FALSE)
  expect_true(all(df$maxIt == 10))
  expect_equal(df$seed, rep(c('a', 'b'), each = 3))
  expect_equal(df$nClusters, rep(1:3, 2))
})

test_that('do.call', {
  m = evaluate(mTest)
  methods = do.call(lcMethods, list(m, maxIt = 10, seed = 1:3, nClusters = 2))
  expect_is(methods, 'list')
  expect_length(methods, 1 * 3 * 1)
})

test_that('unnamed argument', {
  expect_error({
    lcMethods(mTest, 2)
  })
})
