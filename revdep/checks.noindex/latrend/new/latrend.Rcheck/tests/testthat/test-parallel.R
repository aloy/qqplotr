skip_on_cran()
skip_on_ci()
skip_on_os('windows')
skip_if_not_installed('parallel')
skip_if_not_installed('doParallel')
skip_if(parallel::detectCores(logical = FALSE) < 2)

init_methodSleep = expression({
  setClass('lcMethodSleep', contains = 'lcMethodRandom')
  setMethod('fit', 'lcMethodSleep', function(method, data, envir, verbose, ...) {
    stopifnot(
      is.data.frame(data),
      nrow(data) > 0
    )
    Sys.sleep(method$sleep)

    callNextMethod()
  })
})

eval(init_methodSleep)

if (.Platform$OS.type == 'unix') {
  cl = parallel::makeCluster(2, type = 'FORK')
} else {
  cl = parallel::makeCluster(2)

  # init cl
  parallel::clusterEvalQ(cl, expr = library(latrend))
  do.call(parallel::clusterEvalQ, list(cl, init_methodSleep))
}
doParallel::registerDoParallel(cl)

newTestData = copy(testLongData)

mSleep = new('lcMethodSleep',
  response = 'Value',
  alpha = 10,
  sleep = 1,
  center = meanNA,
  time = 'time',
  id = 'id',
  nClusters = 2,
  name = 'random'
)

# need a long sleep time to counteract the large start-up overhead time in Windows
mSleep10 = update(mSleep, sleep = 10)

test_that('parallel latrendRep', {
  time = system.time({
    output = latrendRep(mSleep10, data = newTestData, .rep = 2, .parallel = TRUE)
  })
  expect_lt(time['elapsed'], 18)
  expect_is(output, 'lcModels')
  expect_length(output, 2)
  expect_is(output[[1]], 'lcModel')
})

test_that('parallel latrendRep with lcmm', {
  mGmm = lcMethodTestLcmmGMM()

  expect_warning({ # warning for seed
    output = latrendRep(mGmm, data = newTestData, .rep = 2, .parallel = TRUE)
  })
  expect_is(output, 'lcModels')
  expect_length(output, 2)
  expect_is(output[[1]], 'lcModel')
})

test_that('parallel latrendBatch with 2 methods', {
  time = system.time({
    output = latrendBatch(list(mSleep10, mSleep10), data = newTestData, parallel = TRUE)
  })
  expect_lt(time['elapsed'], 18)
  expect_is(output, 'lcModels')
  expect_length(output, 2)
  expect_is(output[[1]], 'lcModel')
})

test_that('parallel latrendBatch with lcmm', {
  mGmm = lcMethodTestLcmmGMM()

  output = latrendBatch(list(mGmm, mGmm), data = newTestData, parallel = TRUE)
  expect_is(output, 'lcModels')
  expect_length(output, 2)
  expect_is(output[[1]], 'lcModel')
})

test_that('parallel latrendBatch with local data', {
  localData = generateLongData(
    sizes = c(20, 30),
    fixed = Value ~ 1 + time,
    cluster = ~ 1 + time,
    random = ~ 1,
    id = 'id',
    data = data.frame(time = seq(0, 1, by = .1)),
    fixedCoefs = c(0, 0),
    clusterCoefs = cbind(c(-2, 1), c(2, -1)),
    randomScales = cbind(.1, .1),
    noiseScales = c(.1, .1),
    clusterNames = c('A', 'B'),
    shuffle = TRUE
  )

  output = latrendBatch(list(mSleep, mSleep), data = localData, parallel = TRUE)
  expect_is(output, 'lcModels')
  expect_length(output, 2)
})

test_that('parallel latrendBatch with local data list', {
  a = 20
  b = 30
  localDataList = lapply(1:2, function(seed) {
    set.seed(seed)
    generateLongData(
      sizes = c(a, b),
      fixed = Value ~ 1 + time,
      cluster = ~ 1 + time,
      random = ~ 1,
      id = 'id',
      data = data.frame(time = seq(0, 1, by = .1)),
      fixedCoefs = c(0, 0),
      clusterCoefs = cbind(c(-2, 1), c(2, -1)),
      randomScales = cbind(.1, .1),
      noiseScales = c(.1, .1),
      clusterNames = c('A', 'B'),
      shuffle = TRUE
    )
  })

  output = latrendBatch(list(mSleep), data = localDataList, parallel = TRUE)
  expect_is(output, 'lcModels')
  expect_length(output, 2)
})

test_that('parallel latrendBoot with 2 repetitions', {
  time = system.time({
    output = latrendBoot(mSleep10, data = newTestData, samples = 2, parallel = TRUE)
  })
  expect_lt(time['elapsed'], 18)
  expect_is(output, 'lcModels')
  expect_length(output, 2)
  expect_is(output[[1]], 'lcModel')
})

test_that('parallel latrendCV with 2 folds', {
  time = system.time({
    output = latrendCV(mSleep10, data = newTestData, folds = 2, parallel = TRUE)
  })
  expect_lt(time['elapsed'], 18)
  expect_is(output, 'lcModels')
  expect_length(output, 2)
  expect_is(output[[1]], 'lcModel')
})

# cleanup
if (exists('cl')) {
  parallel::stopCluster(cl)
}

foreach::registerDoSEQ()
