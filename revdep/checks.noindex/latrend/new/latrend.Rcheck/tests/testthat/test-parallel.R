skip_on_cran()
skip_on_ci()
skip_if_not_installed('parallel')
skip_if_not_installed('doParallel')
skip_if(parallel::detectCores(logical = FALSE) < 2)

init_methodSleep = expression({
  setClass('lcMethodSleep', contains = 'lcMethodRandom')
  setMethod('fit', signature('lcMethodSleep'), function(method, data, envir, verbose, ...) {
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
  parallel::clusterEvalQ(cl, expr=library(latrend))
  do.call(parallel::clusterEvalQ, list(cl, init_methodSleep))

  parallel::clusterExport(cl, 'testLongData', envir = environment())
}
doParallel::registerDoParallel(cl)

mSleep = lcMethod('lcMethodSleep',
  response = 'Value',
  alpha = 10,
  sleep = 1,
  center = meanNA,
  time = 'Assessment',
  id = 'Traj',
  nClusters = 2,
  name = 'random')

# need a long sleep time to counteract the large start-up overhead time in Windows
mSleep10 = update(mSleep, sleep = 10)

test_that('parallel latrendRep', {
  time = system.time({
    output = latrendRep(mSleep10, data = testLongData, .rep = 2, .parallel = TRUE)
  })
  expect_lt(time['elapsed'], 18)
  expect_is(output, 'lcModels')
  expect_length(output, 2)
  expect_valid_lcModel(output[[1]])
})

test_that('parallel latrendBatch with 2 methods', {
  time = system.time({
    output = latrendBatch(list(mSleep10, mSleep10), data = testLongData, parallel = TRUE)
  })
  expect_lt(time['elapsed'], 18)
  expect_is(output, 'lcModels')
  expect_length(output, 2)
  expect_valid_lcModel(output[[1]])
})

test_that('parallel latrendBoot with 2 repetitions', {
  time = system.time({
    output = latrendBoot(mSleep10, data = testLongData, samples = 2, parallel = TRUE)
  })
  expect_lt(time['elapsed'], 18)
  expect_is(output, 'lcModels')
  expect_length(output, 2)
  expect_valid_lcModel(output[[1]])
})

test_that('parallel latrendCV with 2 folds', {
  time = system.time({
    output = latrendCV(mSleep10, data = testLongData, folds = 2, parallel = TRUE)
  })
  expect_lt(time['elapsed'], 18)
  expect_is(output, 'lcModels')
  expect_length(output, 2)
  expect_valid_lcModel(output[[1]])
})

# cleanup
if (exists('cl')) {
  parallel::stopCluster(cl)
}

foreach::registerDoSEQ()
