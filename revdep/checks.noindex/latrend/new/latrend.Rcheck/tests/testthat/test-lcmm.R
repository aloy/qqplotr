context('LCMM models')
skip_if_not_installed('lcmm')
skip_on_cran()
rngReset()
tests = c(DEFAULT_LATREND_TESTS)

lcmmData = generateLongData(
  sizes = rep(25L, 3L),
  data = data.frame(Time = 1:4),
  fixed = Value ~ 1,
  cluster = ~ 1,
  clusterCoefs = t(c(-1, 0, 1)),
  random = ~ 1,
  randomScales = cbind(.1, .1, .1),
  noiseScales = rep(.1, 3L),
  id = 'Id'
) %>%
  setnames('Class', 'Cluster')
# %>%
#   .[, Age := 65] %>%
#   .[c(1, 10, 20), Age := NA] %>%

# GMM ####
make.gmm = function(id, time, response, ..., init = 'default') {
  mc = match.call.all()
  mc$fixed = as.formula(sprintf('%s ~ 1', response))
  mc$id = id
  mc$time = time
  mc$maxiter = 10
  mc$seed = 1

  do.call(lcMethodLcmmGMM, as.list(mc)[-1]) %>% evaluate()
}

test_that('gmm', {
  expect_true({
    test.latrend('lcMethodLcmmGMM', instantiator = make.gmm, tests = tests, data = lcmmData)
  })
})

test_that('gmm with init=lme', {
  skip_on_cran()
  method = make.gmm(id = 'Traj', time = 'Assessment', response = 'Value', init = 'lme')
  model = latrend(method, testLongData)
  expect_true(is.lcModel(model))
})

test_that('gmm with init=lme.random', {
  skip_on_cran()
  method = make.gmm(id = 'Traj', time = 'Assessment', response = 'Value', init = 'lme.random')
  model = latrend(method, testLongData)
  expect_true(is.lcModel(model))
})

test_that('gmm with NA covariate', {
  expect_true({
    test.latrend('lcMethodLcmmGMM', instantiator = make.gmm, tests = tests, data = lcmmData)
  })
})

# GBTM ####
make.gbtm = function(id, time, response, ..., init = NULL) {
  mc = match.call.all()
  mc$fixed = as.formula(sprintf('%s ~ 1', response))
  mc$id = id
  mc$time = time
  mc$maxiter = 10
  mc$seed = 1

  do.call(lcMethodLcmmGBTM, as.list(mc)[-1]) %>% evaluate()
}

test_that('gbtm', {
  expect_true({
    test.latrend('lcMethodLcmmGBTM', instantiator = make.gbtm, tests = tests, data = lcmmData)
  })
})
