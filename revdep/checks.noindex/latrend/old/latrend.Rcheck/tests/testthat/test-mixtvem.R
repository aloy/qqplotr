context('MixTVEM')
skip_if(!exists('TVEMMixNormal'), message = 'skipping MixTVEM tests because the TVEMMixNormal() function is not loaded')
rngReset()
tests = c('method', 'basic')

make.mixtvem = function(response, ...) {
  lcMethodMixTVEM(
    formula = as.formula(sprintf('%s ~ time(1) - 1', response)),
    ...,
    convergenceCriterion = 1e-6,
    deg = 1,
    numStarts = 10,
    maxIterations = 1e3,
    numInteriorKnots = 2,
    seed = 2L
  )
}

testData = generateLongData(
  sizes = rep(25L, 3L),
  data = data.frame(Time = 1:10),
  fixed = Value ~ 1,
  cluster = ~ 1,
  clusterCoefs = t(c(-1, 0, 1)),
  random = ~ 1,
  randomScales = cbind(.1, .1, .1),
  noiseScales = rep(.1, 3L),
  id = 'Id'
) %>%
  setnames('Class', 'Cluster')

test_that('default', {
  expect_true({
    test.latrend('lcMethodMixTVEM', instantiator = make.mixtvem, data = testData, tests = tests)
  })
})
