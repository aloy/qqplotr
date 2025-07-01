context('twostep models')
skip_if_not_installed('lme4')
rngReset()
tests = DEFAULT_LATREND_TESTS

make.gckm = function(id, response, ...) {
  m = lcMethodGCKM(formula = as.formula(sprintf('%s ~ (1 | %s)', response, id)), id = id, ...)
  evaluate(m) # dont chain using magrittr because this leads to a wrongful evaluation of nClusters (no clue why)
}

test_that('specify', {
  repfun = function(method, data, ...) {
    as.data.table(data)[, mean(Value), by = id]$V1 %>% cbind()
  }
  clusfun = function(method, data, repMat, ...) {
    clusters = factor(repMat[,1] > 0, levels = c(F,T), labels = c('Low', 'High'))
    lcModelPartition(response = method$response, data = data, trajectoryAssignments = clusters)
  }
  method = lcMethodTestTwoStep(representationStep = repfun, clusterStep = clusfun, standardize = scale)

  model = expect_silent(latrend(method, testLongData))

  expect_valid_lcModel(model)
})

test_that('gckm', {
  expect_true({
    test.latrend('lcMethodGCKM', instantiator = make.gckm, tests = tests)
  })
})

test_that('gckm through latrendBatch', {
  method = make.gckm(id = 'id', response = 'Value')
  models = latrendBatch(lcMethods(method, nClusters = 1:3), testLongData)
  expect_length(models, 3)
})
