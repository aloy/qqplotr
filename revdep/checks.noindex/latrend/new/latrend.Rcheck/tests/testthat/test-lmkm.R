context('LM-KM')
rngReset()
tests = c(DEFAULT_LATREND_TESTS)

make.lmkm = function(id, time, response, ...) {
  form = as.formula(sprintf('%s ~ 1', response))
  lcMethodLMKM(formula = form, time = time, id = id, ..., seed = 1) %>% evaluate()
}

test_that('tests', {
  expect_true({
    test.latrend('lcMethodLMKM', instantiator = make.lmkm, tests = tests)
  })
})
