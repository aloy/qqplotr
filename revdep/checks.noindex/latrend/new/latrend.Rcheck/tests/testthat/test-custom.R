context('custom model')
rngReset()

test_that('default', {
  clusfun = function(data, ...) {
       clusters = as.data.table(data) %>%
           {.[, mean(Value) > 0, by = Traj]$V1} %>%
           factor(levels = c(F,T), labels = c('Low', 'High'))
       list(clusters = clusters)
       lcModelCustom(data = data, response = 'Value', trajectoryAssignments = clusters)
  }
  method = lcMethodCustom(response = 'Value', fun = clusfun)

  model = latrend(method, testLongData)

  expect_valid_lcModel(model)
})
