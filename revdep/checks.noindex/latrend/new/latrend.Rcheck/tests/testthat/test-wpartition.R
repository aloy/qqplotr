context('weighted partition')
rngReset()

refmodel = latrend(lcMethodTestKML(), testLongData)

test_that('default', {
  model = lcModelWeightedPartition(testLongData, response = 'Value', weights=postprob(refmodel))

  expect_valid_lcModel(model)
  expect_equivalent(nClusters(model), nClusters(refmodel))
  expect_equivalent(trajectoryAssignments(model), trajectoryAssignments(refmodel))
  expect_equivalent(postprob(model), postprob(refmodel))
})

test_that('non-unit weights', {
  model = lcModelWeightedPartition(testLongData, response = 'Value', weights=2 * postprob(refmodel))

  expect_valid_lcModel(model)
  expect_equivalent(nClusters(model), nClusters(refmodel))
  expect_equivalent(trajectoryAssignments(model), trajectoryAssignments(refmodel))
  expect_equivalent(postprob(model), postprob(refmodel))
})
