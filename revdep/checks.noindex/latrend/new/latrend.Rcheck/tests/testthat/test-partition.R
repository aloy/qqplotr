context('partition')
rngReset()

refmodel = latrend(lcMethodTestKML(), testLongData)

test_that('integer assignments', {
  intAssignments = trajectoryAssignments(refmodel) %>% as.integer()

  model = lcModelPartition(testLongData,
                           response = 'Value',
                           trajectoryAssignments = intAssignments,
                           nClusters = nClusters(refmodel))
  expect_valid_lcModel(model)
  expect_equivalent(trajectoryAssignments(model), trajectoryAssignments(refmodel))
  expect_equivalent(nClusters(model), nClusters(refmodel))
})

test_that('factor assignments', {
  model = lcModelPartition(testLongData,
                           response = 'Value',
                           trajectoryAssignments = trajectoryAssignments(refmodel))
  expect_valid_lcModel(model)
  expect_equivalent(trajectoryAssignments(model), trajectoryAssignments(refmodel))
  expect_equivalent(nClusters(model), nClusters(refmodel))
})

test_that('local data', {
  {
    a = testLongData
    model = lcModelPartition(a,
                             response = 'Value',
                             trajectoryAssignments = trajectoryAssignments(refmodel))
  }

  expect_valid_lcModel(model)
})
