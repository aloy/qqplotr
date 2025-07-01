context('weighted partition')
rngReset()

refmodel = testModel2

test_that('default', {
  model = lcModelWeightedPartition(testLongData, response = 'Value', weights = postprob(refmodel))

  expect_valid_lcModel(model)
  expect_equivalent(nClusters(model), nClusters(refmodel))
  expect_equivalent(trajectoryAssignments(model), trajectoryAssignments(refmodel))
  expect_equivalent(postprob(model), postprob(refmodel))
})

test_that('non-unit weights', {
  model = lcModelWeightedPartition(testLongData, response = 'Value', weights = 2 * postprob(refmodel))

  expect_valid_lcModel(model)
  expect_equivalent(nClusters(model), nClusters(refmodel))
  expect_equivalent(trajectoryAssignments(model), trajectoryAssignments(refmodel))
  expect_equivalent(postprob(model), postprob(refmodel))
})

# clusterTrajectories ####
partModel = lcModelWeightedPartition(
  testLongData,
  response = 'Value',
  weights = postprob(refmodel)
)

test_that('clusterTrajectories with hard separation model', {
  clusTrajs = clusterTrajectories(partModel)

  expect_is(clusTrajs, 'data.frame')
  expect_named(clusTrajs, c('Cluster', 'time', 'Value'))
  expect_is(clusTrajs$Cluster, 'factor')
  expect_equivalent(unique(clusTrajs$time), unique(testLongData$time))
  expect_equivalent(unique(clusTrajs$Cluster), unique(testLongData$Class))

  refdata = copy(testLongData)
  refdata[, Cluster := trajectoryAssignments(partModel)[make.idRowIndices(partModel)]]
  expect_equal(
    clusTrajs,
    as.data.frame(refdata[, .(Value = mean(Value)), keyby = .(Cluster = Cluster, time)])
  )
})

test_that('clusterTrajectories with an unrepresented cluster', {
  model = lcModelWeightedPartition(
    testLongData,
    response = 'Value',
    weights = cbind(rep(1, nIds(partModel)), 0)
  )

  expect_warning({clusTrajs = clusterTrajectories(model)})
  expect_true(all(as.data.table(clusTrajs)[Cluster == 'B', is.na(Value)]))
})

test_that('clusterTrajectories interpolation with an unrepresented cluster', {
  model = lcModelWeightedPartition(
    testLongData,
    response = 'Value',
    weights = cbind(rep(1, nIds(partModel)), 0)
  )

  expect_warning({clusTrajs = clusterTrajectories(model, at = .3523)})
  expect_true(all(as.data.table(clusTrajs)[Cluster == 'B', is.na(Value)]))
})
