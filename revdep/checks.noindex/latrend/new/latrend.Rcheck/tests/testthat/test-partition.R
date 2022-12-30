context('partition')
rngReset()

refmodel = testModel

# assignments ####
test_that('integer assignments', {
  intAssignments = trajectoryAssignments(refmodel) %>% as.integer()

  model = lcModelPartition(
    testLongData,
    response = 'Value',
    trajectoryAssignments = intAssignments,
    nClusters = nClusters(refmodel)
  )

  expect_valid_lcModel(model)
  expect_equivalent(trajectoryAssignments(model), trajectoryAssignments(refmodel))
  expect_equivalent(nClusters(model), nClusters(refmodel))
  expect_true(converged(model))
})


test_that('factor assignments', {
  model = lcModelPartition(
    testLongData,
    response = 'Value',
    trajectoryAssignments = trajectoryAssignments(refmodel)
  )

  expect_equivalent(trajectoryAssignments(model), trajectoryAssignments(refmodel))
  expect_equivalent(nClusters(model), nClusters(refmodel))
  expect_equivalent(clusterNames(model), clusterNames(refmodel))
})


test_that('table assignments', {
  model = lcModelPartition(
    testLongData,
    response = 'Value',
    trajectoryAssignments = data.frame(
      Traj = ids(refmodel),
      Cluster = trajectoryAssignments(refmodel))
  )

  expect_equivalent(trajectoryAssignments(model), trajectoryAssignments(refmodel))
  expect_equivalent(nClusters(model), nClusters(refmodel))
  expect_equivalent(clusterNames(model), clusterNames(refmodel))
})


test_that('data column assignment', {
  skip_if_not_installed('mclustcomp')

  model = lcModelPartition(
    testLongData,
    response = 'Value',
    trajectoryAssignments = 'Class'
  )

  expect_true(externalMetric(model, refmodel, 'adjustedRand') >= .99)
  expect_equivalent(nClusters(model), nClusters(refmodel))
  expect_equivalent(clusterNames(model), clusterNames(refmodel))
})


test_that('character assignments', {
  model = lcModelPartition(
    testLongData,
    response = 'Value',
    trajectoryAssignments = as.character(trajectoryAssignments(refmodel))
  )

  expect_equivalent(trajectoryAssignments(model), trajectoryAssignments(refmodel))
  expect_equivalent(nClusters(model), nClusters(refmodel))
  expect_equivalent(clusterNames(model), clusterNames(refmodel))
})


test_that('local data', {
  {
    a = testLongData
    model = lcModelPartition(
      a,
      response = 'Value',
      trajectoryAssignments = trajectoryAssignments(refmodel)
    )
  }

  expect_valid_lcModel(model)
})



# clusterTrajectories ####
partModel = lcModelPartition(
  testLongData,
  response = 'Value',
  trajectoryAssignments = aggregate(Class ~ Traj, data.table::first, data = testLongData)$Class
)

test_that('clusterTrajectories', {
  clusTrajs = clusterTrajectories(partModel)

  expect_is(clusTrajs, 'data.frame')
  expect_named(clusTrajs, c('Cluster', 'Assessment', 'Value'))
  expect_is(clusTrajs$Cluster, 'factor')
  expect_equivalent(unique(clusTrajs$Assessment), unique(testLongData$Assessment))
  expect_equivalent(unique(clusTrajs$Cluster), unique(testLongData$Class))
  expect_equal(
    clusTrajs,
    as.data.frame(testLongData[, .(Value = mean(Value)), keyby = .(Cluster = Class, Assessment)])
  )
})

test_that('clusterTrajectories with median center', {
  clusTrajs = clusterTrajectories(partModel, center = median)

  expect_equal(
    clusTrajs,
    as.data.frame(testLongData[, .(Value = median(Value)), keyby = .(Cluster = Class, Assessment)])
  )
})

test_that('clusterTrajectories at subset of times', {
  times = head(time(partModel), 3)
  clusTrajs = clusterTrajectories(partModel, center = mean, at = times)

  expect_equivalent(unique(clusTrajs$Assessment), times)
  expect_equal(
    clusTrajs,
    as.data.frame(
      testLongData[
        Assessment %in% times,
        .(Value = mean(Value)),
        keyby = .(Cluster = Class, Assessment)
      ]
    )
  )
})

test_that('non-converged partition result', {
  intAssignments = trajectoryAssignments(refmodel) %>% as.integer()

  model = lcModelPartition(
    testLongData,
    response = 'Value',
    trajectoryAssignments = intAssignments,
    nClusters = nClusters(refmodel),
    converged = FALSE
  )

  expect_false(converged(model))
})

test_that('numeric converged partition result', {
  intAssignments = trajectoryAssignments(refmodel) %>% as.integer()

  model = lcModelPartition(
    testLongData,
    response = 'Value',
    trajectoryAssignments = intAssignments,
    nClusters = nClusters(refmodel),
    converged = 3
  )

  expect_equal(converged(model), 3)
})

