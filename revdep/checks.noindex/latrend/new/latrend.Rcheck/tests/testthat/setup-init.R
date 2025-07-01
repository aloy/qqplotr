options(deparse.max.lines=5)

options(latrend.id = 'id')
options(latrend.time = 'time')
options(latrend.verbose = R.utils::Verbose())
options(latrend.warnModelDataClusterColumn = FALSE)
options(latrend.warnNewDataClusterColumn = FALSE)
options(latrend.warnTrajectoryLength = 0)

DEFAULT_LATREND_TESTS = c('method', 'basic', 'fitted', 'predict', 'cluster-single', 'cluster-three', 'data-na', 'data-varlen')

foreach::registerDoSEQ()

# remove kml cld file from possible previous failed run
if (file.exists('cld.Rdata')) {
  file.remove('cld.Rdata')
}

# one of the cluster methods is altering the RNG kind, so reset it for each context
rngReset = function() {
  RNGkind('Mersenne-Twister', 'Inversion', 'Rejection')
}

mixt_file = file.path('..', '..', 'MixTVEM.R')
if(file.exists(mixt_file)) {
  source(mixt_file)
}

expect_valid_lcModel = function(object) {
  expect_s4_class(object, 'lcModel')
  expect_true(is.count(nClusters(object)))

  # clusters
  # change cluster names to ensure the model implementations correctly handle this
  clusNames = paste0('T', seq_len(nClusters(object)))
  clusterNames(object) = clusNames
  expect_is(clusterNames(object), 'character')
  expect_length(clusterNames(object), nClusters(object))
  expect_equal(clusterNames(object), clusNames)
  expect_is(clusterSizes(object), 'integer')
  expect_true(all(clusterSizes(object) >= 0))
  expect_is(clusterProportions(object), 'numeric')
  expect_gte(min(clusterProportions(object)), 0)
  expect_lte(max(clusterProportions(object)), 1)

  expect_is(getCall(object), 'call')
  expect_is(getName(object), 'character')
  expect_is(getShortName(object), 'character')
  expect_is(idVariable(object), 'character')
  expect_is(timeVariable(object), 'character')
  expect_is(responseVariable(object), 'character')
  expect_is(coef(object), c('numeric', 'matrix', 'list', 'NULL'), label='coef')
  expect_is(converged(object), c('logical', 'numeric', 'integer'), label='converged')

  expect_true(is.count(nobs(object)))

  expect_true(is.count(nIds(object)))
  expect_length(ids(object), nIds(object))

  expect_true(is.numeric(time(object)))
  expect_gt(length(time(object)), 0)

  expect_is(deviance(object), 'numeric')
  expect_true(is.numeric(df.residual(object)))
  expect_is(logLik(object), 'logLik')
  expect_is(sigma(object), 'numeric')
  expect_gte(estimationTime(object), 0)
  expect_is(formula(object), 'formula')


  # model.data
  expect_is(model.data(object), 'data.frame')
  expect_true(has_name(model.data(object), c(
    idVariable(object),
    timeVariable(object),
    responseVariable(object))))

  # Posterior
  pp = postprob(object)
  expect_true(is_valid_postprob(pp, object))

  clus = trajectoryAssignments(object)
  expect_is(clus, 'factor')
  expect_length(clus, nIds(object))
  expect_gte(min(as.integer(clus)), 1)
  expect_lte(max(as.integer(clus)), nIds(object))

  # Predict
  # cluster-specific prediction
  pred = predict(object, newdata = data.frame(Cluster = 'T1', time = time(object)[c(1,3)]))
  expect_is(pred, 'data.frame', info='predictClusterTime')
  expect_true('Fit' %in% names(pred), info='predictClusterTime')
  expect_equal(nrow(pred), 2, info='predictClusterTime')

  # prediction for all clusters; list of data.frames
  pred2 = predict(object, newdata = data.frame(time = time(object)[c(1,3)]))
  expect_is(pred2, 'list', info='predictTime')
  expect_length(pred2, nClusters(object))
  expect_true('Fit' %in% names(pred2$T1), info='predictTime')

  # id-specific prediction for a specific cluster; data.frame
  pred3 = predict(object, newdata = data.frame(
    Cluster = rep('T1', 4),
    id = c(ids(object)[c(1,1,2)], tail(ids(object), 1)),
    time = c(time(object)[c(1,3,1,1)])
  ))
  expect_is(pred3, 'data.frame', info='predictClusterIdTime')
  expect_true('Fit' %in% names(pred3), info='predictClusterIdTime')
  expect_equal(nrow(pred3), 4, info='predictClusterIdTime')

  # id-specific prediction for all clusters; list of data.frames
  pred4 = predict(object, newdata = data.frame(
    id = c(ids(object)[c(1,1,2)], tail(ids(object), 1)),
    time = c(time(object)[c(1,3,1,1)])
  ))
  expect_is(pred4, 'list', info='predictIdTime')
  expect_length(pred4, nClusters(object))
  expect_true('Fit' %in% names(pred4$T1), info='predictIdTime')

  fitted(object, clusters=trajectoryAssignments(object)) %>%
    expect_is(c('NULL', 'numeric'), info='fittedClusters')
  fitted(object, clusters=NULL) %>%
    expect_is(c('NULL', 'matrix'), info='fittedNull')

  predNul = predict(object, newdata=NULL)
  expect_is(predNul, 'list', info='predictNull')
  expect_length(predNul, nClusters(object))
  expect_true('Fit' %in% names(predNul$T1), info='predictNull')

  # predictForCluster
  predClus = predictForCluster(
    object,
    newdata = data.frame(time = time(object)[c(1,3)]),
    cluster = 'T1'
  )
  expect_is(predClus, 'numeric', info='predictForCluster')
  expect_length(predClus, 2)

  # empty predictForCluster prediction
  predClusNull = predictForCluster(object, cluster = 'T1')
  predClusNull2 = predictForCluster(object, newdata = NULL, cluster = 'T1')
  predClusFitted = predictForCluster(object, newdata = model.data(object), cluster = 'T1')
  expect_equal(predClusNull, predClusNull2)
  expect_equal(predClusNull2, predClusFitted)

  residuals(object, clusters=trajectoryAssignments(object)) %>%
    expect_is(c('NULL', 'numeric'), label='residuals')
  residuals(object, clusters=NULL) %>%
    expect_is(c('NULL', 'matrix'), label='residuals')

  # Derivative predict
  ctPred = clusterTrajectories(object)
  expect_is(ctPred, 'data.frame', label='clusterTrajectories')

  fittedTrajectories(object) %>%
    expect_is(c('NULL', 'data.frame'), label='fittedTrajectories')
  trajectories(object) %>%
    expect_is('data.frame', label='trajectories')

  if (requireNamespace('ggplot2', quietly = TRUE)) {
    expect_true(is.ggplot(plot(object)))
  }

  # Misc
  summary(object) %>%
    expect_is('lcSummary')
  expect_output(print(object))

  newObject = strip(object)
  expect_is(newObject, class(object))

  return(object)
}
