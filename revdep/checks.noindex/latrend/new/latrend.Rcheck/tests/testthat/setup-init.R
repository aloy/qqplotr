options(deparse.max.lines=5)

options(latrend.id = 'Traj')
options(latrend.time = 'Assessment')
options(latrend.verbose = R.utils::Verbose())

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

  # change cluster names to ensure the model implementations correctly handle this
  clusNames = paste0('T', seq_len(nClusters(object)))
  clusterNames(object) = clusNames
  expect_equal(clusterNames(object), clusNames)

  getCall(object) %>%
    expect_is('call')
  model.data(object) %>%
    expect_is('data.frame')
  getName(object) %>%
    expect_is('character')
  getShortName(object) %>%
    expect_is('character')
  idVariable(object) %>%
    expect_is('character')
  timeVariable(object) %>%
    expect_is('character')
  responseVariable(object) %>%
    expect_is('character')
  coef(object) %>%
    expect_is(c('numeric', 'matrix', 'list', 'NULL'), label='coef')
  converged(object) %>%
    expect_is(c('logical', 'numeric', 'integer'), label='converged')
  nClusters(object) %T>%
    {expect_true(is.count(.))}
  clusterNames(object) %>%
    expect_is('character') %>%
    expect_length(nClusters(object))


  # Posterior
  pp = postprob(object)
  expect_true(is_valid_postprob(pp, object))

  clus = trajectoryAssignments(object)
  expect_is(clus, 'factor')
  expect_length(clus, nIds(object))
  expect_gte(min(as.integer(clus)), 1)
  expect_lte(max(as.integer(clus)), nIds(object))

  # Predict
  if(!is(object, 'lcModelCustom')) {
    # cluster-specific prediction
    pred = predict(object, newdata=data.frame(Cluster='T1', Assessment=time(object)[c(1,3)]))
    expect_is(pred, 'data.frame', info='predictClusterTime')
    expect_true('Fit' %in% names(pred), info='predictClusterTime')
    expect_equal(nrow(pred), 2, info='predictClusterTime')

    # prediction for all clusters; list of data.frames
    pred2 = predict(object, newdata=data.frame(Assessment=time(object)[c(1,3)]))
    expect_is(pred2, 'list', info='predictTime')
    expect_length(pred2, nClusters(object))
    expect_true('Fit' %in% names(pred2$T1), info='predictTime')

    # id-specific prediction for a specific cluster; data.frame
    pred3 = predict(object, newdata=data.frame(Cluster=rep('T1', 4),
                                       Traj=c(ids(object)[c(1,1,2)], tail(ids(object), 1)),
                                       Assessment=c(time(object)[c(1,3,1,1)])))
    expect_is(pred3, 'data.frame', info='predictClusterIdTime')
    expect_true('Fit' %in% names(pred3), info='predictClusterIdTime')
    expect_equal(nrow(pred3), 4, info='predictClusterIdTime')

    # id-specific prediction for all clusters; list of data.frames
    pred4 = predict(object, newdata=data.frame(Traj=c(ids(object)[c(1,1,2)], tail(ids(object), 1)),
                                       Assessment=c(time(object)[c(1,3,1,1)])))
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

    residuals(object, clusters=trajectoryAssignments(object)) %>%
      expect_is(c('NULL', 'numeric'), label='residuals')
    residuals(object, clusters=NULL) %>%
      expect_is(c('NULL', 'matrix'), label='residuals')
  }


  # Derivative predict
  clusterTrajectories(object) %>%
    expect_is('data.frame', label='clusterTrajectories')
  trajectories(object) %>%
    expect_is('data.frame', label='trajectories')

  expect_true(is.ggplot(plot(object)))

  # Misc
  summary(object) %>%
    expect_is('lcSummary')
  expect_output(print(object))

  return(object)
}
