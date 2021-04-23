lcMethodTestKML = function(...) {
  lcMethodKML(response = 'Value', nbRedrawing=1, maxIt=10, ..., seed=1)
}

if (requireNamespace('lcmm')) {
  lcMethodTestLcmmGMM = function(...) {
    lcMethodLcmmGMM(fixed = Value ~ 1, maxiter=10, ..., seed=1)
  }

  lcMethodTestLcmmGBTM = function(...) {
    lcMethodLcmmGBTM(fixed = Value ~ 1, maxiter=10, ..., seed=1)
  }
}

if (requireNamespace('flexmix')) {
  lcMethodTestFlexmixGBTM = function(...) {
    lcMethodFlexmixGBTM(formula = Value ~ Assessment, ..., control=list(iter.max=1, tolerance=1e-3), seed=1)
  }

  lcMethodTestFlexmix = function(...) {
    lcMethodFlexmix(formula = Value ~ 0, ...)
  }
}

if (requireNamespace('crimCV')) {
  lcMethodTestCrimCV = function(...) {
    lcMethodCrimCV(response = 'Value', ..., model='ZIP', dpolyp=2, dpolyl=1, init=5, seed=1)
  }
}

if (requireNamespace('funFEM')) {
  lcMethodTestFunFEM = function(...) {
    lcMethodFunFEM(response = 'Value', ...)
  }
}

if (requireNamespace('mclust')) {
  lcMethodTestMclustLLPA = function(...) {
    lcMethodMclustLLPA(response = 'Value', ...)
  }
}

lcMethodTestRandom = function(...) {
  lcMethodRandom(response = 'Value', ...)
}

lcMethodTestStratify = function(...) {
  lcMethodStratify(response = 'Value', ...)
}

if (requireNamespace('lme4')) {
  lcMethodTestGCKM = function(...) {
    lcMethodGCKM(formula = Value ~ (1 | Traj), ...)
  }
}

lcMethodTestLMKM = function(...) {
  lcMethodLMKM(formula = Value ~ Assessment, ...)
}

lcMethodTestTwoStep = function(...) {
  lcMethodFeature(response = 'Value', ...)
}

lcMethodTestMixTVEM = function(...) {
  lcMethodMixTVEM(formula = Value ~ time(1) - 1, ..., convergenceCriterion = 1e-6, numStarts = 10, maxIterations = 1e3, numInteriorKnots = 6, seed=2L)
}

lcMethodTestCrimCVt = function(...) {
  lcMethodCrimCV(response = 'Value', ..., model='ZIPt', dpolyp=2, init=5, seed=1)
}

if (requireNamespace('longclust')) {
  lcMethodTestLongclust = function(...) {
    lcMethodLongclust(response = 'Value', modelSubset='VVA', gaussian=TRUE, ..., seed=1)
  }

  lcMethodTestLongclustT = function(...) {
    lcMethodLongclust(response = 'Value', modelSubset='VEI', gaussian=FALSE, ..., seed=1)
  }
}

if (requireNamespace('mixtools')) {
  lcMethodTestMixtoolsNPRM = function(...) {
    lcMethodMixtoolsNPRM(response = 'Value', maxiter=10, eps=1e-04, seed=1)
  }

  lcMethodTestMixtoolsGMM = function(...) {
    lcMethodMixtoolsGMM(formula = Value ~ Assessment + (Assessment | Traj), epsilon=1e-02, ..., seed=1)
  }
}

if (requireNamespace('mixAK')) {
  lcMethodTestMixAK_GLMM = function(...) {
    lcMethodMixAK_GLMM(fixed = Value ~ 1, random = ~ Assessment, ..., seed=1)
  }
}
