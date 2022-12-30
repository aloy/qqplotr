pkgname <- "latrend"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('latrend')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("PAP.adh")
### * PAP.adh

flush(stderr()); flush(stdout())

### Name: PAP.adh
### Title: Biweekly Mean Therapy Adherence of OSA Patients over 1 Year
### Aliases: PAP.adh PAP.adh1y
### Keywords: datasets

### ** Examples

data(PAP.adh)

if (require("ggplot2")) {
  plotTrajectories(PAP.adh, id = "Patient", time = "Biweek", response = "UsageHours")

  # plot according to cluster ground truth
  plotTrajectories(
    PAP.adh,
    id = "Patient",
    time = "Biweek",
    response = "UsageHours",
    cluster = "Group"
  )
}



cleanEx()
nameEx("as.list.lcMethod")
### * as.list.lcMethod

flush(stderr()); flush(stdout())

### Name: as.list.lcMethod
### Title: Extract the method arguments as a list
### Aliases: as.list.lcMethod

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
as.list(method)

as.list(method, args = c("id", "time"))

if (require("kml")) {
  method <- lcMethodKML("Y", id = "Id", time = "Time")
  as.list(method)

  # select arguments used by kml()
  as.list(method, args = kml::kml)

  # select arguments used by either kml() or parALGO()
  as.list(method, args = c(kml::kml, kml::parALGO))
}



cleanEx()
nameEx("clusterNames-set")
### * clusterNames-set

flush(stderr()); flush(stdout())

### Name: clusterNames<-
### Title: Update the cluster names
### Aliases: clusterNames<-

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData, nClusters = 2)
clusterNames(model) <- c("Group 1", "Group 2")



cleanEx()
nameEx("clusterNames")
### * clusterNames

flush(stderr()); flush(stdout())

### Name: clusterNames
### Title: Get the cluster names
### Aliases: clusterNames

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData)
clusterNames(model) # A, B



cleanEx()
nameEx("clusterProportions")
### * clusterProportions

flush(stderr()); flush(stdout())

### Name: clusterProportions
### Title: Proportional size of each cluster
### Aliases: clusterProportions clusterProportions,lcModel-method

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData, nClusters = 2)
clusterProportions(model)



cleanEx()
nameEx("clusterSizes")
### * clusterSizes

flush(stderr()); flush(stdout())

### Name: clusterSizes
### Title: Number of trajectories per cluster
### Aliases: clusterSizes

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData, nClusters = 2)
clusterSizes(model)



cleanEx()
nameEx("clusterTrajectories")
### * clusterTrajectories

flush(stderr()); flush(stdout())

### Name: clusterTrajectories
### Title: Extract the cluster trajectories
### Aliases: clusterTrajectories clusterTrajectories,lcModel-method

### ** Examples

method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData)

clusterTrajectories(model)

clusterTrajectories(model, at = c(0, .5, 1))



cleanEx()
nameEx("coef.lcModel")
### * coef.lcModel

flush(stderr()); flush(stdout())

### Name: coef.lcModel
### Title: Extract lcModel coefficients
### Aliases: coef.lcModel

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData, nClusters = 2)
coef(model)



cleanEx()
nameEx("confusionMatrix")
### * confusionMatrix

flush(stderr()); flush(stdout())

### Name: confusionMatrix
### Title: Compute the posterior confusion matrix
### Aliases: confusionMatrix

### ** Examples

data(latrendData)

if (rlang::is_installed("lcmm")) {
  method <- lcMethodLcmmGMM(
    fixed = Y ~ Time,
    mixture = ~ Time,
    random = ~ 1,
    id = "Id",
    time = "Time"
  )
  model <- latrend(method, latrendData)
  confusionMatrix(model)
}



cleanEx()
nameEx("converged")
### * converged

flush(stderr()); flush(stdout())

### Name: converged
### Title: Check model convergence
### Aliases: converged converged,lcModel-method

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData, nClusters = 2)
converged(model)



cleanEx()
nameEx("createTestDataFold")
### * createTestDataFold

flush(stderr()); flush(stdout())

### Name: createTestDataFold
### Title: Create the test fold data for validation
### Aliases: createTestDataFold

### ** Examples

data(latrendData)

if (require("caret")) {
  trainDataList <- createTrainDataFolds(latrendData, id = "Id", folds = 10)
  testData1 <- createTestDataFold(latrendData, trainDataList[[1]], id = "Id")
}



cleanEx()
nameEx("createTestDataFolds")
### * createTestDataFolds

flush(stderr()); flush(stdout())

### Name: createTestDataFolds
### Title: Create all k test folds from the training data
### Aliases: createTestDataFolds

### ** Examples

data(latrendData)

if (require("caret")) {
  trainDataList <- createTrainDataFolds(latrendData, folds = 10, id = "Id")
  testDataList <- createTestDataFolds(latrendData, trainDataList)
}



cleanEx()
nameEx("createTrainDataFolds")
### * createTrainDataFolds

flush(stderr()); flush(stdout())

### Name: createTrainDataFolds
### Title: Create the training data for each of the k models in k-fold
###   cross validation evaluation
### Aliases: createTrainDataFolds

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")

if (require("caret")) {
  trainFolds <- createTrainDataFolds(latrendData, folds = 5, id = "Id", seed = 1)

  foldModels <- latrendBatch(method, data = trainFolds)
  testDataFolds <- createTestDataFolds(latrendData, trainFolds)
}



cleanEx()
nameEx("defineInternalMetric")
### * defineInternalMetric

flush(stderr()); flush(stdout())

### Name: defineInternalMetric
### Title: Define an internal metric for lcModels
### Aliases: defineInternalMetric

### ** Examples

defineInternalMetric("BIC", fun = BIC)

mae <- function(object) {
  mean(abs(residuals(object)))
}
defineInternalMetric("MAE", fun = mae)



cleanEx()
nameEx("estimationTime")
### * estimationTime

flush(stderr()); flush(stdout())

### Name: estimationTime
### Title: Get the model estimation time
### Aliases: estimationTime estimationTime,lcModel-method
###   estimationTime,lcModels-method estimationTime,list-method

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData)

estimationTime(model)
estimationTime(model, unit = 'mins')
estimationTime(model, unit = 'days')



cleanEx()
nameEx("externalMetric")
### * externalMetric

flush(stderr()); flush(stdout())

### Name: externalMetric,lcModel,lcModel-method
### Title: Compute external model metric(s)
### Aliases: externalMetric,lcModel,lcModel-method externalMetric
###   externalMetric,lcModels,missing-method
###   externalMetric,lcModels,character-method
###   externalMetric,lcModels,lcModel-method
###   externalMetric,lcModels,lcModels-method
###   externalMetric,list,lcModel-method

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model2 <- latrend(method, latrendData, nClusters = 2)
model3 <- latrend(method, latrendData, nClusters = 3)

if (require("mclustcomp")) {
  externalMetric(model2, model3, "adjustedRand")
}



cleanEx()
nameEx("fitted.lcModel")
### * fitted.lcModel

flush(stderr()); flush(stdout())

### Name: fitted.lcModel
### Title: Extract lcModel fitted values
### Aliases: fitted.lcModel

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData)
fitted(model)



cleanEx()
nameEx("fittedTrajectories")
### * fittedTrajectories

flush(stderr()); flush(stdout())

### Name: fittedTrajectories
### Title: Extract the fitted trajectories for all strata
### Aliases: fittedTrajectories fittedTrajectories,lcModel-method

### ** Examples

data(latrendData)
# Note: not a great example because the fitted trajectories
# are identical to the respective cluster trajectory
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData)
fittedTrajectories(model)

fittedTrajectories(model, at = time(model)[c(1, 2)])



cleanEx()
nameEx("formula.lcMethod")
### * formula.lcMethod

flush(stderr()); flush(stdout())

### Name: formula.lcMethod
### Title: Extract formula
### Aliases: formula.lcMethod

### ** Examples

method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
formula(method) # Y ~ Time



cleanEx()
nameEx("formula.lcModel")
### * formula.lcModel

flush(stderr()); flush(stdout())

### Name: formula.lcModel
### Title: Extract the formula of a lcModel
### Aliases: formula.lcModel

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, data = latrendData)
formula(model) # Y ~ Time



cleanEx()
nameEx("generateLongData")
### * generateLongData

flush(stderr()); flush(stdout())

### Name: generateLongData
### Title: Generate longitudinal test data
### Aliases: generateLongData

### ** Examples

longdata <- generateLongData(
  sizes = c(40, 70), id = "Id",
  cluster = ~poly(Time, 2, raw = TRUE),
  clusterCoefs = cbind(c(1, 2, 5), c(-3, 4, .2))
)

if (require("ggplot2")) {
  plotTrajectories(longdata, response = "Value", id = "Id", time = "Time")
}



cleanEx()
nameEx("getCall.lcModel")
### * getCall.lcModel

flush(stderr()); flush(stdout())

### Name: getCall.lcModel
### Title: Get the model call
### Aliases: getCall.lcModel
### Keywords: internal

### ** Examples

data(latrendData)
method <- lcMethodRandom("Y", id = "Id", time = "Time")
model <- latrend(method, latrendData)
getCall(model)



cleanEx()
nameEx("getLabel")
### * getLabel

flush(stderr()); flush(stdout())

### Name: getLabel
### Title: Extract the method label.
### Aliases: getLabel getLabel,lcMethod-method getLabel,lcModel-method

### ** Examples

method <- lcMethodLMKM(Y ~ Time, time = "Time")
getLabel(method) # ""

getLabel(update(method, label = "v2")) # "v2"



cleanEx()
nameEx("getLcMethod")
### * getLcMethod

flush(stderr()); flush(stdout())

### Name: getLcMethod
### Title: Get the method specification of a lcModel
### Aliases: getLcMethod getLcMethod,lcModel-method

### ** Examples

method <- lcMethodRandom("Y", id = "Id", time = "Time")
model <- latrend(method, latrendData)
getLcMethod(model)



cleanEx()
nameEx("getName")
### * getName

flush(stderr()); flush(stdout())

### Name: getName
### Title: Get the (short) name of the lcMethod or Model
### Aliases: getName getName,lcMethod-method getShortName
###   getShortName,lcMethod-method getName,lcModel-method
###   getShortName,lcModel-method

### ** Examples

method <- lcMethodLMKM(Y ~ Time)
getName(method) # "lm-kmeans"
method <- lcMethodLMKM(Y ~ Time)
getShortName(method) # "LMKM"



cleanEx()
nameEx("idVariable")
### * idVariable

flush(stderr()); flush(stdout())

### Name: idVariable
### Title: Extract the trajectory identifier variable
### Aliases: idVariable idVariable,lcMethod-method
###   idVariable,lcModel-method

### ** Examples

method <- lcMethodLMKM(Y ~ Time, id = "Traj")
idVariable(method) # "Traj"

method <- lcMethodRandom("Y", id = "Id", time = "Time")
model <- latrend(method, latrendData)
idVariable(model) # "Id"



cleanEx()
nameEx("ids")
### * ids

flush(stderr()); flush(stdout())

### Name: ids
### Title: Get the trajectory ids on which the model was fitted
### Aliases: ids

### ** Examples

data(latrendData)
method <- lcMethodRandom("Y", id = "Id", time = "Time")
model <- latrend(method, latrendData)
ids(model) # 1, 2, ..., 200



cleanEx()
nameEx("indexy")
### * indexy

flush(stderr()); flush(stdout())

### Name: [[,lcMethod-method
### Title: Retrieve and evaluate a lcMethod argument by name
### Aliases: [[,lcMethod-method $,lcMethod-method

### ** Examples

method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time", nClusters = 3)
method$nClusters # 3
m = lcMethodLMKM(Y ~ Time, id = "Id", time = "Time", nClusters = 5)
m[["nClusters"]] # 5

k = 2
m = lcMethodLMKM(Y ~ Time, id = "Id", time = "Time", nClusters = k)
m[["nClusters", eval=FALSE]] # k



cleanEx()
nameEx("initialize-lcMethod-method")
### * initialize-lcMethod-method

flush(stderr()); flush(stdout())

### Name: initialize,lcMethod-method
### Title: lcMethod initialization
### Aliases: initialize,lcMethod-method

### ** Examples

new("lcMethodLMKM", formula = Y ~ Time, id = "Id", time = "Time")



cleanEx()
nameEx("latrend-parallel")
### * latrend-parallel

flush(stderr()); flush(stdout())

### Name: latrend-parallel
### Title: Parallel computing using latrend
### Aliases: latrend-parallel

### ** Examples




cleanEx()
nameEx("latrend")
### * latrend

flush(stderr()); flush(stdout())

### Name: latrend
### Title: Cluster longitudinal data
### Aliases: latrend

### ** Examples

data(latrendData)
model <- latrend(lcMethodLMKM(Y ~ Time, id = "Id", time = "Time"), data = latrendData)

model <- latrend("lcMethodLMKM", formula = Y ~ Time, id = "Id", time = "Time", data = latrendData)

method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, data = latrendData, nClusters = 3)

model <- latrend(method, data = latrendData, nClusters = 3, seed = 1)



cleanEx()
nameEx("latrendBatch")
### * latrendBatch

flush(stderr()); flush(stdout())

### Name: latrendBatch
### Title: Cluster longitudinal data for a list of method specifications
### Aliases: latrendBatch

### ** Examples

data(latrendData)
refMethod <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
methods <- lcMethods(refMethod, nClusters = 1:2)
models <- latrendBatch(methods, data = latrendData)

# different dataset per method
models <- latrendBatch(
   methods,
   data = .(
     subset(latrendData, Time > .5),
     subset(latrendData, Time < .5)
   )
)




cleanEx()
nameEx("latrendBoot")
### * latrendBoot

flush(stderr()); flush(stdout())

### Name: latrendBoot
### Title: Cluster longitudinal data using bootstrapping
### Aliases: latrendBoot

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
bootModels <- latrendBoot(method, latrendData, samples = 10)

bootMAE <- metric(bootModels, name = "MAE")
mean(bootMAE)
sd(bootMAE)



cleanEx()
nameEx("latrendCV")
### * latrendCV

flush(stderr()); flush(stdout())

### Name: latrendCV
### Title: Cluster longitudinal data over k folds
### Aliases: latrendCV

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")

if (require("caret")) {
  model <- latrendCV(method, latrendData, folds = 5, seed = 1)

  model <- latrendCV(method, subset(latrendData, Time < .5), folds = 5)
}



cleanEx()
nameEx("latrendData")
### * latrendData

flush(stderr()); flush(stdout())

### Name: latrendData
### Title: Artificial longitudinal dataset comprising three classes
### Aliases: latrendData
### Keywords: datasets

### ** Examples

data(latrendData)

if (require("ggplot2")) {
  plotTrajectories(latrendData, id = "Id", time = "Time", response = "Y")

  # plot according to the reference class
  plotTrajectories(latrendData, id = "Id", time = "Time", response = "Y", cluster = "Class")
}



cleanEx()
nameEx("latrendRep")
### * latrendRep

flush(stderr()); flush(stdout())

### Name: latrendRep
### Title: Cluster longitudinal data repeatedly
### Aliases: latrendRep

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
models <- latrendRep(method, data = latrendData, .rep = 5) # 5 repeated runs

models <- latrendRep(method, data = latrendData, .seed = 1, .rep = 3)



cleanEx()
nameEx("lcFitMethods")
### * lcFitMethods

flush(stderr()); flush(stdout())

### Name: lcFitMethods
### Title: Method fit modifiers
### Aliases: lcFitMethods lcFitConverged-class lcFitConverged
###   lcFitRep-class lcFitRep lcFitRepMin lcFitRepMax

### ** Examples


data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time", nClusters = 2)
metaMethod <- lcFitConverged(method, maxRep = 10)
metaMethod
model <- latrend(metaMethod, latrendData)

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time", nClusters = 2)
repMethod <- lcFitRep(method, rep = 10, metric = "RSS", maximize = FALSE)
repMethod
model <- latrend(repMethod, latrendData)

minMethod <- lcFitRepMin(method, rep = 10, metric = "RSS")

maxMethod <- lcFitRepMax(method, rep = 10, metric = "ASW")



cleanEx()
nameEx("lcMethod-class")
### * lcMethod-class

flush(stderr()); flush(stdout())

### Name: lcMethod-class
### Title: lcMethod class
### Aliases: lcMethod-class lcMethod

### ** Examples

method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time", nClusters = 2)
method

method <- new("lcMethodLMKM", formula = Y ~ Time, id = "Id", time = "Time", nClusters = 2)

# get argument names
names(method)

# evaluate argument
method$nClusters

# create a copy with updated nClusters argument
method3 <- update(method, nClusters = 3)



cleanEx()
nameEx("lcMethodAkmedoids")
### * lcMethodAkmedoids

flush(stderr()); flush(stdout())

### Name: lcMethodAkmedoids
### Title: Specify AKMedoids method
### Aliases: lcMethodAkmedoids

### ** Examples

data(latrendData)
if (require("akmedoids")) {
  method <- lcMethodAkmedoids(response = "Y", time = "Time", id = "Id", nClusters = 3)
  model <- latrend(method, data = latrendData)
}



cleanEx()
nameEx("lcMethodCrimCV")
### * lcMethodCrimCV

flush(stderr()); flush(stdout())

### Name: lcMethodCrimCV
### Title: Specify a zero-inflated repeated-measures GBTM method
### Aliases: lcMethodCrimCV

### ** Examples

# This example is not tested because crimCV sometimes fails
# to converge and throws the error "object 'Frtr' not found"
## Not run: 
##D data(latrendData)
##D if (require("crimCV")) {
##D   method <- lcMethodCrimCV("Y", id = "Id", time = "Time", nClusters = 3, dpolyp = 1, init = 2)
##D   model <- latrend(method, data = subset(latrendData, Time > .5))
##D 
##D   if (require("ggplot2")) {
##D     plot(model)
##D   }
##D 
##D   data(TO1adj)
##D   method <- lcMethodCrimCV(response = "Offenses", time = "Offense", id = "Subject",
##D     nClusters = 2, dpolyp = 1, init = 2)
##D   model <- latrend(method, data = TO1adj[1:100, ])
##D }
## End(Not run)



cleanEx()
nameEx("lcMethodDtwclust")
### * lcMethodDtwclust

flush(stderr()); flush(stdout())

### Name: lcMethodDtwclust
### Title: Specify time series clustering via dtwclust
### Aliases: lcMethodDtwclust

### ** Examples

data(latrendData)

if (require("dtwclust")) {
  method <- lcMethodDtwclust("Y", id = "Id", time = "Time", nClusters = 3)
  model <- latrend(method, latrendData)
}



cleanEx()
nameEx("lcMethodFlexmix")
### * lcMethodFlexmix

flush(stderr()); flush(stdout())

### Name: lcMethodFlexmix
### Title: Method interface to flexmix()
### Aliases: lcMethodFlexmix

### ** Examples

data(latrendData)
if (require("flexmix")) {
  method <- lcMethodFlexmix(Y ~ Time, id = "Id", time = "Time", nClusters = 3)
  model <- latrend(method, latrendData)
}



cleanEx()
nameEx("lcMethodFlexmixGBTM")
### * lcMethodFlexmixGBTM

flush(stderr()); flush(stdout())

### Name: lcMethodFlexmixGBTM
### Title: Group-based trajectory modeling using flexmix
### Aliases: lcMethodFlexmixGBTM

### ** Examples

data(latrendData)
if (require("flexmix")) {
  method <- lcMethodFlexmixGBTM(Y ~ Time, id = "Id", time = "Time", nClusters = 3)
  model <- latrend(method, latrendData)
}



cleanEx()
nameEx("lcMethodFunFEM")
### * lcMethodFunFEM

flush(stderr()); flush(stdout())

### Name: lcMethodFunFEM
### Title: Specify a FunFEM method
### Aliases: lcMethodFunFEM

### ** Examples

data(latrendData)

if (require("funFEM") && require("fda")) {
  method <- lcMethodFunFEM("Y", id = "Id", time = "Time", nClusters = 3)
  model <- latrend(method, latrendData)

  method <- lcMethodFunFEM("Y",
   basis = function(time) {
      create.bspline.basis(time, nbasis = 10, norder = 4)
   }
  )
}



cleanEx()
nameEx("lcMethodFunction")
### * lcMethodFunction

flush(stderr()); flush(stdout())

### Name: lcMethodFunction
### Title: Specify a custom method based on a function
### Aliases: lcMethodFunction

### ** Examples

data(latrendData)
# Stratification based on the mean response level
clusfun <- function(data, response, id, time, ...) {
  clusters <- data.table::as.data.table(data)[, mean(Y) > 0, by = Id]$V1
  lcModelPartition(
    data = data,
    trajectoryAssignments = factor(
      clusters,
      levels = c(FALSE, TRUE),
      labels = c("Low", "High")
    ),
    response = response,
    time = time,
    id = id
  )
}
method <- lcMethodFunction(response = "Y", fun = clusfun, id = "Id", time = "Time")
model <- latrend(method, data = latrendData)



cleanEx()
nameEx("lcMethodGCKM")
### * lcMethodGCKM

flush(stderr()); flush(stdout())

### Name: lcMethodGCKM
### Title: Two-step clustering through latent growth curve modeling and
###   k-means
### Aliases: lcMethodGCKM

### ** Examples

data(latrendData)

if (require("lme4")) {
  method <- lcMethodGCKM(Y ~ (Time | Id), id = "Id", time = "Time", nClusters = 3)
  model <- latrend(method, latrendData)
}



cleanEx()
nameEx("lcMethodKML")
### * lcMethodKML

flush(stderr()); flush(stdout())

### Name: lcMethodKML
### Title: Specify a longitudinal k-means (KML) method
### Aliases: lcMethodKML

### ** Examples

data(latrendData)

if (require("kml")) {
  method <- lcMethodKML("Y", id = "Id", time = "Time", nClusters = 3)
  model <- latrend(method, latrendData)
}



cleanEx()
nameEx("lcMethodLMKM")
### * lcMethodLMKM

flush(stderr()); flush(stdout())

### Name: lcMethodLMKM
### Title: Two-step clustering through linear regression modeling and
###   k-means
### Aliases: lcMethodLMKM

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time", nClusters = 3)
model <- latrend(method, latrendData)



cleanEx()
nameEx("lcMethodLcmmGBTM")
### * lcMethodLcmmGBTM

flush(stderr()); flush(stdout())

### Name: lcMethodLcmmGBTM
### Title: Specify GBTM method
### Aliases: lcMethodLcmmGBTM

### ** Examples

data(latrendData)
if (rlang::is_installed("lcmm")) {
  method <- lcMethodLcmmGBTM(
    fixed = Y ~ Time,
    mixture = ~ 1,
   id = "Id",
   time = "Time",
   nClusters = 3
  )
  gbtm <- latrend(method, data = latrendData)
  summary(gbtm)

  method <- lcMethodLcmmGBTM(
    fixed = Y ~ Time,
    mixture = ~ Time,
    id = "Id",
    time = "Time",
    nClusters = 3
  )
}



cleanEx()
nameEx("lcMethodLcmmGMM")
### * lcMethodLcmmGMM

flush(stderr()); flush(stdout())

### Name: lcMethodLcmmGMM
### Title: Specify GMM method using lcmm
### Aliases: lcMethodLcmmGMM

### ** Examples

data(latrendData)

if (rlang::is_installed("lcmm")) {
  method <- lcMethodLcmmGMM(
    fixed = Y ~ Time,
    mixture = ~ Time,
    random = ~ 1,
    id = "Id",
    time = "Time", ,
    nClusters = 2
  )
  gmm <- latrend(method, data = latrendData)
  summary(gmm)

  method <- lcMethodLcmmGMM(
    fixed = Y ~ Time,
    mixture = ~ Time,
    random = ~ Time,
    id = "Id",
    time = "Time",
    nClusters = 3
  )
}



cleanEx()
nameEx("lcMethodMclustLLPA")
### * lcMethodMclustLLPA

flush(stderr()); flush(stdout())

### Name: lcMethodMclustLLPA
### Title: Longitudinal latent profile analysis
### Aliases: lcMethodMclustLLPA

### ** Examples

data(latrendData)
if (require("mclust")) {
  method <- lcMethodMclustLLPA("Y", id = "Id", time = "Time", nClusters = 3)
  model <- latrend(method, latrendData)
}



cleanEx()
nameEx("lcMethodMixAK_GLMM")
### * lcMethodMixAK_GLMM

flush(stderr()); flush(stdout())

### Name: lcMethodMixAK_GLMM
### Title: Specify a GLMM iwht a normal mixture in the random effects
### Aliases: lcMethodMixAK_GLMM

### ** Examples

data(latrendData)
# this example only runs when the mixAK package is installed
try({
 method <- lcMethodMixAK_GLMM(fixed = Y ~ 1, random = ~ Time,
  id = "Id", time = "Time", nClusters = 3)
 model <- latrend(method, latrendData)
 summary(model)
})



cleanEx()
nameEx("lcMethodMixTVEM")
### * lcMethodMixTVEM

flush(stderr()); flush(stdout())

### Name: lcMethodMixTVEM
### Title: Specify a MixTVEM
### Aliases: lcMethodMixTVEM

### ** Examples




cleanEx()
nameEx("lcMethodMixtoolsGMM")
### * lcMethodMixtoolsGMM

flush(stderr()); flush(stdout())

### Name: lcMethodMixtoolsGMM
### Title: Specify mixed mixture regression model using mixtools
### Aliases: lcMethodMixtoolsGMM

### ** Examples




cleanEx()
nameEx("lcMethodMixtoolsNPRM")
### * lcMethodMixtoolsNPRM

flush(stderr()); flush(stdout())

### Name: lcMethodMixtoolsNPRM
### Title: Specify non-parametric estimation for independent repeated
###   measures
### Aliases: lcMethodMixtoolsNPRM

### ** Examples

data(latrendData)

if (require("mixtools")) {
  method <- lcMethodMixtoolsNPRM("Y", id = "Id", time = "Time", nClusters = 3)
  model <- latrend(method, latrendData)
}



cleanEx()
nameEx("lcMethodRandom")
### * lcMethodRandom

flush(stderr()); flush(stdout())

### Name: lcMethodRandom
### Title: Specify a random-partitioning method
### Aliases: lcMethodRandom

### ** Examples

data(latrendData)
method <- lcMethodRandom(response = "Y", id = "Id", time = "Time")
model <- latrend(method, latrendData)

# uniform clusters
method <- lcMethodRandom(
  alpha = 1e3,
  nClusters = 3,
  response = "Y",
  id = "Id",
  time = "Time"
)

# single large cluster
method <- lcMethodRandom(
  alpha = c(100, 1, 1, 1),
  nClusters = 4,
  response = "Y",
  id = "Id",
  time = "Time"
)



cleanEx()
nameEx("lcMethodStratify")
### * lcMethodStratify

flush(stderr()); flush(stdout())

### Name: lcMethodStratify
### Title: Specify a stratification method
### Aliases: lcMethodStratify

### ** Examples

data(latrendData)
# Stratification based on the mean response level
method <- lcMethodStratify(
  "Y",
  mean(Y) > 0,
  clusterNames = c("Low", "High"),
  id = "Id",
  time = "Time"
)
model <- latrend(method, latrendData)
summary(model)

# Stratification function
stratfun <- function(trajdata) {
   trajmean <- mean(trajdata$Y)
   factor(
     trajmean > 1.7,
     levels = c(FALSE, TRUE),
     labels = c("Low", "High")
   )
}
method <- lcMethodStratify("Y", stratfun, id = "Id", time = "Time")

# Multiple clusters
stratfun3 <- function(trajdata) {
   trajmean <- mean(trajdata$Y)
   cut(
     trajmean,
     c(-Inf, .5, 2, Inf),
     labels = c("Low", "Medium", "High")
   )
}
method <- lcMethodStratify("Y", stratfun3, id = "Id", time = "Time")



cleanEx()
nameEx("lcMethods")
### * lcMethods

flush(stderr()); flush(stdout())

### Name: lcMethods
### Title: Generate a list of lcMethod objects
### Aliases: lcMethods

### ** Examples

data(latrendData)
baseMethod <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
methods <- lcMethods(baseMethod, nClusters = 1:6)

nclus <- 1:6
methods <- lcMethods(baseMethod, nClusters = nclus)

# list notation, useful for providing functions
methods <- lcMethods(baseMethod, nClusters = .(1, 3, 5))
length(methods) # 3



cleanEx()
nameEx("lcModelPartition")
### * lcModelPartition

flush(stderr()); flush(stdout())

### Name: lcModelPartition
### Title: Create a lcModel with pre-defined partitioning
### Aliases: lcModelPartition

### ** Examples

# comparing a model to the ground truth using the adjusted Rand index
data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData, nClusters = 3)

# extract the reference class from the Class column
trajLabels <- aggregate(Class ~ Id, head, 1, data = latrendData)
trajLabels$Cluster <- trajLabels$Class
refModel <- lcModelPartition(latrendData, response = "Y", trajectoryAssignments = trajLabels)

if (require("mclustcomp")) {
  externalMetric(model, refModel, "adjustedRand")
}



cleanEx()
nameEx("lcModels")
### * lcModels

flush(stderr()); flush(stdout())

### Name: lcModels
### Title: Construct a flat (named) list of lcModel objects
### Aliases: lcModels lcModels-class

### ** Examples

data(latrendData)
lmkmMethod <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
lmkmModel <- latrend(lmkmMethod, latrendData)
rngMethod <- lcMethodRandom("Y", id = "Id", time = "Time")
rngModel <- latrend(rngMethod, latrendData)

lcModels(lmkmModel, rngModel)

lcModels(defaults = c(lmkmModel, rngModel))



cleanEx()
nameEx("logLik.lcModel")
### * logLik.lcModel

flush(stderr()); flush(stdout())

### Name: logLik.lcModel
### Title: Extract the log-likelihood of a lcModel
### Aliases: logLik.lcModel

### ** Examples

data(latrendData)

if (rlang::is_installed("lcmm")) {
  method <- lcMethodLcmmGBTM(
    fixed = Y ~ Time,
    mixture = ~ 1,
    id = "Id",
    time = "Time",
    nClusters = 3
  )
  gbtm <- latrend(method, data = latrendData)
  logLik(gbtm)
}



cleanEx()
nameEx("max.lcModels")
### * max.lcModels

flush(stderr()); flush(stdout())

### Name: max.lcModels
### Title: Select the lcModel with the highest metric value
### Aliases: max.lcModels

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")

model1 <- latrend(method, latrendData, nClusters = 1)
model2 <- latrend(method, latrendData, nClusters = 2)
model3 <- latrend(method, latrendData, nClusters = 3)

models <- lcModels(model1, model2, model3)

if (require("clusterCrit")) {
  max(models, "Dunn")
}



cleanEx()
nameEx("metric")
### * metric

flush(stderr()); flush(stdout())

### Name: metric
### Title: Compute internal model metric(s)
### Aliases: metric metric,lcModel-method metric,list-method
###   metric,lcModels-method

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData)
metric(model, "WMAE")

if (require("clusterCrit")) {
  metric(model, c("WMAE", "Dunn"))
}



cleanEx()
nameEx("min.lcModels")
### * min.lcModels

flush(stderr()); flush(stdout())

### Name: min.lcModels
### Title: Select the lcModel with the lowest metric value
### Aliases: min.lcModels

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")

model1 <- latrend(method, latrendData, nClusters = 1)
model2 <- latrend(method, latrendData, nClusters = 2)
model3 <- latrend(method, latrendData, nClusters = 3)

models <- lcModels(model1, model2, model3)

min(models, "WMAE")



cleanEx()
nameEx("model.data.lcModel")
### * model.data.lcModel

flush(stderr()); flush(stdout())

### Name: model.data.lcModel
### Title: Extract the model data that was used for fitting
### Aliases: model.data.lcModel

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData)
model.data(model)



cleanEx()
nameEx("model.frame.lcModel")
### * model.frame.lcModel

flush(stderr()); flush(stdout())

### Name: model.frame.lcModel
### Title: Extract model training data
### Aliases: model.frame.lcModel

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, data = latrendData)
model.frame(model)



cleanEx()
nameEx("nClusters")
### * nClusters

flush(stderr()); flush(stdout())

### Name: nClusters
### Title: Number of clusters
### Aliases: nClusters

### ** Examples

data(latrendData)
method <- lcMethodRandom("Y", id = "Id", time = "Time", nClusters = 3)
model <- latrend(method, latrendData)
nClusters(model) # 3



cleanEx()
nameEx("nIds")
### * nIds

flush(stderr()); flush(stdout())

### Name: nIds
### Title: Number of trajectories
### Aliases: nIds

### ** Examples

data(latrendData)
method <- lcMethodRandom("Y", id = "Id", time = "Time")
model <- latrend(method, latrendData)
nIds(model)



cleanEx()
nameEx("names-lcMethod-method")
### * names-lcMethod-method

flush(stderr()); flush(stdout())

### Name: names,lcMethod-method
### Title: lcMethod argument names
### Aliases: names,lcMethod-method length,lcMethod-method

### ** Examples

method <- lcMethodLMKM(Y ~ Time)
names(method)
length(method)



cleanEx()
nameEx("nobs.lcModel")
### * nobs.lcModel

flush(stderr()); flush(stdout())

### Name: nobs.lcModel
### Title: Number of observations used for the lcModel fit
### Aliases: nobs.lcModel

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData)
nobs(model)



cleanEx()
nameEx("plot-lcModel-method")
### * plot-lcModel-method

flush(stderr()); flush(stdout())

### Name: plot-lcModel-method
### Title: Plot a lcModel
### Aliases: plot-lcModel-method plot,lcModel,ANY-method
###   plot,lcModel-method

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData, nClusters = 3)

if (require("ggplot2")) {
  plot(model)
}



cleanEx()
nameEx("plotClusterTrajectories")
### * plotClusterTrajectories

flush(stderr()); flush(stdout())

### Name: plotClusterTrajectories
### Title: Plot cluster trajectories
### Aliases: plotClusterTrajectories
###   plotClusterTrajectories,data.frame-method
###   plotClusterTrajectories,lcModel-method

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData, nClusters = 3)

if (require("ggplot2")) {
  plotClusterTrajectories(model)

  # show assigned trajectories
  plotClusterTrajectories(model, trajectories = TRUE)

  # show 95th percentile observation interval
  plotClusterTrajectories(model, trajectories = "95pct")

  # show observation standard deviation
  plotClusterTrajectories(model, trajectories = "sd")

  # show observation standard error
  plotClusterTrajectories(model, trajectories = "se")

  # show observation range
  plotClusterTrajectories(model, trajectories = "range")
}



cleanEx()
nameEx("plotFittedTrajectories")
### * plotFittedTrajectories

flush(stderr()); flush(stdout())

### Name: plotFittedTrajectories
### Title: Plot fitted trajectories of a lcModel
### Aliases: plotFittedTrajectories plotFittedTrajectories,lcModel-method

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData, nClusters = 3)

if (require("ggplot2")) {
  plotFittedTrajectories(model)
}



cleanEx()
nameEx("plotMetric")
### * plotMetric

flush(stderr()); flush(stdout())

### Name: plotMetric
### Title: Plot one or more internal metrics for all lcModels
### Aliases: plotMetric

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
methods <- lcMethods(method, nClusters = 1:3)
models <- latrendBatch(methods, latrendData)

if (require("ggplot2")) {
  plotMetric(models, "WMAE")
}

if (require("ggplot2") && require("clusterCrit")) {
  plotMetric(models, c("WMAE", "Dunn"))
}



cleanEx()
nameEx("plotTrajectories")
### * plotTrajectories

flush(stderr()); flush(stdout())

### Name: plotTrajectories
### Title: Plot the data trajectories
### Aliases: plotTrajectories plotTrajectories,data.frame-method
###   plotTrajectories,ANY-method plotTrajectories,lcModel-method

### ** Examples

data(latrendData)

if (require("ggplot2")) {
  plotTrajectories(latrendData, response = "Y", id = "Id", time = "Time")

  plotTrajectories(
    latrendData,
    response = quote(exp(Y)),
    id = "Id",
    time = "Time"
  )

  plotTrajectories(
    latrendData,
    response = "Y",
    id = "Id",
    time = "Time",
    cluster = "Class"
  )

  # compute cluster membership based on the mean being below 0
  assignments <- aggregate(Y ~ Id, latrendData, mean)$Y < 0
  plotTrajectories(
    latrendData,
    response = "Y",
    id = "Id",
    time = "Time",
    cluster = assignments
  )
}
data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData, nClusters = 3)

if (require("ggplot2")) {
  plotTrajectories(model)
}



cleanEx()
nameEx("postprob")
### * postprob

flush(stderr()); flush(stdout())

### Name: postprob
### Title: Posterior probability per fitted trajectory
### Aliases: postprob postprob,lcModel-method

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData)

postprob(model)

if (rlang::is_installed("lcmm")) {
  gmmMethod = lcMethodLcmmGMM(
    fixed = Y ~ Time,
    mixture = ~ Time,
    id = "Id",
    time = "Time",
    idiag = TRUE,
    nClusters = 2
  )
  gmmModel <- latrend(gmmMethod, data = latrendData)
  postprob(gmmModel)
}



cleanEx()
nameEx("predict.lcModel")
### * predict.lcModel

flush(stderr()); flush(stdout())

### Name: predict.lcModel
### Title: lcModel predictions
### Aliases: predict.lcModel

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData)

predFitted <- predict(model) # same result as fitted(model)

# Cluster trajectory of cluster A
predCluster <- predict(model, newdata = data.frame(Cluster = "A", Time = time(model)))

# Prediction for id S1 given cluster A membership
predId <- predict(model, newdata = data.frame(Cluster = "A", Id = "S1", Time = time(model)))

# Prediction matrix for id S1 for all clusters
predIdAll <- predict(model, newdata = data.frame(Id = "S1", Time = time(model)))



cleanEx()
nameEx("predictAssignments")
### * predictAssignments

flush(stderr()); flush(stdout())

### Name: predictAssignments
### Title: Predict the cluster assignments for new trajectories
### Aliases: predictAssignments predictAssignments,lcModel-method

### ** Examples

## Not run: 
##D data(latrendData)
##D if (require("kml")) {
##D   model <- latrend(method = lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
##D   predictAssignments(model, newdata = data.frame(Id = 999, Y = 0, Time = 0))
##D }
## End(Not run)



cleanEx()
nameEx("predictForCluster")
### * predictForCluster

flush(stderr()); flush(stdout())

### Name: predictForCluster
### Title: lcModel prediction conditional on a cluster
### Aliases: predictForCluster predictForCluster,lcModel-method

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData)

predictForCluster(
  model,
  newdata = data.frame(Time = c(0, 1)),
  cluster = "B"
)

# all fitted values under cluster B
predictForCluster(model, cluster = "B")



cleanEx()
nameEx("qqPlot")
### * qqPlot

flush(stderr()); flush(stdout())

### Name: qqPlot
### Title: Quantile-quantile plot
### Aliases: qqPlot qqPlot,lcModel-method

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time", nClusters = 3)
model <- latrend(method, latrendData)

if (require("ggplot2") && require("qqplotr")) {
  qqPlot(model)
}



cleanEx()
nameEx("responseVariable")
### * responseVariable

flush(stderr()); flush(stdout())

### Name: responseVariable
### Title: Extract the response variable
### Aliases: responseVariable responseVariable,lcMethod-method
###   responseVariable,lcModel-method

### ** Examples

method <- lcMethodLMKM(Y ~ Time)
responseVariable(method) # "Y"
data(latrendData)
method <- lcMethodRandom("Y", id = "Id", time = "Time")
model <- latrend(method, latrendData)
responseVariable(model) # "Y"



cleanEx()
nameEx("strip")
### * strip

flush(stderr()); flush(stdout())

### Name: strip
### Title: Reduce the lcModel memory footprint for serialization
### Aliases: strip strip,lcMethod-method strip,ANY-method
###   strip,lcModel-method

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData)
newModel <- strip(model)



cleanEx()
nameEx("subset.lcModels")
### * subset.lcModels

flush(stderr()); flush(stdout())

### Name: subset.lcModels
### Title: Subsetting a lcModels list based on method arguments
### Aliases: subset.lcModels

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")

model1 <- latrend(method, latrendData, nClusters = 1)
model2 <- latrend(method, latrendData, nClusters = 2)
model3 <- latrend(method, latrendData, nClusters = 3)

rngMethod <- lcMethodRandom("Y", id = "Id", time = "Time")
rngModel <- latrend(rngMethod, latrendData)

models <- lcModels(model1, model2, model3, rngModel)

subset(models, nClusters > 1 & .method == 'lmkm')



cleanEx()
nameEx("test")
### * test

flush(stderr()); flush(stdout())

### Name: test
### Title: Test a condition
### Aliases: test
### Keywords: internal

### ** Examples

## Not run: 
##D test('gt', 2 > 1)
##D test('eq', 1 + 1, 2)
##D test('lt', 2 < 1, onFail = "warn")
## End(Not run)



cleanEx()
nameEx("test.latrend")
### * test.latrend

flush(stderr()); flush(stdout())

### Name: test.latrend
### Title: Test the implementation of an lcMethod and associated lcModel
###   subclasses
### Aliases: test.latrend

### ** Examples

test.latrend("lcMethodRandom", tests = c("method", "basic"), clusterRecovery = "skip")



cleanEx()
nameEx("timeVariable")
### * timeVariable

flush(stderr()); flush(stdout())

### Name: timeVariable
### Title: Extract the time variable
### Aliases: timeVariable timeVariable,lcMethod-method
###   timeVariable,lcModel-method

### ** Examples

method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
timeVariable(method) # "Time"
data(latrendData)
method <- lcMethodRandom("Y", id = "Id", time = "Time")
model <- latrend(method, latrendData)
timeVariable(model) # "Time"



cleanEx()
nameEx("trajectoryAssignments")
### * trajectoryAssignments

flush(stderr()); flush(stdout())

### Name: trajectoryAssignments
### Title: Get the cluster membership of each trajectory
### Aliases: trajectoryAssignments trajectoryAssignments,matrix-method
###   trajectoryAssignments,lcModel-method

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData)
trajectoryAssignments(model)

# assign trajectories at random using weighted sampling
trajectoryAssignments(model, strategy = which.weight)



cleanEx()
nameEx("update.lcMethod")
### * update.lcMethod

flush(stderr()); flush(stdout())

### Name: update.lcMethod
### Title: Update a method specification
### Aliases: update.lcMethod

### ** Examples

method <- lcMethodLMKM(Y ~ 1, nClusters = 2)
method2 <- update(method, formula = ~ . + Time)

method3 <- update(method2, nClusters = 3)

k <- 2
method4 <- update(method, nClusters = k) # nClusters: k

method5 <- update(method, nClusters = k, .eval = TRUE) # nClusters: 2




cleanEx()
nameEx("update.lcModel")
### * update.lcModel

flush(stderr()); flush(stdout())

### Name: update.lcModel
### Title: Update a lcModel
### Aliases: update.lcModel

### ** Examples

data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model2 <- latrend(method, latrendData, nClusters = 2)

# fit for a different number of clusters
model3 <- update(model2, nClusters = 3)



cleanEx()
nameEx("which.weight")
### * which.weight

flush(stderr()); flush(stdout())

### Name: which.weight
### Title: Sample an index of a vector weighted by the elements
### Aliases: which.weight

### ** Examples

x = c(.01, .69, .3)
which.weight(x) #1, 2, or 3



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
