pkgname <- "latrend"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('latrend')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("as.list.lcMethod")
### * as.list.lcMethod

flush(stderr()); flush(stdout())

### Name: as.list.lcMethod
### Title: Extract the method arguments as a list
### Aliases: as.list.lcMethod

### ** Examples

data(latrendData)
method <- lcMethodKML("Y", id = "Id", time = "Time")
as.list(method)

as.list(method, args = c('id', 'time'))

# select arguments used by kml()
as.list(method, args = kml::kml)

# select arguments used by either kml() or parALGO()
as.list(method, args = c(kml::kml, kml::parALGO))



cleanEx()
nameEx("cash")
### * cash

flush(stderr()); flush(stdout())

### Name: $,lcMethod-method
### Title: Retrieve and evaluate a lcMethod argument by name
### Aliases: $,lcMethod-method

### ** Examples

m <- lcMethodKML(nClusters = 3)
m$nClusters # 3



cleanEx()
nameEx("clusterNames-set")
### * clusterNames-set

flush(stderr()); flush(stdout())

### Name: clusterNames<-
### Title: Update the cluster names
### Aliases: clusterNames<-

### ** Examples

data(latrendData)
model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
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
model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
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
model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
clusterProportions(model)



cleanEx()
nameEx("clusterSizes")
### * clusterSizes

flush(stderr()); flush(stdout())

### Name: clusterSizes
### Title: Number of strata per cluster
### Aliases: clusterSizes

### ** Examples

model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
clusterSizes(model)



cleanEx()
nameEx("clusterTrajectories")
### * clusterTrajectories

flush(stderr()); flush(stdout())

### Name: clusterTrajectories
### Title: Extract the cluster trajectories
### Aliases: clusterTrajectories clusterTrajectories,lcModel-method

### ** Examples

model <- latrend(method = lcMethodLcmmGMM(fixed = Y ~ Time, mixture = fixed),
  id = "Id", time = "Time", data = latrendData)
clusterTrajectories(model)

clusterTrajectories(model, at = c(0, .5, 1))



cleanEx()
nameEx("confusionMatrix")
### * confusionMatrix

flush(stderr()); flush(stdout())

### Name: confusionMatrix
### Title: Compute the posterior confusion matrix
### Aliases: confusionMatrix

### ** Examples

data(latrendData)
model = latrend(lcMethodLcmmGMM(
  fixed = Y ~ Time, mixture = ~ Time, random = ~ 1,
  id = "Id", time = "Time"),
  data=latrendData)
confusionMatrix(model)



cleanEx()
nameEx("createTestDataFold")
### * createTestDataFold

flush(stderr()); flush(stdout())

### Name: createTestDataFold
### Title: Create the test fold data for validation
### Aliases: createTestDataFold

### ** Examples

data(latrendData)
trainDataList <- createTrainDataFolds(latrendData, id = "Id", folds = 10)
testData1 <- createTestDataFold(latrendData, trainDataList[[1]], id = "Id")



cleanEx()
nameEx("createTestDataFolds")
### * createTestDataFolds

flush(stderr()); flush(stdout())

### Name: createTestDataFolds
### Title: Create all k test folds from the training data
### Aliases: createTestDataFolds

### ** Examples

data(latrendData)
trainDataList <- createTrainDataFolds(latrendData, folds = 10, id = "Id")
testDataList <- createTestDataFolds(latrendData, trainDataList)



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
trainFolds <- createTrainDataFolds(latrendData, folds = 10, id = "Id")

trainFolds <- createTrainDataFolds(latrendData, folds = 10, id = "Id", seed = 1)



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
model1 <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
model2 <- latrend(lcMethodLcmmGMM(fixed = Y ~ Time, mixture = ~ Time,
   id = "Id", time = "Time"), latrendData)
ari <- externalMetric(model1, model2, 'adjustedRand')



cleanEx()
nameEx("formula.lcMethod")
### * formula.lcMethod

flush(stderr()); flush(stdout())

### Name: formula.lcMethod
### Title: Extract formula
### Aliases: formula.lcMethod

### ** Examples

m <- lcMethodMixtoolsGMM(formula = Y ~ Time + (1 | Id))
formula(m) # Y ~ Time + (1 | Id)



cleanEx()
nameEx("generateLongData")
### * generateLongData

flush(stderr()); flush(stdout())

### Name: generateLongData
### Title: Generate longitudinal test data
### Aliases: generateLongData

### ** Examples

longdata <- generateLongData(sizes = c(40, 70), id = "Id",
                            cluster = ~poly(Time, 2, raw = TRUE),
                            clusterCoefs = cbind(c(1, 2, 5), c(-3, 4, .2)))
plotTrajectories(longdata, response = "Value", id = "Id", time = "Time")



cleanEx()
nameEx("getLcMethod")
### * getLcMethod

flush(stderr()); flush(stdout())

### Name: getLcMethod
### Title: Get the method specification of a lcModel
### Aliases: getLcMethod

### ** Examples

model = latrend(method=lcMethodKML("Y", id = "Id", time = "Time"), data=latrendData)
getLcMethod(model)



cleanEx()
nameEx("idVariable")
### * idVariable

flush(stderr()); flush(stdout())

### Name: idVariable
### Title: Extract the trajectory identifier variable
### Aliases: idVariable idVariable,lcMethod-method
###   idVariable,lcModel-method

### ** Examples

method <- lcMethodKML(id = "Traj")
idVariable(method) # "Traj"

model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
idVariable(model) # "Id"




cleanEx()
nameEx("ids")
### * ids

flush(stderr()); flush(stdout())

### Name: ids
### Title: Get the unique ids included in this model
### Aliases: ids

### ** Examples

model = latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
ids(model) # S1, S2, ..., S500



cleanEx()
nameEx("indexy")
### * indexy

flush(stderr()); flush(stdout())

### Name: [[,lcMethod-method
### Title: Retrieve and evaluate a lcMethod argument by name
### Aliases: [[,lcMethod-method

### ** Examples

m = lcMethodKML(nClusters = 5)
m[["nClusters"]] # 5

k = 2
m = lcMethodKML(nClusters = k)
m[["nClusters", eval=FALSE]] # k



cleanEx()
nameEx("latrend")
### * latrend

flush(stderr()); flush(stdout())

### Name: latrend
### Title: Cluster longitudinal data
### Aliases: latrend

### ** Examples

data(latrendData)
model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), data = latrendData)

method <- lcMethodKML("Y", id = "Id", time = "Time")
model <- latrend(method, data = latrendData, nClusters = 3)

model <- latrend(method, data = latrendData, nClusters = 3, seed = 1)



cleanEx()
nameEx("latrendBatch")
### * latrendBatch

flush(stderr()); flush(stdout())

### Name: latrendBatch
### Title: Cluster longitudinal data for a list of model specifications
### Aliases: latrendBatch

### ** Examples

data(latrendData)
methods <- lcMethods(lcMethodKML("Y", id = "Id", time = "Time"), nClusters = 1:3)
models <- latrendBatch(methods, data = latrendData)

models <- latrendBatch(lcMethods(lcMethodKML("Y", id = "Id", time = "Time"), nClusters = 1:2),
   data = .(subset(latrendData, Time > .5),
            subset(latrendData, Time < .5))) # different data per method




cleanEx()
nameEx("latrendBoot")
### * latrendBoot

flush(stderr()); flush(stdout())

### Name: latrendBoot
### Title: Cluster longitudinal data using bootstrapping
### Aliases: latrendBoot

### ** Examples

data(latrendData)
method <- lcMethodKML("Y", id = "Id", time = "Time")
model <- latrendBoot(method, latrendData, samples = 10)



cleanEx()
nameEx("latrendCV")
### * latrendCV

flush(stderr()); flush(stdout())

### Name: latrendCV
### Title: Cluster longitudinal data over k folds
### Aliases: latrendCV

### ** Examples

data(latrendData)
method <- lcMethodKML("Y", id = "Id", time = "Time")
model <- latrendCV(method, latrendData, folds = 5)

model <- latrendCV(method, subset(latrendData, Time < .5), folds = 5, seed = 1)



cleanEx()
nameEx("latrendRep")
### * latrendRep

flush(stderr()); flush(stdout())

### Name: latrendRep
### Title: Cluster longitudinal data repeatedly
### Aliases: latrendRep

### ** Examples

data(latrendData)
method <- lcMethodKML("Y", id = "Id", time = "Time")
models <- latrendRep(method, data = latrendData, .rep = 5) # 5 repeated runs

models <- latrendRep(method, data = latrendData, .seed = 1, .rep = 3)



cleanEx()
nameEx("lcMethod-class")
### * lcMethod-class

flush(stderr()); flush(stdout())

### Name: lcMethod-class
### Title: lcMethod class
### Aliases: lcMethod-class compose compose,lcMethod-method fit
###   fit,lcMethod-method getLabel getLabel,lcMethod-method getName
###   getName,lcMethod-method getShortName getShortName,lcMethod-method
###   length,lcMethod-method names,lcMethod-method preFit
###   preFit,lcMethod-method prepareData prepareData,lcMethod-method
###   postFit postFit,lcMethod-method validate validate,lcMethod-method

### ** Examples

getName(lcMethodKML("Y")) # "longitudinal k-means"
getShortName(lcMethodKML("Y")) # "KML"
m = lcMethodKML("Y")
names(m)



cleanEx()
nameEx("lcMethod.call")
### * lcMethod.call

flush(stderr()); flush(stdout())

### Name: lcMethod.call
### Title: Create a lcMethod object from a call
### Aliases: lcMethod.call

### ** Examples

data(latrendData)
lcMethodKML2 <- function(response = "Y", id = "Id", time = "Time", nClusters = 2, ...) {
  lcMethod.call("lcMethodKML", call = stackoverflow::match.call.defaults(),
    defaults = c(kml::kml, kml::parALGO),
    excludeArgs = c("object", "nbClusters", "parAlgo", "toPlot", "saveFreq"))
}
method <- lcMethodKML2(nClusters = 3)
latrend(method, data = latrendData)



cleanEx()
nameEx("lcMethodAkmedoids")
### * lcMethodAkmedoids

flush(stderr()); flush(stdout())

### Name: lcMethodAkmedoids
### Title: Specify AKMedoids method
### Aliases: lcMethodAkmedoids

### ** Examples

library(akmedoids)
data(latrendData)
method <- lcMethodAkmedoids(response = "Y", time = "Time", id = "Id", nClusters = 3)
model <- latrend(method, data = latrendData)



cleanEx()
nameEx("lcMethodCrimCV")
### * lcMethodCrimCV

flush(stderr()); flush(stdout())

### Name: lcMethodCrimCV
### Title: Specify a zero-inflated repeated-measures GBTM method
### Aliases: lcMethodCrimCV

### ** Examples




cleanEx()
nameEx("lcMethodCustom")
### * lcMethodCustom

flush(stderr()); flush(stdout())

### Name: lcMethodCustom
### Title: Specify a custom method based on a model function
### Aliases: lcMethodCustom

### ** Examples

data(latrendData)
# Stratification based on the mean response level
clusfun <- function(data, response, id, time, ...) {
   clusters <- data.table::as.data.table(data)[, mean(Y) > 0, by = Id]$V1
   lcModelCustom(data = data,
     trajectoryAssignments = factor(clusters, levels = c(FALSE, TRUE), labels = c("Low", "High")),
     response = response,
     time = time,
     id = id)
}
method <- lcMethodCustom(response = "Y", fun = clusfun, id = "Id", time = "Time")
model <- latrend(method, data = latrendData)



cleanEx()
nameEx("lcMethodDtwclust")
### * lcMethodDtwclust

flush(stderr()); flush(stdout())

### Name: lcMethodDtwclust
### Title: Specify time series clustering via dtwclust
### Aliases: lcMethodDtwclust

### ** Examples

library(dtwclust)
data(latrendData)
method <- lcMethodDtwclust("Y", id = "Id", time = "Time", nClusters = 3)
model <- latrend(method, latrendData)



cleanEx()
nameEx("lcMethodFlexmix")
### * lcMethodFlexmix

flush(stderr()); flush(stdout())

### Name: lcMethodFlexmix
### Title: Method interface to flexmix()
### Aliases: lcMethodFlexmix

### ** Examples

library(flexmix)
data(latrendData)
method <- lcMethodFlexmix(Y ~ Time, id = "Id", time = "Time", nClusters = 3)
model <- latrend(method, latrendData)



cleanEx()
nameEx("lcMethodFlexmixGBTM")
### * lcMethodFlexmixGBTM

flush(stderr()); flush(stdout())

### Name: lcMethodFlexmixGBTM
### Title: Group-based trajectory modeling using flexmix
### Aliases: lcMethodFlexmixGBTM

### ** Examples

library(flexmix)
data(latrendData)
method <- lcMethodFlexmixGBTM(Y ~ Time, id = "Id", time = "Time", nClusters = 3)
model <- latrend(method, latrendData)



cleanEx()
nameEx("lcMethodFunFEM")
### * lcMethodFunFEM

flush(stderr()); flush(stdout())

### Name: lcMethodFunFEM
### Title: Specify a FunFEM method
### Aliases: lcMethodFunFEM

### ** Examples

library(funFEM)
library(fda)
data(latrendData)
method <- lcMethodFunFEM("Y", id = "Id", time = "Time", nClusters = 3)
model <- latrend(method, latrendData)

method <- lcMethodFunFEM("Y",
   basis = function(time) {
      create.bspline.basis(time,
        nbasis = 10, norder = 4)
})



cleanEx()
nameEx("lcMethodGCKM")
### * lcMethodGCKM

flush(stderr()); flush(stdout())

### Name: lcMethodGCKM
### Title: Two-step clustering through linear mixed modeling and k-means
### Aliases: lcMethodGCKM

### ** Examples

library(lme4)
data(latrendData)
method <- lcMethodGCKM(Y ~ (Time | Id), id = "Id", time = "Time", nClusters = 3)
model <- latrend(method, latrendData)



cleanEx()
nameEx("lcMethodKML")
### * lcMethodKML

flush(stderr()); flush(stdout())

### Name: lcMethodKML
### Title: Specify a longitudinal k-means (KML) method
### Aliases: lcMethodKML

### ** Examples

library(kml)
data(latrendData)
method <- lcMethodKML("Y", id = "Id", time = "Time", nClusters = 3)
model <- latrend(method, latrendData)



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
method <- lcMethodLcmmGBTM(fixed = Y ~ Time, mixture = ~ 1,
   id = "Id", time = "Time", nClusters = 3)
gbtm <- latrend(method, data = latrendData)
summary(gbtm)

method <- lcMethodLcmmGBTM(fixed = Y ~ Time, mixture = ~ Time,
    id = "Id", time = "Time", nClusters = 3)



cleanEx()
nameEx("lcMethodLcmmGMM")
### * lcMethodLcmmGMM

flush(stderr()); flush(stdout())

### Name: lcMethodLcmmGMM
### Title: Specify GMM method using lcmm
### Aliases: lcMethodLcmmGMM

### ** Examples

data(latrendData)
method <- lcMethodLcmmGMM(fixed = Y ~ Time,
   mixture = ~ Time, random = ~ 1,
   id = "Id", time = "Time", , nClusters = 3)
gmm <- latrend(method, data = latrendData)
summary(gmm)

method <- lcMethodLcmmGMM(fixed = Y ~ Time,
    mixture = ~ Time, random = ~ Time,
    id = "Id", time = "Time", nClusters = 3)



cleanEx()
nameEx("lcMethodLongclust")
### * lcMethodLongclust

flush(stderr()); flush(stdout())

### Name: lcMethodLongclust
### Title: Specify Longclust method
### Aliases: lcMethodLongclust

### ** Examples

library(longclust)
data(latrendData)
method <- lcMethodLongclust("Y", id = "Id", time = "Time", nClusters = 3)
model <- latrend(method, latrendData)



cleanEx()
nameEx("lcMethodMclustLLPA")
### * lcMethodMclustLLPA

flush(stderr()); flush(stdout())

### Name: lcMethodMclustLLPA
### Title: Longitudinal latent profile analysis
### Aliases: lcMethodMclustLLPA

### ** Examples

library(mclust)
data(latrendData)
method <- lcMethodMclustLLPA("Y", id = "Id", time = "Time", nClusters = 3)
model <- latrend(method, latrendData)



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

library(mixtools)
data(latrendData)
method <- lcMethodMixtoolsNPRM("Y", id = "Id", time = "Time", nClusters = 3)
model <- latrend(method, latrendData)



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
method <- lcMethodRandom(alpha = 1e3, nClusters = 3, response = "Y", id = "Id", time = "Time")

# single large cluster
method <- lcMethodRandom(alpha = c(100, 1, 1, 1), nClusters = 4,
  response = "Y", id = "Id", time = "Time")



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
method <- lcMethodStratify("Y", mean(Y) > 0,
   clusterNames = c("Low", "High"), id = "Id", time = "Time")
model <- latrend(method, latrendData)
summary(model)

# Stratification function
stratfun <- function(trajdata) {
   trajmean <- mean(trajdata$Y)
   factor(trajmean > 1.7,
      levels = c(FALSE, TRUE),
      labels = c("Low", "High"))
}
method <- lcMethodStratify("Y", stratfun, id = "Id", time = "Time")

# Multiple clusters
stratfun3 <- function(trajdata) {
   trajmean <- mean(trajdata$Y)
   cut(trajmean,
      c(-Inf, .5, 2, Inf),
      labels = c("Low", "Medium", "High"))
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
baseMethod <- lcMethodKML("Y", id = "Id", time = "Time")
methods <- lcMethods(baseMethod, nClusters = 1:6)

nclus <- 1:6
methods <- lcMethods(baseMethod, nClusters = nclus)

methods <- lcMethods(baseMethod, nClusters = 3, center = .(mean, mean, median))
length(methods) # 3

methods <- lcMethods(baseMethod, nClusters = 1:3, center = .(mean, mean, median))
length(methods) # 9



cleanEx()
nameEx("lcModels")
### * lcModels

flush(stderr()); flush(stdout())

### Name: lcModels
### Title: Construct a flat (named) list of lcModel objects
### Aliases: lcModels

### ** Examples

data(latrendData)
kml <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
gmm <- latrend(lcMethodLcmmGMM(fixed = Y ~ Time, mixture = ~ Time,
   id = "Id", time = "Time"), latrendData)
lcModels(kml, gmm)

lcModels(defaults = c(kml, gmm))



cleanEx()
nameEx("max.lcModels")
### * max.lcModels

flush(stderr()); flush(stdout())

### Name: max.lcModels
### Title: Select the lcModel with the highest metric value
### Aliases: max.lcModels

### ** Examples

data(latrendData)
baseMethod <- lcMethodKML(response = "Y", id = "Id", time = "Time")
kml1 <- latrend(baseMethod, nClusters = 1, latrendData)
kml2 <- latrend(baseMethod, nClusters = 2, latrendData)
kml3 <- latrend(baseMethod, nClusters = 3, latrendData)
models <- lcModels(kml1, kml2, kml3)
max(models, 'WRSS')



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
model <- latrend(lcMethodLcmmGMM(fixed = Y ~ Time, mixture = ~ Time,
   id = "Id", time = "Time"), latrendData)
bic <- metric(model, "BIC")

ic <- metric(model, c("AIC", "BIC"))



cleanEx()
nameEx("min.lcModels")
### * min.lcModels

flush(stderr()); flush(stdout())

### Name: min.lcModels
### Title: Select the lcModel with the lowest metric value
### Aliases: min.lcModels

### ** Examples

data(latrendData)
baseMethod <- lcMethodKML(response = "Y", id = "Id", time = "Time")
kml1 <- latrend(baseMethod, nClusters = 1, latrendData)
kml2 <- latrend(baseMethod, nClusters = 2, latrendData)
kml3 <- latrend(baseMethod, nClusters = 3, latrendData)
models <- lcModels(kml1, kml2, kml3)
min(models, 'WRSS')



cleanEx()
nameEx("plotMetric")
### * plotMetric

flush(stderr()); flush(stdout())

### Name: plotMetric
### Title: Plot one or more internal metrics for all lcModels
### Aliases: plotMetric

### ** Examples

data(latrendData)
baseMethod <- lcMethodKML(response = "Y", id = "Id", time = "Time")
kml1 <- latrend(baseMethod, nClusters = 1, latrendData)
kml2 <- latrend(baseMethod, nClusters = 2, latrendData)
kml3 <- latrend(baseMethod, nClusters = 3, latrendData)
models <- lcModels(kml1, kml2, kml3)
plotMetric(models, "BIC", by = "nClusters", group = ".name")



cleanEx()
nameEx("plotTrajectories")
### * plotTrajectories

flush(stderr()); flush(stdout())

### Name: plotTrajectories
### Title: Plot trajectories
### Aliases: plotTrajectories plotTrajectories,data.frame-method
###   plotTrajectories,lcModel-method

### ** Examples

data(latrendData)
plotTrajectories(latrendData, response = "Y", id = "Id", time = "Time")

plotTrajectories(latrendData, response = quote(exp(Y)), id = "Id", time = "Time")



cleanEx()
nameEx("postprob")
### * postprob

flush(stderr()); flush(stdout())

### Name: postprob
### Title: Posterior probability per fitted id
### Aliases: postprob postprob,lcModel-method

### ** Examples

data(latrendData)
model <- latrend(lcMethodLcmmGMM(fixed = Y ~ Time, mixture = ~ Time,
   id = "Id", time = "Time"), data = latrendData)
postprob(model)



cleanEx()
nameEx("predict.lcModel")
### * predict.lcModel

flush(stderr()); flush(stdout())

### Name: predict.lcModel
### Title: lcModel predictions
### Aliases: predict.lcModel

### ** Examples

data(latrendData)
model <- latrend(lcMethodLcmmGMM(
   fixed = Y ~ Time, mixture = ~ Time,
   id = "Id", time = "Time"), latrendData)
predFitted <- predict(model) # same result as fitted(model)

# Cluster trajectory of cluster A
predCluster <- predict(model, newdata = data.frame(Cluster = "A", Time = time(model)))

# Prediction for id S1 given cluster A membership
predId <- predict(model, newdata = data.frame(Cluster = "A", Id = "S1", Time = time(model)))

# Prediction matrix for id S1 for all clusters
predIdAll <- predict(model, newdata = data.frame(Id = "S1", Time = time(model)))



cleanEx()
nameEx("responseVariable")
### * responseVariable

flush(stderr()); flush(stdout())

### Name: responseVariable
### Title: Extract the response variable
### Aliases: responseVariable responseVariable,lcMethod-method
###   responseVariable,lcModel-method

### ** Examples

method <- lcMethodKML("Value")
responseVariable(method) # "Value"

method <- lcMethodLcmmGBTM(fixed = Value ~ Time, mixture = ~ Time)
responseVariable(method) # "Value"

data(latrendData)
model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
responseVariable(model) # "Value"



cleanEx()
nameEx("subset.lcModels")
### * subset.lcModels

flush(stderr()); flush(stdout())

### Name: subset.lcModels
### Title: Subsetting a lcModels list based on method arguments
### Aliases: subset.lcModels

### ** Examples

data(latrendData)
mKML <- lcMethodKML(response = "Y", id = "Id", time = "Time")
kml1 <- latrend(mKML, nClusters = 1, latrendData)
kml2 <- latrend(mKML, nClusters = 2, latrendData)
kml3 <- latrend(mKML, nClusters = 3, latrendData)
gmm <- latrend(lcMethodLcmmGMM(fixed = Y ~ Time, mixture = ~ Time,
   id = "Id", time = "Time"), latrendData)
models <- lcModels(kml1, kml2, kml3, gmm)

subset(models, nClusters > 1 & .method == 'kml')



cleanEx()
nameEx("timeVariable")
### * timeVariable

flush(stderr()); flush(stdout())

### Name: timeVariable
### Title: Extract the time variable
### Aliases: timeVariable timeVariable,lcMethod-method
###   timeVariable,lcModel-method

### ** Examples

method <- lcMethodKML(time = "Assessment")
timeVariable(method) # "Assessment"

data(latrendData)
model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
idVariable(model) # "Id"



cleanEx()
nameEx("trajectories")
### * trajectories

flush(stderr()); flush(stdout())

### Name: trajectories
### Title: Extract the fitted trajectories for all strata
### Aliases: trajectories trajectories,lcModel-method

### ** Examples

data(latrendData)
model <- latrend(method = lcMethodKML("Y", id = "Id", time = "Time"), data = latrendData)
trajectories(model)

trajectories(model, at = c(0, .5, 1))



cleanEx()
nameEx("trajectoryAssignments")
### * trajectoryAssignments

flush(stderr()); flush(stdout())

### Name: trajectoryAssignments
### Title: Get the cluster membership of each trajectory
### Aliases: trajectoryAssignments trajectoryAssignments,lcModel-method

### ** Examples

data(latrendData)
model <- latrend(method = lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
trajectoryAssignments(model)

# assign ids at random using weighted sampling
trajectoryAssignments(model, strategy = which.weight)



cleanEx()
nameEx("update.lcMethod")
### * update.lcMethod

flush(stderr()); flush(stdout())

### Name: update.lcMethod
### Title: Update a method specification
### Aliases: update.lcMethod

### ** Examples

m <- lcMethodMixtoolsGMM(Value ~ 1)
m2 <- update(m, formula = ~ . + Time)

m3 <- update(m2, nClusters = 3)

k <- 2
m4 <- update(m, nClusters = k) # nClusters: k

m5 <- update(m, nClusters = k, .eval = TRUE) # nClusters: 2




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
