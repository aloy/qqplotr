# Methods ####
# test method with deterministic result
mTest1 = lcMethodTestLMKM(nClusters = 1)
mTest2 = mTest = lcMethodTestLMKM(nClusters = 2)
mTest3 = lcMethodTestLMKM(nClusters = 3)
mTest4 = lcMethodTestLMKM(nClusters = 4)

# method that produces a random result each time
mRandom = lcMethodTestRandom()
# method that produces an error when fitted
mError = lcMethodError()

# Models ####
# hard cluster models
testModel1 = latrend(mTest, data = testLongData, nClusters = 1)
testModel2 = testModel = latrend(mTest, data = testLongData, nClusters = 2)
testModel3 = latrend(mTest, data = testLongData, nClusters = 3)
testModel4 = latrend(mTest, data = testLongData, nClusters = 4)

rngModel1 = latrend(mRandom, data = testLongData, nClusters = 1)
rngModel2 = rngModel = latrend(mRandom, data = testLongData, nClusters = 2)

# fuzzy cluster models
# fuzzyModel1 = latrend(lcMethodTestLcmmGMM(), testLongData, nClusters = 1)
# fuzzyModel2 = latrend(lcMethodTestLcmmGMM(), testLongData, nClusters = 2)
# fuzzyModel3 = latrend(lcMethodTestLcmmGMM(), testLongData, nClusters = 3)
