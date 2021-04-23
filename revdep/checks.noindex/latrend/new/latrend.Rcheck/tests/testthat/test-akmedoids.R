context('akmedoids')
skip_if_not_installed('akmedoids', minimum_version = '1.3.0')
rngReset()

library(akmedoids)
data(traj, package = 'akmedoids')
capture.output({
  impTraj = akmedoids::data_imputation(traj, id_field=TRUE, method=2, replace_with=1, fill_zeros=FALSE)
})
trajMat = as.matrix(impTraj$CompleteData[-1])
rownames(trajMat) = impTraj$CompleteData[,1]

test_that('default', {
  suppressWarnings({
    model = latrend(lcMethodAkmedoids(response = 'Value'), trajMat)
  })
  expect_valid_lcModel(model)
})

test_that('many clusters', {
  suppressWarnings({
    model = latrend(lcMethodAkmedoids(response = 'Value', nClusters=10), trajMat)
  })
  expect_valid_lcModel(model)
})
