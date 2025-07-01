context('akmedoids')
skip_if_not_installed('akmedoids', minimum_version = '1.3.0')
rngReset()
# akmedoids requires at least 3 clusters, so most tests cannot be applied
tests = c('method', 'cluster-three')

library(akmedoids)

# Generate data ####
data(traj, package = 'akmedoids')
capture.output({
  impTraj = data_imputation(traj, id_field = TRUE, method = 2, replace_with = 1, fill_zeros = FALSE)
})
trajMat = as.matrix(impTraj$CompleteData[-1])
rownames(trajMat) = impTraj$CompleteData[,1]
trajData = tsframe(
  trajMat,
  id = 'Id',
  time = 'Time',
  response = 'Value',
  times = seq_len(ncol(trajMat)),
  as.data.table = TRUE
)
trajData[, Cluster := LETTERS[as.integer(Id) %% 3 + 1]]

# Tests ####
test_that('default', {
  expect_true({
    test.latrend(
      'lcMethodAkmedoids',
      tests = tests,
      data = trajData,
      clusterRecovery = 'skip',
      args = list(seed = 1)
    )
  })
})
