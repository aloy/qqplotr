context('crimcv')
# skip on CI because crimCV fitting fails at random with error "object 'Frtr' not found"
skip_on_ci()
skip_on_cran()
skip_if_not_installed('crimCV')
rngReset()
tests = setdiff(DEFAULT_LATREND_TESTS, c('data-na', 'data-varlen'))

library(crimCV)

make.crimcv = function(...) {
  lcMethodCrimCV(
    ...,
    dpolyp = 2,
    dpolyl = 1,
    init = 5,
    seed = 1
  )
}

# Create test dataset
data(TO1adj)
subTO1adj = TO1adj[1:99, seq(1, ncol(TO1adj), by = 2)]
testData = tsframe(
  subTO1adj,
  id = 'Id',
  time = 'Time',
  response = 'Value',
  times = seq_len(ncol(subTO1adj)),
  as.data.table = TRUE
) %>%
  .[, Cluster := rep(LETTERS[1:3], each = 33 * ncol(subTO1adj))]

# Tests ####
test_that('zip', {
  expect_true({
    test.latrend(
      'lcMethodCrimCV',
      instantiator = make.crimcv,
      data = testData,
      tests = tests,
      args = list(model = 'ZIP'),
      clusterRecovery = 'skip'
    )
  })
})

test_that('zipt', {
  expect_true({
    test.latrend(
      'lcMethodCrimCV',
      instantiator = make.crimcv,
      data = testData,
      tests = tests,
      args = list(model = 'ZIPt'),
      clusterRecovery = 'skip'
    )
  })
})
