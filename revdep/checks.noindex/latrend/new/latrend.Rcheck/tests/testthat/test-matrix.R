context('matrix')
rngReset()

test_that('rowColumns', {
  m = matrix(1:12, ncol=3)
  expect_error(.rowColumns(m, 1))
  expect_equal(.rowColumns(m, c(1,1,1,1)), m[,1])
  expect_equal(.rowColumns(m, c(1,2,3,1)), m[cbind(1:4, c(1,2,3,1))])
  expect_error(.rowColumns(m, c(1,2,NA,3)))
  expect_error(.rowColumns(m, c(1,2,Inf,3)))
  expect_error(.rowColumns(m, c(1,4,1,1)))
  expect_error(.rowColumns(m, c(0,1,1,1)))
})


test_that('tsframe', {
  m = matrix(1:12, nrow=3)
  df = tsframe(m, response = 'Value')
  expect_is(df, 'data.frame')
  expect_named(df, c('id', 'time', 'Value'))
  expect_equal(nrow(df), length(m))
  expect_is(df$id, c('integer', 'factor'))
  expect_true(is.numeric(df$time))
  expect_is(df$Value, 'numeric')
  expect_equal(df$Value, as.numeric(t(m)))
})

test_that('tsframe with non-numeric colnames', {
  m = matrix(1:12, nrow=3)
  colnames(m) = paste0('Obs', 1:4)

  suppressWarnings({
    df = tsframe(m, response = 'Value')
  })
  expect_is(df, 'data.frame')
  expect_named(df, c('id', 'time', 'Value'))
  expect_equal(nrow(df), length(m))
  expect_is(df$id, c('integer', 'factor'))
  expect_true(is.numeric(df$time))
  expect_is(df$Value, 'numeric')
  expect_equal(df$Value, as.numeric(t(m)))
})

test_that('tsmatrix', {
  m = matrix(1:12, nrow=3)
  df = tsframe(m, response = 'Value')
  mat = tsmatrix(df, response = 'Value')

  expect_is(mat, 'matrix')
  expect_equal(nrow(mat), nrow(m))
  expect_equal(ncol(mat), ncol(m))
  expect_equal(as.numeric(mat), as.numeric(m))
})

test_that('tsmatrix with incomplete trajectories', {
  m = matrix(1:12, nrow=3)
  df = tsframe(m, response = 'Value')
  mat = tsmatrix(df[-1, ], response = 'Value')

  expect_equal(nrow(mat), nrow(m))
  expect_equal(ncol(mat), ncol(m))
})

test_that('trajectoryAssignments,matrix', {
  model = latrend(mTest, data = testLongData)
  pp = postprob(model)

  refLabels = trajectoryAssignments(model)
  trajLabels = trajectoryAssignments(pp)

  expect_equal(trajLabels, refLabels)
})
