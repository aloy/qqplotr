context('matrix')
rngReset()

test_that('rowColumns', {
  m = matrix(1:12, ncol=3)
  expect_error(rowColumns(m, 1))
  expect_equal(rowColumns(m, c(1,1,1,1)), m[,1])
  expect_equal(rowColumns(m, c(1,2,3,1)), m[cbind(1:4, c(1,2,3,1))])
  expect_error(rowColumns(m, c(1,2,NA,3)))
  expect_error(rowColumns(m, c(1,2,Inf,3)))
  expect_error(rowColumns(m, c(1,4,1,1)))
  expect_error(rowColumns(m, c(0,1,1,1)))
})


test_that('meltRepeatedMeasures', {
  m = matrix(1:12, nrow=3)
  df = meltRepeatedMeasures(m, response = 'Value')
  expect_is(df, 'data.frame')
  expect_named(df, c('Traj', 'Assessment', 'Value'))
  expect_equal(nrow(df), length(m))
  expect_is(df$Traj, c('integer', 'factor'))
  expect_true(is.numeric(df$Assessment))
  expect_is(df$Value, 'numeric')
  expect_equal(df$Value, as.numeric(t(m)))
})

test_that('meltRepeatedMeasures with non-numeric colnames', {
  m = matrix(1:12, nrow=3)
  colnames(m) = paste0('Obs', 1:4)

  suppressWarnings({
    df = meltRepeatedMeasures(m, response = 'Value')
  })
  expect_is(df, 'data.frame')
  expect_named(df, c('Traj', 'Assessment', 'Value'))
  expect_equal(nrow(df), length(m))
  expect_is(df$Traj, c('integer', 'factor'))
  expect_true(is.numeric(df$Assessment))
  expect_is(df$Value, 'numeric')
  expect_equal(df$Value, as.numeric(t(m)))
})

test_that('dcastRepeatedMeasures', {
  m = matrix(1:12, nrow=3)
  df = meltRepeatedMeasures(m, response = 'Value')
  mat = dcastRepeatedMeasures(df, response = 'Value')

  expect_is(mat, 'matrix')
  expect_equal(nrow(mat), nrow(m))
  expect_equal(ncol(mat), ncol(m))
  expect_equal(as.numeric(mat), as.numeric(m))
})
