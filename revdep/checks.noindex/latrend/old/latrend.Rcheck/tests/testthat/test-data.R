test_that('PAP.adh structure', {
  data(PAP.adh)

  expect_is(PAP.adh, 'data.frame')
  expect_named(PAP.adh, c('Patient', 'Biweek', 'MaxDay', 'UsageHours', 'Group'))
  expect_is(PAP.adh$Patient, 'factor')
  expect_is(PAP.adh$Biweek, 'integer')
  expect_is(PAP.adh$MaxDay, 'integer')
  expect_is(PAP.adh$UsageHours, 'numeric')
  expect_is(PAP.adh$Group, 'factor')
})

test_that('PAP.adh content', {
  expect_gte(min(PAP.adh$UsageHours), 0)
  expect_true(noNA(PAP.adh$Patient))
  expect_equal(uniqueN(PAP.adh$Patient), nlevels(PAP.adh$Patient))
  expect_equal(uniqueN(PAP.adh$Group), nlevels(PAP.adh$Group))
  expect_true(noNA(PAP.adh$Biweek))
  expect_true(noNA(PAP.adh$UsageHours))
  expect_equal(min(PAP.adh$Biweek), 1)
  expect_equal(max(PAP.adh$Biweek), 26)
})

test_that('PAP.adh1y', {
  data(PAP.adh)
  data(PAP.adh1y)

  expect_lt(nlevels(PAP.adh1y$Patient), nlevels(PAP.adh$Patient))
  expect_lt(nlevels(PAP.adh1y$Group), nlevels(PAP.adh$Group))

  # check for empty levels
  expect_equal(uniqueN(PAP.adh1y$Patient), nlevels(PAP.adh1y$Patient))
  expect_equal(uniqueN(PAP.adh1y$Group), nlevels(PAP.adh1y$Group))

  expect_true(all(levels(PAP.adh1y) %in% levels(PAP.adh)))
})
