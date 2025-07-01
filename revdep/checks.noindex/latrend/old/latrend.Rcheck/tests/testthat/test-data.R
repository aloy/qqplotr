test_that('PAP.adh structure', {
  data(PAP.adh)

  expect_is(PAP.adh, 'data.frame')
  expect_named(PAP.adh, c('Patient', 'Week', 'UsageHours', 'Group'))
  expect_is(PAP.adh$Patient, 'integer')
  expect_is(PAP.adh$Week, 'integer')
  expect_is(PAP.adh$UsageHours, 'numeric')
  expect_is(PAP.adh$Group, 'factor')
})

test_that('PAP.adh content', {
  with(PAP.adh, {
    expect_gte(min(UsageHours), 0)
    expect_true(noNA(Patient))
    expect_equal(uniqueN(Patient), 301)
    expect_equal(nlevels(Group), 3)
    expect_equal(uniqueN(Group), nlevels(Group))
    expect_true(noNA(Week))
    expect_true(noNA(UsageHours))
    expect_equal(min(Week), 1)
    expect_equal(max(Week), 13)
  })
})

test_that('PAP.adh1y', {
  data(PAP.adh1y)

  expect_is(PAP.adh1y, 'data.frame')
  expect_named(PAP.adh1y, c('Patient', 'Biweek', 'MaxDay', 'UsageHours', 'Group'))
  expect_is(PAP.adh1y$Patient, 'factor')
  expect_is(PAP.adh1y$Biweek, 'integer')
  expect_is(PAP.adh1y$MaxDay, 'integer')
  expect_is(PAP.adh1y$UsageHours, 'numeric')
  expect_is(PAP.adh1y$Group, 'factor')
})

test_that('PAP.adh1y content', {
  with(PAP.adh1y, {
    expect_gte(min(UsageHours), 0)
    expect_true(noNA(Patient))
    expect_equal(uniqueN(Patient), nlevels(Patient))
    expect_equal(uniqueN(Group), nlevels(Group))
    expect_true(noNA(Biweek))
    expect_true(noNA(UsageHours))
    expect_equal(min(Biweek), 1)
    expect_equal(max(Biweek), 26)
  })
})


test_that('guessResponseVariable', {
  expect_equal(
    .guessResponseVariable(PAP.adh, id = 'Patient', time = 'Week', cluster = 'Group'),
    'UsageHours'
  )

  expect_equal(
    .guessResponseVariable(PAP.adh, id = 'Patient', time = 'Week'),
    'UsageHours'
  )

  expect_equal(
    .guessResponseVariable(PAP.adh),
    'UsageHours'
  )
})
