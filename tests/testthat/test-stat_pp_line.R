test_that("stat_pp_line returns two-row data with x and y", {
  p <- ggplot(smp_norm, aes(sample = x)) + stat_pp_line()
  d <- layer_data(p, 1)
  expect_equal(nrow(d), 2)
  expect_true(all(c("x", "y") %in% names(d)))
})

test_that("stat_pp_line default is identity line from (0,0) to (1,1)", {
  p <- ggplot(smp_norm, aes(sample = x)) + stat_pp_line()
  d <- layer_data(p, 1)
  expect_equal(d$x, c(0, 1))
  expect_equal(d$y, c(0, 1))
})

test_that("stat_pp_line detrend = TRUE gives flat line at y = 0", {
  p <- ggplot(smp_norm, aes(sample = x)) + stat_pp_line(detrend = TRUE)
  d <- layer_data(p, 1)
  expect_equal(d$y, c(0, 0))
})

test_that("stat_pp_line respects custom ab parameter", {
  p <- ggplot(smp_norm, aes(sample = x)) + stat_pp_line(ab = c(0.1, 0.8))
  d <- layer_data(p, 1)
  expect_equal(d$y[1], 0.1)
  expect_equal(d$y[2], 0.9)
})
