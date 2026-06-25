test_that("stat_qq_line validates qtype", {
  expect_error(stat_qq_line(qtype = 0), "qtype")
  expect_error(stat_qq_line(qtype = 10), "qtype")
})

test_that("stat_qq_line validates qprobs", {
  expect_error(stat_qq_line(qprobs = c(0.25)), "qprobs")
  expect_error(stat_qq_line(qprobs = c(-0.1, 0.75)), "qprobs")
  expect_error(stat_qq_line(qprobs = c(0.25, 1.1)), "qprobs")
})

test_that("stat_qq_line returns two-row data with x and y", {
  p <- ggplot(smp_norm, aes(sample = x)) + stat_qq_line()
  d <- layer_data(p, 1)
  expect_equal(nrow(d), 2)
  expect_true(all(c("x", "y") %in% names(d)))
})

test_that("stat_qq_line identity = TRUE gives slope 1 intercept 0", {
  p <- ggplot(smp_norm, aes(sample = x)) + stat_qq_line(identity = TRUE)
  d <- layer_data(p, 1)
  slope <- diff(d$y) / diff(d$x)
  intercept <- d$y[1] - slope * d$x[1]
  expect_equal(slope, 1)
  expect_equal(intercept, 0)
})

test_that("stat_qq_line detrend = TRUE gives flat line at y = 0", {
  p <- ggplot(smp_norm, aes(sample = x)) + stat_qq_line(detrend = TRUE)
  d <- layer_data(p, 1)
  expect_equal(d$y, c(0, 0))
})
