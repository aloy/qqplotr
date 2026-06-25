test_that("stat_qq_point validates qtype", {
  expect_error(stat_qq_point(qtype = 0), "qtype")
  expect_error(stat_qq_point(qtype = 10), "qtype")
})

test_that("stat_qq_point validates qprobs", {
  expect_error(stat_qq_point(qprobs = c(0.25)), "qprobs")
  expect_error(stat_qq_point(qprobs = c(-0.1, 0.75)), "qprobs")
  expect_error(stat_qq_point(qprobs = c(0.25, 1.1)), "qprobs")
})

test_that("stat_qq_point returns expected columns and row count", {
  p <- ggplot(smp_norm, aes(sample = x)) + stat_qq_point()
  d <- layer_data(p, 1)
  expect_true(all(c("x", "y") %in% names(d)))
  expect_equal(nrow(d), 100)
})

test_that("stat_qq_point detrend shifts values toward zero", {
  p_raw <- ggplot(smp_norm, aes(sample = x)) + stat_qq_point()
  p_detrend <- ggplot(smp_norm, aes(sample = x)) + stat_qq_point(detrend = TRUE)
  expect_lt(
    mean(abs(layer_data(p_detrend, 1)$y)),
    mean(abs(layer_data(p_raw, 1)$y))
  )
})

test_that("stat_qq_point works with non-normal distributions", {
  p <- ggplot(smp_exp, aes(sample = x)) +
    stat_qq_point(distribution = "exp", dparams = list(rate = 1))
  d <- layer_data(p, 1)
  expect_equal(nrow(d), 100)
  expect_true(all(c("x", "y") %in% names(d)))
})

test_that("stat_qq_point auto-estimates MLE parameters", {
  p <- ggplot(smp_norm, aes(sample = x)) + stat_qq_point(distribution = "norm")
  expect_no_error(layer_data(p, 1))
})
