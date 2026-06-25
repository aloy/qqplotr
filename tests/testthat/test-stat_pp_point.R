test_that("stat_pp_point returns expected columns and row count", {
  p <- ggplot(smp_norm, aes(sample = x)) + stat_pp_point()
  d <- layer_data(p, 1)
  expect_true(all(c("x", "y") %in% names(d)))
  expect_equal(nrow(d), 100)
})

test_that("stat_pp_point x and y values are in [0, 1]", {
  p <- ggplot(smp_norm, aes(sample = x)) + stat_pp_point()
  d <- layer_data(p, 1)
  expect_true(all(d$x >= 0) && all(d$x <= 1))
  expect_true(all(d$y >= 0) && all(d$y <= 1))
})

test_that("stat_pp_point detrend shifts values toward zero", {
  p_raw <- ggplot(smp_norm, aes(sample = x)) + stat_pp_point()
  p_detrend <- ggplot(smp_norm, aes(sample = x)) + stat_pp_point(detrend = TRUE)
  d_raw <- layer_data(p_raw, 1)
  d_detrend <- layer_data(p_detrend, 1)
  expect_lt(mean(abs(d_detrend$y)), mean(abs(d_raw$y - 0.5)))
})

test_that("stat_pp_point works with explicit dparams", {
  p <- ggplot(smp_norm, aes(sample = x)) +
    stat_pp_point(dparams = list(mean = 0, sd = 1))
  d <- layer_data(p, 1)
  expect_equal(nrow(d), 100)
})
