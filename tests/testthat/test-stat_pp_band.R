test_that("stat_pp_band validates conf", {
  expect_error(stat_pp_band(conf = -0.1), "conf")
  expect_error(stat_pp_band(conf = 1.1), "conf")
})

test_that("stat_pp_band validates B", {
  expect_error(stat_pp_band(B = -1), "B")
})

test_that("stat_pp_band boot type returns expected columns", {
  set.seed(42)
  p <- ggplot(smp_norm, aes(sample = x)) +
    stat_pp_band(dparams = list(mean = 0, sd = 1), B = 50)
  d <- layer_data(p, 1)
  expect_true(all(c("x", "ymin", "ymax") %in% names(d)))
  expect_equal(nrow(d), 100)
})

test_that("stat_pp_band upper >= lower for all rows", {
  set.seed(42)
  p <- ggplot(smp_norm, aes(sample = x)) +
    stat_pp_band(dparams = list(mean = 0, sd = 1), B = 50)
  d <- layer_data(p, 1)
  expect_true(all(d$ymax >= d$ymin))
})

test_that("stat_pp_band wider conf gives wider bands", {
  set.seed(42)
  p_lo <- ggplot(smp_norm, aes(sample = x)) +
    stat_pp_band(dparams = list(mean = 0, sd = 1), conf = 0.50, B = 50)
  set.seed(42)
  p_hi <- ggplot(smp_norm, aes(sample = x)) +
    stat_pp_band(dparams = list(mean = 0, sd = 1), conf = 0.99, B = 50)
  width_lo <- mean(layer_data(p_lo, 1)$ymax - layer_data(p_lo, 1)$ymin)
  width_hi <- mean(layer_data(p_hi, 1)$ymax - layer_data(p_hi, 1)$ymin)
  expect_gt(width_hi, width_lo)
})

test_that("stat_pp_band ell type produces valid bands", {
  p <- ggplot(smp_norm, aes(sample = x)) +
    stat_pp_band(bandType = "ell", dparams = list(mean = 0, sd = 1))
  d <- layer_data(p, 1)
  expect_true(all(d$ymax >= d$ymin))
})

test_that("stat_pp_band detrend shifts bands toward zero", {
  set.seed(42)
  p_raw <- ggplot(smp_norm, aes(sample = x)) +
    stat_pp_band(dparams = list(mean = 0, sd = 1), B = 50)
  set.seed(42)
  p_detrend <- ggplot(smp_norm, aes(sample = x)) +
    stat_pp_band(dparams = list(mean = 0, sd = 1), B = 50, detrend = TRUE)
  d_raw <- layer_data(p_raw, 1)
  d_detrend <- layer_data(p_detrend, 1)
  expect_lt(
    mean(abs((d_detrend$ymax + d_detrend$ymin) / 2)),
    mean(abs((d_raw$ymax + d_raw$ymin) / 2 - 0.5))
  )
})

test_that("stat_pp_band handles discrete distributions", {
  p <- ggplot(smp_pois, aes(sample = x)) +
    stat_pp_band(distribution = "pois", dparams = list(lambda = 3), B = 50)
  expect_no_error(layer_data(p, 1))
})
