test_that("stat_qq_band validates qtype", {
  expect_error(stat_qq_band(qtype = 0), "qtype")
  expect_error(stat_qq_band(qtype = 10), "qtype")
})

test_that("stat_qq_band validates conf", {
  expect_error(stat_qq_band(conf = -0.1), "conf")
  expect_error(stat_qq_band(conf = 1.1), "conf")
})

test_that("stat_qq_band validates B", {
  expect_error(stat_qq_band(B = -1), "B")
})

test_that("stat_qq_band returns expected columns and row count", {
  p <- ggplot(smp_norm, aes(sample = x)) + stat_qq_band()
  d <- layer_data(p, 1)
  expect_true(all(c("x", "ymin", "ymax") %in% names(d)))
  expect_equal(nrow(d), 100)
})

test_that("stat_qq_band upper >= lower for all rows", {
  p <- ggplot(smp_norm, aes(sample = x)) + stat_qq_band()
  d <- layer_data(p, 1)
  expect_true(all(d$ymax >= d$ymin))
})

test_that("stat_qq_band wider conf gives wider bands", {
  p_lo <- ggplot(smp_norm, aes(sample = x)) + stat_qq_band(conf = 0.50)
  p_hi <- ggplot(smp_norm, aes(sample = x)) + stat_qq_band(conf = 0.99)
  width_lo <- mean(layer_data(p_lo, 1)$ymax - layer_data(p_lo, 1)$ymin)
  width_hi <- mean(layer_data(p_hi, 1)$ymax - layer_data(p_hi, 1)$ymin)
  expect_gt(width_hi, width_lo)
})

test_that("stat_qq_band ks bands are wider than pointwise", {
  p_pw <- ggplot(smp_norm, aes(sample = x)) +
    stat_qq_band(bandType = "pointwise")
  p_ks <- ggplot(smp_norm, aes(sample = x)) + stat_qq_band(bandType = "ks")
  width_pw <- mean(layer_data(p_pw, 1)$ymax - layer_data(p_pw, 1)$ymin)
  width_ks <- mean(layer_data(p_ks, 1)$ymax - layer_data(p_ks, 1)$ymin)
  expect_gt(width_ks, width_pw)
})

test_that("stat_qq_band detrend centers pointwise bands at zero", {
  p <- ggplot(smp_norm, aes(sample = x)) + stat_qq_band(detrend = TRUE)
  d <- layer_data(p, 1)
  expect_equal((d$ymax + d$ymin) / 2, rep(0, nrow(d)), tolerance = 1e-10)
})

test_that("stat_qq_band boot type produces valid bands", {
  set.seed(42)
  p <- ggplot(smp_norm, aes(sample = x)) +
    stat_qq_band(bandType = "boot", B = 50)
  d <- layer_data(p, 1)
  expect_true(all(d$ymax >= d$ymin))
})

test_that("stat_qq_band ell type produces valid bands", {
  p <- ggplot(smp_norm, aes(sample = x)) + stat_qq_band(bandType = "ell")
  d <- layer_data(p, 1)
  expect_true(all(d$ymax >= d$ymin))
})

test_that("stat_qq_band ts type smoke test", {
  skip_on_cran()
  set.seed(42)
  p <- ggplot(smp_norm, aes(sample = x)) + stat_qq_band(bandType = "ts", B = 50)
  d <- layer_data(p, 1)
  expect_true(all(d$ymax >= d$ymin))
})

test_that("stat_qq_band handles discrete distributions", {
  p <- ggplot(smp_pois, aes(sample = x)) +
    stat_qq_band(distribution = "pois", dparams = list(lambda = 3))
  expect_no_error(layer_data(p, 1))
})
