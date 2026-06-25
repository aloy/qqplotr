test_that("geom_qq_band validates qtype", {
  expect_error(geom_qq_band(qtype = 0), "qtype")
})

test_that("geom_qq_band validates conf", {
  expect_error(geom_qq_band(conf = 1.5), "conf")
})

test_that("geom_qq_band validates B", {
  expect_error(geom_qq_band(B = -1), "B")
})

test_that("geom_qq_band produces same data as stat_qq_band", {
  p_stat <- ggplot(smp_norm, aes(sample = x)) + stat_qq_band()
  p_geom <- ggplot(smp_norm, aes(sample = x)) + geom_qq_band()
  d_stat <- layer_data(p_stat, 1)[, c("x", "ymin", "ymax")]
  d_geom <- layer_data(p_geom, 1)[, c("x", "ymin", "ymax")]
  expect_equal(d_stat, d_geom)
})
