test_that("measurement invariance metric model", {
  fit <- measurement_invariance(
    x1:x3,
    x4:x6,
    x7:x9,
    data = lavaan::HolzingerSwineford1939,
    group = "school",
    invariance_level = "metric",
    return_result = T,
    quite = T # you can change this to metric
  )
  expect_true(is.data.frame(fit))
})
