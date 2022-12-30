test_that(desc = "measurement invariance: metric", {
  summary <- measurement_invariance(
    model = "visual  =~ x1 + x2 + x3;
              textual =~ x4 + x5 + x6;
              speed   =~ x7 + x8 + x9",
    data = lavaan::HolzingerSwineford1939,
    group = "school",
    invariance_level = "metric",
    quite = TRUE,
    return_result = TRUE
  )
  expect_false(is.null(summary))
})

test_that(desc = "measurement invariance: scalar", {
  summary <- measurement_invariance(
    model = "visual  =~ x1 + x2 + x3;
              textual =~ x4 + x5 + x6;
              speed   =~ x7 + x8 + x9",
    data = lavaan::HolzingerSwineford1939,
    group = "school",
    invariance_level = "scalar",
    quite = TRUE,
    return_result = TRUE
  )
  expect_false(is.null(summary))
})
