context('mixAK')
skip_if_not_installed('mixAK')
rngReset()

test_that('default', {
  suppressWarnings({
    m = lcMethodTestMixAK_GLMM(PED = FALSE, nClusters = 2)
    model = latrend(m, testLongData)
    expect_valid_lcModel(model)
  })
})

test_that('multichain', {
  suppressWarnings({
    m = lcMethodTestMixAK_GLMM(PED = TRUE, nClusters = 2)
    model = latrend(m, testLongData)
    expect_valid_lcModel(model)
  })
})
