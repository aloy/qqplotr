context('crimcv')
skip_if_not_installed('crimCV')
rngReset()

library(crimCV)
data(TO1adj)
subTO1adj = TO1adj[1:100, seq(1, ncol(TO1adj), by=2)]

test_that('default tau', {
  suppressWarnings({
    model = latrend(lcMethodTestCrimCVt(), subTO1adj) %>%
      expect_valid_lcModel()
  })

  plot(model, what='nu') %>%
    expect_is('ggplot')
})

test_that('default', {
  suppressWarnings({
    model = latrend(lcMethodTestCrimCV(), subTO1adj) %>%
      expect_valid_lcModel()
  })

  plot(model, what='nu') %>%
    expect_is('ggplot')
})

test_that('many clusters', {
  skip_on_cran()
  suppressWarnings({
    model = latrend(lcMethodTestCrimCVt(nClusters=3), subTO1adj) %>%
      expect_valid_lcModel()
  })
})
