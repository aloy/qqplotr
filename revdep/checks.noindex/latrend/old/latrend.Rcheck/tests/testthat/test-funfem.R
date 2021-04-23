context('funfem')
skip_if_not_installed('funFEM')
rngReset()

library(funFEM)
data(CanadianWeather)
femData = CanadianWeather$dailyAv[,,'Temperature.C'] %>% t

test_that('default', {
  suppressWarnings({
    model = latrend(lcMethodTestFunFEM(), femData) %>%
      expect_valid_lcModel()
  })
})

test_that('many clusters', {
  suppressWarnings({
    model = latrend(lcMethodTestFunFEM(nClusters=4), femData) %>%
      expect_valid_lcModel()
  })
})

test_that('testLongData', {
  suppressWarnings({
    model = latrend(lcMethodTestFunFEM(), testLongData) %>%
      expect_valid_lcModel()
  })
})
