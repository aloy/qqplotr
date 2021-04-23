library(testthat)
library(HLMdiag)
library(lme4)
library(nlme)

data(sleepstudy, package = 'lme4')

sleepstudy2 <- sleepstudy
sleepstudy2[7,1] <- NA
sleepstudy2[2,2] <- NA


test_check("HLMdiag")
