library("lme4", quietly = TRUE)
library("nlme", quietly = TRUE)

context("tests for pull_resid")

bdf <- nlme::bdf
bdf.lmer <- lme4::lmer(IQ.verb ~ ses + aritPOST + langPOST + schoolSES + 
                         (1|schoolNR), data = bdf)
bdf.lme <- nlme::lme(IQ.verb ~ ses + aritPOST + langPOST + schoolSES, 
                     random = ~1|schoolNR, data = bdf)

expect_warning(bdf.resids.lmer.raw <- hlm_resid(bdf.lmer))
expect_warning(bdf.resids.lmer.std <- hlm_resid(bdf.lmer, standardize = TRUE))

expect_warning(bdf.resids.lme.raw <- hlm_resid(bdf.lme))
expect_warning(bdf.resids.lme.std <- hlm_resid(bdf.lme, standardize = TRUE))


test_that("ls results match hlm_resid, lme4", {
  expect_equal(expect_warning(pull_resid(bdf.lmer)), 
               bdf.resids.lmer.raw$.ls.resid) 
  expect_equal(expect_warning(pull_resid(bdf.lmer, standardize = TRUE)), 
               bdf.resids.lmer.std$.std.ls.resid) 
})

test_that("ls results match hlm_resid, nlme", {
  expect_equal(expect_warning(pull_resid(bdf.lme)), 
               bdf.resids.lme.raw$.ls.resid) 
  expect_equal(expect_warning(pull_resid(bdf.lme, standardize = TRUE)), 
               bdf.resids.lme.std$.std.ls.resid) 
})

test_that("eb results match hlm_resid, lme4", {
  expect_equal(pull_resid(bdf.lmer, type = "eb"), 
               bdf.resids.lmer.raw$.resid) 
  expect_equal(pull_resid(bdf.lmer, type = "eb", standardize = TRUE), 
               bdf.resids.lmer.std$.std.resid) 
})

test_that("eb results match hlm_resid, nlme", {
  expect_equal(pull_resid(bdf.lme, type = "eb"), 
               bdf.resids.lme.raw$.resid) 
  expect_equal(pull_resid(bdf.lme, type = "eb", standardize = TRUE), 
               bdf.resids.lme.std$.std.resid) 
})

test_that("marginal results match hlm_resid, lme4", {
  expect_equal(pull_resid(bdf.lmer, type = "marginal"), 
               bdf.resids.lmer.raw$.mar.resid) 
  expect_equal(pull_resid(bdf.lmer, type = "marginal", standardize = TRUE), 
               bdf.resids.lmer.std$.chol.mar.resid) 
})

test_that("marginal results match hlm_resid, lme4", {
  expect_equal(pull_resid(bdf.lme, type = "marginal"), 
               bdf.resids.lme.raw$.mar.resid) 
  expect_equal(pull_resid(bdf.lme, type = "marginal", standardize = TRUE), 
               bdf.resids.lme.std$.chol.mar.resid) 
})