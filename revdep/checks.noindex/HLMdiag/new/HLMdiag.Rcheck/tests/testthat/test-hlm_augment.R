library("lme4", quietly = TRUE)
library("nlme", quietly = TRUE)

context("tests for hlm_augment, lmer models")


#sleepstudy models 
data(sleepstudy, package = 'lme4')

sleepstudy2 <- sleepstudy
sleepstudy2[7,1] <- NA
sleepstudy2[2,2] <- NA

sleep.lmer <- lme4::lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)
sleep.lme <- nlme::lme(Reaction ~ Days, random =  ~ Days|Subject, data = sleepstudy)
sleep.lmer.aug <- hlm_augment(sleep.lmer)
sleep.lme.aug <- hlm_augment(sleep.lme)

sleep.lmer.augG <- hlm_augment(sleep.lmer, level = "Subject")
sleep.lme.augG <- hlm_augment(sleep.lme, level = "Subject")

#chemistry scores models - 3 level 
data(Chem97, package = "mlmRev")  
Chem97 <- Chem97[1:257,]
chem.lmer <- lme4::lmer(score ~ gcsecnt + (1|lea/school), data = Chem97)
chem.lme <- nlme::lme(score ~ gcsecnt, random = ~1|lea/school, data = Chem97)
chem.lmer.aug <- suppressWarnings(hlm_augment(chem.lmer))
chem.lme.aug <- suppressWarnings(hlm_augment(chem.lme))

chem.lmer.augG <- hlm_augment(chem.lmer, level = "lea")
chem.lme.augG <- hlm_augment(chem.lme, level = "lea")
chem.lmer.augG2 <- hlm_augment(chem.lmer, level = "school:lea")
chem.lme.augG2 <- hlm_augment(chem.lme, level = "school") 

test_that("Number of rows is equal to number of observations when level equals 1 for lme4 models", {
  expect_equal(nrow(sleep.lmer.aug), nrow(sleep.lmer@frame))
  expect_equal(nrow(chem.lmer.aug), nrow(chem.lmer@frame))
})

test_that("Number of columns is correct when level equals 1 for lme4 models", {
  expect_equal(ncol(sleep.lmer.aug), 12 + ncol(sleep.lmer@frame))
  expect_equal(ncol(chem.lmer.aug), 12 + ncol(chem.lmer@frame))
})

test_that("Number of rows is equal to number of groups when level is set for lme4 models", {
  expect_equal(nrow(sleep.lmer.augG), length(unique(sleep.lmer@flist[["Subject"]])))
  expect_equal(nrow(chem.lmer.augG), length(unique(chem.lmer@flist[["lea"]])))
  expect_equal(nrow(chem.lmer.augG2), length(unique(chem.lmer@flist[["school:lea"]])))
})

test_that("Number of columns is correct when level is set for lme4 models", {
  expect_equal(ncol(sleep.lmer.augG), 10)
  expect_equal(ncol(chem.lmer.augG), 8)
  expect_equal(ncol(chem.lmer.augG2), 10)
})

test_that("Number of rows is correct for different na.actions for lme4 models", {
  sleep.lmerNA <- lme4::lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy2, na.action = na.exclude)
  sleep.lmerNA2 <- lme4::lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy2)
  
  expect_equal(nrow(hlm_augment(sleep.lmerNA, data = sleepstudy2)), 180)
  expect_equal(nrow(hlm_augment(sleep.lmerNA2, data = sleepstudy2)), 178)
})

context("tests for hlm_augment, lme models")

test_that("Number of rows is equal to number of observations when level equals 1 for nlme models", {
  expect_equal(nrow(sleep.lme.aug), nrow(sleep.lmer@frame))
  expect_equal(nrow(chem.lme.aug), nrow(chem.lmer@frame))
})

test_that("Number of columns is correct when level equals 1 for nlme models", {
  expect_equal(ncol(sleep.lme.aug), 12 + ncol(sleep.lmer@frame))
  expect_equal(ncol(chem.lme.aug), 12 + ncol(chem.lmer@frame))
})

test_that("Number of rows is equal to number of groups when level is set for nlme models", {
  expect_equal(nrow(sleep.lme.augG), length(unique(sleep.lmer@flist[["Subject"]])))
  expect_equal(nrow(chem.lme.augG), length(unique(chem.lmer@flist[["lea"]])))
  expect_equal(nrow(chem.lme.augG2), length(unique(chem.lmer@flist[["school:lea"]])))
})

test_that("Number of columns is correct when level is set for nlme models", {
  expect_equal(ncol(sleep.lme.augG), 10)
  expect_equal(ncol(chem.lme.augG), 8)
  expect_equal(ncol(chem.lmer.augG2), 10)
})

test_that("Number of rows is correct for different na.actions for nlme models", {
  sleep.lmeNA <- nlme::lme(Reaction ~ Days, random =  ~ Days|Subject, data = sleepstudy2, na.action = na.exclude)
  sleep.lmeNA2 <- nlme::lme(Reaction ~ Days, random =  ~ Days|Subject, data = sleepstudy2, na.action = na.omit)
  
  expect_equal(nrow(hlm_augment(sleep.lmeNA)), 180)
  expect_equal(nrow(hlm_augment(sleep.lmeNA2)), 178)
})

