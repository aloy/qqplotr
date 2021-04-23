library("lme4", quietly = TRUE)
library("nlme", quietly = TRUE)


context("tests for hlm_influence")

#sleepstudy
data(sleepstudy, package = 'lme4')
sleep.lmer <- lme4::lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)  
sleep.lme <- nlme::lme(Reaction ~ Days, random =  ~ Days|Subject, data = sleepstudy)

#chemistry 
data(Chem97, package = "mlmRev")  
Chem97 <- Chem97[1:257,]
chem.lmer <- lme4::lmer(score ~ gcsecnt + (1|lea/school), data = Chem97)
chem.lme <- nlme::lme(score ~ gcsecnt, random = ~1|lea/school, data = Chem97)


test_that("Number of rows and columns are correct for default approximations for lme4 models", {
  #sleepstudy
  sleep.lmer.infl <- hlm_influence(sleep.lmer)
  expect_equal(ncol(sleep.lmer.infl), 6 + ncol(sleep.lmer@frame))
  expect_equal(nrow(sleep.lmer.infl), nrow(sleep.lmer@frame))
  
  #chemistry
  chem.lmer.infl <- hlm_influence(chem.lmer)
  expect_equal(ncol(chem.lmer.infl), 6 + ncol(chem.lmer@frame)) 
  expect_equal(nrow(chem.lmer.infl), nrow(chem.lmer@frame))
})

test_that("Number of rows and columns are correct for full refits for lme4 models", {
  skip_on_cran()
  #sleepstudy
  sleep.lmer.infl2 <- suppressWarnings(hlm_influence(sleep.lmer, approx = FALSE))
  expect_equal(ncol(sleep.lmer.infl2), 6 + ncol(sleep.lmer@frame) + length(HLMdiag:::varcomp.mer(sleep.lmer)))
  
  #chemistry
  chem.lmer.infl2 <- suppressWarnings(suppressMessages(hlm_influence(chem.lmer, approx = FALSE)))
  expect_equal(ncol(chem.lmer.infl2), 6 + ncol(chem.lmer@frame) + length(HLMdiag:::varcomp.mer(chem.lmer)))
})

test_that("Number of rows and columns are correct for default approximations for nlme models", {
  #sleepstudy
  sleep.lme.infl <- hlm_influence(sleep.lme)
  expect_equal(ncol(sleep.lme.infl), 6 + ncol(sleep.lme$data))
  expect_equal(nrow(sleep.lme.infl), nrow(sleep.lme$data))
  
  #chemistry
  chem.lme.infl <- hlm_influence(chem.lme)
  expect_equal(ncol(chem.lme.infl), 6 + ncol(chem.lmer@frame)) #same here 
  expect_equal(nrow(chem.lme.infl), nrow(chem.lme$data))
  
})

test_that("Number of rows and columns are correct for full refits for nlme models", {
  skip_on_cran()
  
  #sleepstudy
  sleep.lme.infl2 <- hlm_influence(sleep.lme, approx = FALSE)
  expect_equal(ncol(sleep.lme.infl2), 6 + ncol(sleep.lmer@frame) + length(HLMdiag:::varcomp.lme(sleep.lme)))
  
  #chemistry
  chem.lme.infl2 <- hlm_influence(chem.lme, approx = FALSE)
  expect_equal(ncol(chem.lme.infl2), 6 + ncol(chem.lmer@frame) + length(HLMdiag:::varcomp.lme(chem.lme)))
  
})

test_that("Number of columns is correct when leverage is specified for lme4 models", {
  #sleepstudy
  sleep.lmer.infl <- hlm_influence(sleep.lmer, leverage = c("overall", "fixef", "ranef", "ranef.uc"))
  expect_equal(ncol(sleep.lmer.infl), 9 + ncol(sleep.lmer@frame))
  
  #chemistry 
  chem.lmer.infl <- hlm_influence(chem.lmer, leverage = c("overall", "fixef", "ranef", "ranef.uc"))
  expect_equal(ncol(chem.lmer.infl), 9 + ncol(chem.lmer@frame))
})

test_that("Number of columns is correct when leverage is specified for nlme models", {
  #sleepstudy
  sleep.lme.infl <- hlm_influence(sleep.lme, leverage = c("overall", "fixef", "ranef", "ranef.uc"))
  expect_equal(ncol(sleep.lme.infl), 9 + ncol(sleep.lmer@frame))
  
  #chemistry 
  chem.lme.infl <- hlm_influence(chem.lme, leverage = c("overall", "fixef", "ranef", "ranef.uc"))
  expect_equal(ncol(chem.lme.infl), 9 + ncol(chem.lmer@frame)) 
})


test_that("Number of rows is correct when level is specified for lme4 models", {
  #sleepstudy
  sleep.lmer.infl <- hlm_influence(sleep.lmer, level = "Subject")
  expect_equal(nrow(sleep.lmer.infl), length(unique(sleep.lmer@flist[["Subject"]])))
  
  #chemistry
  chem.lmer.infl <- hlm_influence(chem.lmer, level = "lea")
  expect_equal(nrow(chem.lmer.infl), length(unique(chem.lmer@flist[["lea"]])))
  
  chem.lmer.infl2 <- hlm_influence(chem.lmer, level = "school:lea")
  expect_equal(nrow(chem.lmer.infl2), length(unique(chem.lmer@flist[["school:lea"]])))

})

test_that("Number of rows is correct when level is specified for nlme models", {
  #sleepstudy
  sleep.lme.infl <- hlm_influence(sleep.lme, level = "Subject")
  expect_equal(nrow(sleep.lme.infl), length(unique(sleep.lme$groups[["Subject"]])))
  
  #chemistry
  chem.lme.infl <- hlm_influence(chem.lme, level = "lea")
  expect_equal(nrow(chem.lme.infl), length(unique(chem.lme$groups[["lea"]])))
  
  chem.lme.infl2 <- hlm_influence(chem.lme, level = "school")
  expect_equal(nrow(chem.lme.infl2), length(unique(chem.lme$groups[["school"]])))
})


test_that("Influence diagnostic columns match output from influence functions for lme4 models", {
  #sleepstudy
  sleep.lmer.infl <- hlm_influence(sleep.lmer)
  expect_equal(sleep.lmer.infl$cooksd, cooks.distance(sleep.lmer))
  expect_equal(sleep.lmer.infl$mdffits, mdffits(sleep.lmer))
  expect_equal(sleep.lmer.infl$covtrace, covtrace(sleep.lmer))
  expect_equal(sleep.lmer.infl$covratio, covratio(sleep.lmer))
  expect_equal(sleep.lmer.infl$leverage.overall, leverage(sleep.lmer)[,1])

  
  #chemistry
  chem.lmer.infl <- hlm_influence(chem.lmer)
  expect_equal(chem.lmer.infl$cooksd, cooks.distance(chem.lmer))
  expect_equal(chem.lmer.infl$mdffits, mdffits(chem.lmer))
  expect_equal(chem.lmer.infl$covtrace, covtrace(chem.lmer))
  expect_equal(chem.lmer.infl$covratio, covratio(chem.lmer))
  expect_equal(chem.lmer.infl$leverage.overall, leverage(chem.lmer)[,1])

})

test_that("Influence diagnostic columns match output from influence functions for nlme models", {
  #sleepstudy
  sleep.lme.infl <- hlm_influence(sleep.lme)
  expect_equal(sleep.lme.infl$cooksd, cooks.distance(sleep.lme))
  expect_equal(sleep.lme.infl$mdffits, mdffits(sleep.lme))
  expect_equal(sleep.lme.infl$covtrace, covtrace(sleep.lme))
  expect_equal(sleep.lme.infl$covratio, covratio(sleep.lme))
  expect_equal(sleep.lme.infl$leverage.overall, leverage(sleep.lme)[,1])
  
  #chemistry
  chem.lme.infl <- hlm_influence(chem.lme)
  expect_equal(chem.lme.infl$cooksd, cooks.distance(chem.lme))
  expect_equal(chem.lme.infl$mdffits, mdffits(chem.lme))
  expect_equal(chem.lme.infl$covtrace, covtrace(chem.lme))
  expect_equal(chem.lme.infl$covratio, covratio(chem.lme))
  expect_equal(chem.lme.infl$leverage.overall, leverage(chem.lme)[,1])  
  
})

test_that("Number of rows and columns are correct when delete is specified for lme4 models", {
  #sleepstudy
  expect_warning(sleep.lmer.infl <- hlm_influence(sleep.lmer, delete = c(1,10,13)))
  expect_equal(nrow(sleep.lmer.infl), 1)
  expect_equal(ncol(sleep.lmer.infl), 4)

  expect_warning(hlm_influence(sleep.lmer, delete = c(1,10,13), leverage = c("ranef", "ranef.uc")))
  expect_warning(hlm_influence(sleep.lmer, delete = c(1,10,13), leverage = "ranef"))

  #chemistry 
  expect_warning(chem.lmer.infl <- hlm_influence(chem.lmer, delete = c(2, 8, 78)))
  expect_equal(nrow(chem.lmer.infl), 1)
  expect_equal(ncol(chem.lmer.infl), 4)
})

test_that("Number of rows and columns are correct when delete is specified for nlme models", {
  #sleepstudy
  expect_warning(sleep.lme.infl <- hlm_influence(sleep.lme, delete = c(1,10,13)))
  expect_equal(nrow(sleep.lme.infl), 1)
  expect_equal(ncol(sleep.lme.infl), 4)
  
  expect_warning(hlm_influence(sleep.lme, delete = c(1,10,13), leverage = c("ranef", "ranef.uc")))
  expect_warning(hlm_influence(sleep.lme, delete = c(1,10,13), leverage = "ranef"))
  
  #chemistry
  expect_warning(chem.lme.infl <- hlm_influence(chem.lme, delete = c(2, 8, 78)))
  expect_equal(nrow(chem.lme.infl), 1)
  expect_equal(ncol(chem.lme.infl), 4)
})

