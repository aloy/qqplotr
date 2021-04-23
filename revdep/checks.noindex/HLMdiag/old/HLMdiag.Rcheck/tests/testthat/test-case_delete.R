library("lme4", quietly = TRUE)
library("nlme", quietly = TRUE)

context("basic tests for case_delete") 

#there is a model failed to converge warning (need to suppress this)

#sleepstudy models 
data(sleepstudy, package = 'lme4')
sleep.lmer <- lme4::lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)
sleep.lme <- nlme::lme(Reaction ~ Days, random =  ~ Days|Subject, data = sleepstudy)

sleep.lmer.case <- suppressWarnings(case_delete(sleep.lmer))
sleep.lme.case <- case_delete(sleep.lme)

sleep.lmer.caseG <- case_delete(sleep.lmer, level = "Subject")
sleep.lme.caseG <- case_delete(sleep.lme, level = "Subject")

sleep.lmer.caseD <- case_delete(sleep.lmer, delete = c(1, 20, 100))
sleep.lme.caseD <- case_delete(sleep.lmer, delete = c(1, 20, 100))

sleep.lmer.caseGD <- case_delete(sleep.lmer, level = "Subject", delete = "308")
sleep.lme.caseGD <- case_delete(sleep.lme, level = "Subject", delete = "308") 

#chemistry scores models - 3 level 

data(Chem97, package = "mlmRev")  
Chem97 <- Chem97[1:257,]
chem.lmer <- lme4::lmer(score ~ gcsecnt + (1|lea/school), data = Chem97)
chem.lme <- nlme::lme(score ~ gcsecnt, random = ~1|lea/school, data = Chem97)

chem.lmer.case <- suppressMessages(case_delete(chem.lmer)) 
chem.lme.case <- case_delete(chem.lme)

chem.lmer.caseG <- suppressMessages(case_delete(chem.lmer, level = "lea")) 
chem.lme.caseG <- case_delete(chem.lme, level = "lea")

chem.lmer.caseG2 <- suppressMessages(case_delete(chem.lmer, level = "school:lea")) 
chem.lme.caseG2 <- case_delete(chem.lme, level = "school") 

chem.lmer.caseGD <- case_delete(chem.lmer, level = "lea", delete = c("1", "2"))
chem.lme.caseGD <- case_delete(chem.lme, level = "lea", delete = c("1", "2"))

chem.lmer.caseGD2 <- case_delete(chem.lmer, level = "school:lea", delete = c("1:1", "5:2"))
chem.lme.caseGD2 <- case_delete(chem.lme, level = "school", delete = c("1/1", "2/5"))

test_that("Correct influence diagnostics are returned for lme4 models", {
  expect_equal(length(sleep.lmer.case), 9)
  expect_equal(length(chem.lmer.case), 9)
})

test_that("Correct influence diagnostics are returned for nlme models", {
  expect_equal(length(sleep.lme.case), 9)
  expect_equal(length(chem.lme.case), 9)
})


context("case_delete, original fixed effects")

test_that("Original fixed effects matches output from fixef for lme4 models",{
  #sleepstudy
  expect_equal(sleep.lmer.case$fixef.original, fixef(sleep.lmer))
  expect_equal(sleep.lmer.caseG$fixef.original, fixef(sleep.lmer))
  
  #chemistry  
  expect_equal(chem.lmer.case$fixef.original, fixef(chem.lmer))
  expect_equal(chem.lmer.caseG$fixef.original, fixef(chem.lmer))
  expect_equal(chem.lmer.caseG2$fixef.original, fixef(chem.lmer))
})

test_that("Original fixed effects matches output from fixef for nlme models",{
  #sleepstudy
  expect_equal(sleep.lme.case$fixef.original, fixef(sleep.lme))
  expect_equal(sleep.lme.caseG$fixef.original, fixef(sleep.lme))
  
  #chemistry  
  expect_equal(chem.lme.case$fixef.original, fixef(chem.lme))
  expect_equal(chem.lme.caseG$fixef.original, fixef(chem.lme))
  expect_equal(chem.lme.caseG2$fixef.original, fixef(chem.lme))
})

context("case_delete, original random effects")

#passes
test_that("Original predicted random effects match output from ranef for lme4 models", {
  
  #sleepstudy
  expect_equal(sleep.lmer.case$ranef.original[[1]], ranef(sleep.lmer)[[1]][,1])
  expect_equal(sleep.lmer.case$ranef.original[[2]], ranef(sleep.lmer)[[1]][,2])
  
  expect_equal(sleep.lmer.caseG$ranef.original[[1]], ranef(sleep.lmer)[[1]][,1])
  expect_equal(sleep.lmer.caseG$ranef.original[[2]], ranef(sleep.lmer)[[1]][,2])
  
  #chemistry 
  expect_equal(chem.lmer.case$ranef.original[[1]][,1], ranef(chem.lmer)[[1]][,1]) 
  expect_equal(chem.lmer.case$ranef.original[[2]][,1], ranef(chem.lmer)[[2]][,1]) 
  
  expect_equal(chem.lmer.caseG$ranef.original[[1]][,1], ranef(chem.lmer)[[1]][,1])
  expect_equal(chem.lmer.caseG$ranef.original[[2]][,1], ranef(chem.lmer)[[2]][,1])
  
  expect_equal(chem.lmer.caseG2$ranef.original[[1]][,1], ranef(chem.lmer)[[1]][,1])
  expect_equal(chem.lmer.caseG2$ranef.original[[2]][,1], ranef(chem.lmer)[[2]][,1])
  
})


test_that("Original predicted random effects match output from ranef for nlme models", {
  #sleepstudy
  expect_equal(sleep.lme.case$ranef.original[[1]], ranef(sleep.lme)[,1])
  expect_equal(sleep.lme.case$ranef.original[[2]], ranef(sleep.lme)[,2])
  expect_equal(sleep.lme.caseG$ranef.original[[1]], ranef(sleep.lme)[,1])
  expect_equal(sleep.lme.caseG$ranef.original[[2]], ranef(sleep.lme)[,2])
  
  #sleepstudy
  expect_equal(chem.lme.case$ranef.original[[1]][,1], ranef(chem.lme)[[1]][,1]) 
  expect_equal(chem.lme.case$ranef.original[[2]][,1], ranef(chem.lme)[[2]][,1]) 
  
  expect_equal(chem.lme.caseG$ranef.original[[1]][,1], ranef(chem.lme)[[1]][,1])
  expect_equal(chem.lme.caseG$ranef.original[[2]][,1], ranef(chem.lme)[[2]][,1])
  
  expect_equal(chem.lme.caseG2$ranef.original[[1]][,1], ranef(chem.lme)[[1]][,1])
  expect_equal(chem.lme.caseG2$ranef.original[[2]][,1], ranef(chem.lme)[[2]][,1])
})

context("case_delete, original variance-covariance matrix")

#passes 
test_that("Original variance-covariance matrix matches output from vcov for lme4 models", {
  #sleepstudy
  expect_equal(sleep.lmer.case$vcov.original, as.matrix(vcov(sleep.lmer)))
  expect_equal(sleep.lmer.caseG$vcov.original, as.matrix(vcov(sleep.lmer)))
  
  #chemistry 
  expect_equal(chem.lmer.case$vcov.original, as.matrix(vcov(chem.lmer)))
  expect_equal(chem.lmer.caseG$vcov.original, as.matrix(vcov(chem.lmer)))
  expect_equal(chem.lmer.caseG2$vcov.original, as.matrix(vcov(chem.lmer)))
})


test_that("Original variance-covariance matrix matches output from vcov for nlme models", {
  #sleepstudy
  expect_equal(sleep.lme.case$vcov.original, as.matrix(vcov(sleep.lme)))
  expect_equal(sleep.lme.caseG$vcov.original, as.matrix(vcov(sleep.lme)))
  
  #chemistry
  expect_equal(chem.lme.case$vcov.original, as.matrix(vcov(chem.lme)))
  expect_equal(chem.lme.caseG$vcov.original, as.matrix(vcov(chem.lme)))
  expect_equal(chem.lme.caseG2$vcov.original, as.matrix(vcov(chem.lme)))
})

context("case_delete, original variance components")

test_that("Variance components match output from varcomp for lme4 models", {
  #sleepstudy
  expect_equal(sleep.lmer.case$varcomp.original, varcomp.mer(sleep.lmer))
  expect_equal(sleep.lmer.caseG$varcomp.original, varcomp.mer(sleep.lmer))
  
  #chemistry 
  expect_equal(chem.lmer.case$varcomp.original, varcomp.mer(chem.lmer))
  expect_equal(chem.lmer.caseG$varcomp.original, varcomp.mer(chem.lmer))
  expect_equal(chem.lmer.caseG2$varcomp.original, varcomp.mer(chem.lmer))
})

test_that("Variance components match output from varcomp for nlme models", {
  #sleepstudy
  expect_equal(sleep.lme.case$varcomp.original, varcomp.lme(sleep.lme))
  expect_equal(sleep.lme.caseG$varcomp.original, varcomp.lme(sleep.lme))
  
  #chemistry
  expect_equal(chem.lme.case$varcomp.original, varcomp.lme(chem.lme))
  expect_equal(chem.lme.caseG$varcomp.original, varcomp.lme(chem.lme))
  expect_equal(chem.lme.caseG2$varcomp.original, varcomp.lme(chem.lme))
})

context("case_delete, fixed effects after deletion")
#passed
test_that("Dimensions of fixed effects after deletion are correct for single case deletion for lme4 models", {
  #number of rows is number of observations, number of columns is number of fixed effects plus one
  
  #sleepstudy
  expect_equal(nrow(sleep.lmer.case$fixef.delete), nrow(sleep.lmer@frame))
  expect_equal(ncol(sleep.lmer.case$fixef.delete), 1 + length(fixef(sleep.lmer)))
  expect_equal(length(sleep.lmer.caseD$fixef.delete), length(fixef(sleep.lmer)))
  
  #chemistry 
  expect_equal(nrow(chem.lmer.case$fixef.delete), nrow(chem.lmer@frame))
  expect_equal(ncol(chem.lmer.case$fixef.delete), 1 + length(fixef(chem.lmer)))
})

test_that("Dimensions of fixed effects after deletion are correct for single case deletion for nlme models", {
  #sleepstudy
  expect_equal(nrow(sleep.lme.case$fixef.delete), nrow(sleep.lme$groups))
  expect_equal(ncol(sleep.lme.case$fixef.delete), 1 + length(fixef(sleep.lme)))
  expect_equal(length(sleep.lme.caseD$fixef.delete), length(fixef(sleep.lme)))
  
  #chemistry
  expect_equal(nrow(chem.lme.case$fixef.delete), nrow(chem.lme$groups))
  expect_equal(ncol(chem.lme.case$fixef.delete), 1 + length(fixef(chem.lme)))
})


test_that("Dimensions of fixed effects after deletion are correct for group deletion for lme4 models", {
  #number of rows is number of groups, number of columns is number of fixed effects 
  #sleepstudy
  expect_equal(nrow(sleep.lmer.caseG$fixef.delete), length(unique(sleep.lmer@flist[["Subject"]])))
  expect_equal(ncol(sleep.lmer.caseG$fixef.delete), length(fixef(sleep.lmer)))
  expect_equal(length(sleep.lmer.caseGD$fixef.delete), length(fixef(sleep.lmer)))
  
  #chemistry
  expect_equal(nrow(chem.lmer.caseG$fixef.delete), length(unique(chem.lmer@flist[["lea"]])))
  expect_equal(ncol(chem.lmer.caseG$fixef.delete), length(fixef(chem.lmer)))
  expect_equal(nrow(chem.lmer.caseG2$fixef.delete), length(unique(chem.lmer@flist[["school:lea"]])))
  expect_equal(ncol(chem.lmer.caseG2$fixef.delete), length(fixef(chem.lmer)))
})

test_that("Dimensions of fixed effects after deletion are correct for group deletion for nlme models", {
  #sleepstudy
  expect_equal(nrow(sleep.lme.caseG$fixef.delete), length(unique(sleep.lme$groups$Subject)))
  expect_equal(ncol(sleep.lme.caseG$fixef.delete), length(fixef(sleep.lme)))
  expect_equal(length(sleep.lme.caseGD$fixef.delete), length(fixef(sleep.lme)))
  
  #chemistry
  expect_equal(nrow(chem.lme.caseG$fixef.delete), length(unique(chem.lme$groups[["lea"]])))
  expect_equal(ncol(chem.lme.caseG$fixef.delete), length(fixef(chem.lme)))
  expect_equal(nrow(chem.lme.caseG2$fixef.delete), length(unique(chem.lme$groups[["school"]])))
  expect_equal(ncol(chem.lme.caseG2$fixef.delete), length(fixef(chem.lme)))
})

context("case_delete, random effects after deletion")

test_that("Dimensions of random effects after deletion are correct for single case deletion for lme4 models", {
  #number of rows is number of groups times number of observations, number of columns is two plus number of random effects
  #if delete was set, number of columns is just number of random effects, number of rows is the number of groups 
  
  #sleepstudy 
  expect_equal(nrow(sleep.lmer.case$ranef.delete), nrow(sleep.lmer@frame) * length(unique(sleep.lmer@flist[["Subject"]])))
  expect_equal(ncol(sleep.lmer.case$ranef.delete), 2 + ncol(ranef(sleep.lmer)$Subject))
  expect_equal(nrow(sleep.lmer.caseD$ranef.delete), length(unique(sleep.lmer@flist[["Subject"]])))
  expect_equal(ncol(sleep.lmer.caseD$ranef.delete), ncol(ranef(sleep.lmer)$Subject))
               
  
  #chemistry 
  
  expect_equal(nrow(chem.lmer.case$ranef.delete[[1]]), -3 + (nrow(chem.lmer@frame) * length(unique(chem.lmer@flist[["school:lea"]]))))
  expect_equal(ncol(chem.lmer.case$ranef.delete[[1]]), 2 + ncol(ranef(chem.lmer)$'school:lea'))
  
  expect_equal(nrow(chem.lmer.case$ranef.delete[[2]]), nrow(chem.lmer@frame) * length(unique(chem.lmer@flist[["lea"]])))
  expect_equal(ncol(chem.lmer.case$ranef.delete[[2]]), 2 + ncol(ranef(chem.lmer)$lea))

})

test_that("Dimensions of random effects after deletion are correct for single case deletion for nlme models", {
  #number of rows is number of groups times number of observations, number of columns is two plus number of random effects
  #if delete was set, number of columns is just number of random effects, number of rows is the number of groups 
  
  #sleepstudy 
  expect_equal(nrow(sleep.lme.case$ranef.delete), nrow(sleep.lme$groups) * length(unique(sleep.lme$groups$Subject)))
  expect_equal(ncol(sleep.lme.case$ranef.delete), 2 + ncol(ranef(sleep.lme)))
  expect_equal(nrow(sleep.lme.caseD$ranef.delete), length(unique(sleep.lme$groups$Subject)))
  expect_equal(ncol(sleep.lme.caseD$ranef.delete), ncol(ranef(sleep.lme)))
  
  #chemistry 
  #ranef flips the order for lme 
  expect_equal(nrow(chem.lme.case$ranef.delete[[2]]), -3 + nrow(chem.lme$groups) * length(unique(chem.lme$groups[["school"]])))
  expect_equal(ncol(chem.lme.case$ranef.delete[[2]]), 2 + ncol(ranef(chem.lme)$'school'))
  
  expect_equal(nrow(chem.lme.case$ranef.delete[[1]]), nrow(chem.lme$groups) * length(unique(chem.lme$groups$lea)))
  expect_equal(ncol(chem.lme.case$ranef.delete[[1]]), 2 + ncol(ranef(chem.lme)$lea))
})

test_that("Dimensions of random effects after deletion are correct for group deletion when delete param is NULL for lme4 models", {
  #number of rows is number of groups times (number of groups minus 1), columns is two plus number of random effects
  
  #sleepstudy
  n <- length(unique(sleep.lmer@flist[["Subject"]]))
  expect_equal(nrow(sleep.lmer.caseG$ranef.delete), n * (n-1))
  expect_equal(ncol(sleep.lmer.caseG$ranef.delete), 2 + ncol(ranef(sleep.lmer)$Subject))
  
  #chemistry
  n <- length(unique(chem.lmer@flist[["lea"]]))
  expect_equal(nrow(chem.lmer.caseG$ranef.delete[[2]]), n * (n-1))  
  expect_equal(ncol(chem.lmer.caseG$ranef.delete[[2]]), 2 + ncol(ranef(chem.lmer)$lea))
  
  n <- length(unique(chem.lmer@flist[["school:lea"]]))
  expect_equal(nrow(chem.lmer.caseG2$ranef.delete[[1]]), n * (n-1))  
  expect_equal(ncol(chem.lmer.caseG2$ranef.delete[[1]]), 2 + ncol(ranef(chem.lmer)$'school:lea'))
})

test_that("Dimensions of random effects after deletion are correct for group deletion when delete param is NULL for nlme models", {
  #number of rows is number of groups times (number of groups minus 1), columns is two plus number of random effects
  
  #sleepstudy
  n <- length(unique(sleep.lme$groups$Subject))
  expect_equal(nrow(sleep.lme.caseG$ranef.delete), n * (n-1))
  expect_equal(ncol(sleep.lme.caseG$ranef.delete), 2 + ncol(ranef(sleep.lme)))
  
  #chemistry
  n <- length(unique(chem.lme$groups[["lea"]]))
  expect_equal(nrow(chem.lme.caseG$ranef.delete[[1]]), n * (n-1))
  expect_equal(ncol(chem.lme.caseG$ranef.delete[[1]]), 1 + ncol(ranef(chem.lme)$lea)) #lme doesn't add delete column
  
  n <- length(unique(chem.lme$groups[["school"]]))
  expect_equal(nrow(chem.lme.caseG2$ranef.delete[[2]]), n * (n-1))
  expect_equal(ncol(chem.lme.caseG2$ranef.delete[[2]]), 1 + ncol(ranef(chem.lme)$school))
})


test_that("Dimensions of random effects after deletion are correct for group deletion when delete param is set for lme4 models", {
  #sleepstudy 
  expect_equal(nrow(sleep.lmer.caseGD$ranef.delete), length(unique(sleep.lmer@flist[["Subject"]])) - 1)
  expect_equal(ncol(sleep.lmer.caseGD$ranef.delete), ncol(ranef(sleep.lmer)$Subject))
  
  #chemistry 
  expect_equal(nrow(chem.lmer.caseGD$ranef.delete[[2]]), length(unique(chem.lmer@flist[["lea"]])) - 2)
  expect_equal(ncol(chem.lmer.caseGD$ranef.delete[[2]]), ncol(ranef(chem.lmer)$lea))
  
  expect_equal(nrow(chem.lmer.caseGD$ranef.delete[[1]]), 12) #there are 12 schools left after removing 2 leas 
  expect_equal(ncol(chem.lmer.caseGD$ranef.delete[[1]]), ncol(ranef(chem.lmer)$school))
  
  expect_equal(nrow(chem.lmer.caseGD2$ranef.delete[[2]]), length(unique(chem.lmer@flist[["lea"]]))) 
  expect_equal(ncol(chem.lmer.caseGD2$ranef.delete[[2]]), ncol(ranef(chem.lmer)$'lea'))
  
  expect_equal(nrow(chem.lmer.caseGD2$ranef.delete[[1]]), length(unique(chem.lmer@flist[["school:lea"]])) - 2) 
  expect_equal(ncol(chem.lmer.caseGD2$ranef.delete[[1]]), ncol(ranef(chem.lmer)$'school:lea'))
})

test_that("Dimensions of random effects after deletion are correct for group deletion when delete param is set for nlme models", {
  #sleepstudy 
  expect_equal(nrow(sleep.lme.caseGD$ranef.delete), length(unique(sleep.lme$groups$Subject)) - 1)
  expect_equal(ncol(sleep.lme.caseGD$ranef.delete), ncol(ranef(sleep.lme)))
  
  #chemistry 
  expect_equal(nrow(chem.lme.caseGD$ranef.delete[[1]]), length(unique(chem.lme$groups[["lea"]])) - 2)
  expect_equal(ncol(chem.lme.caseGD$ranef.delete[[1]]), ncol(ranef(chem.lme)$lea))
  
  expect_equal(nrow(chem.lme.caseGD$ranef.delete[[2]]), 12) #there are 12 schools left after removing 2 leas 
  expect_equal(ncol(chem.lme.caseGD$ranef.delete[[2]]), ncol(ranef(chem.lme)$school))
  
  expect_equal(nrow(chem.lme.caseGD2$ranef.delete[[1]]), length(unique(chem.lme$groups[["lea"]])))
  expect_equal(ncol(chem.lme.caseGD2$ranef.delete[[1]]), ncol(ranef(chem.lme)$'school'))
  
  expect_equal(nrow(chem.lme.caseGD2$ranef.delete[[2]]), length(unique(chem.lme$groups[["school"]])) - 2) 
  expect_equal(ncol(chem.lme.caseGD2$ranef.delete[[2]]), ncol(ranef(chem.lme)$lea))
})

context("case_delete, variance covariance matrices after deletion")

test_that("Dimensions of variance covariance matrices after deletion are correct for single case deletion for lme4 models", {
  #sleepstudy
  expect_equal(length(sleep.lmer.case$vcov.delete), nrow(sleep.lmer@frame))
  expect_equal(nrow(sleep.lmer.case$vcov.delete[[1]]), nrow(as.matrix(vcov(sleep.lmer))))
  expect_equal(ncol(sleep.lmer.case$vcov.delete[[1]]), ncol(as.matrix(vcov(sleep.lmer))))
  expect_equal(nrow(sleep.lmer.caseD$vcov.delete), nrow(as.matrix(vcov(sleep.lmer))))
  expect_equal(ncol(sleep.lmer.caseD$vcov.delete), ncol(as.matrix(vcov(sleep.lmer))))
  
  #chemistry
  expect_equal(length(chem.lmer.case$vcov.delete), nrow(chem.lmer@frame))
  expect_equal(nrow(chem.lmer.case$vcov.delete[[1]]), nrow(as.matrix(vcov(chem.lmer))))
  expect_equal(ncol(chem.lmer.case$vcov.delete[[1]]), ncol(as.matrix(vcov(chem.lmer))))
})

test_that("Dimensions of variance covariance matrices after deletion are correct for single case deletion for nlme models", {
  #sleepstudy
  expect_equal(length(sleep.lme.case$vcov.delete), nrow(sleep.lme$data))
  expect_equal(nrow(sleep.lme.case$vcov.delete[[1]]), nrow(as.matrix(vcov(sleep.lme))))
  expect_equal(ncol(sleep.lme.case$vcov.delete[[1]]), ncol(as.matrix(vcov(sleep.lme))))
  expect_equal(nrow(sleep.lme.caseD$vcov.delete), nrow(as.matrix(vcov(sleep.lme))))
  expect_equal(ncol(sleep.lme.caseD$vcov.delete), ncol(as.matrix(vcov(sleep.lme))))
  
  #chemistry
  expect_equal(length(chem.lme.case$vcov.delete), nrow(chem.lme$data))
  expect_equal(nrow(chem.lme.case$vcov.delete[[1]]), nrow(as.matrix(vcov(chem.lme))))
  expect_equal(ncol(chem.lme.case$vcov.delete[[1]]), ncol(as.matrix(vcov(chem.lme))))
})


test_that("Dimensions of variance covariance matrices after deletion are correct for group deletion when delete param is NULL for lme4 models", {
  
  #sleepstudy
  expect_equal(length(sleep.lmer.caseG$vcov.delete), length(unique(sleep.lmer@flist[["Subject"]])))
  expect_equal(nrow(sleep.lmer.caseG$vcov.delete[[1]]), nrow(as.matrix(vcov(sleep.lmer))))
  expect_equal(ncol(sleep.lmer.caseG$vcov.delete[[1]]), ncol(as.matrix(vcov(sleep.lmer))))
  
  #chemistry
  expect_equal(length(chem.lmer.caseG$vcov.delete), length(unique(chem.lmer@flist[["lea"]])))
  expect_equal(nrow(chem.lmer.caseG$vcov.delete[[1]]), nrow(as.matrix(vcov(chem.lmer))))
  expect_equal(ncol(chem.lmer.caseG$vcov.delete[[1]]), ncol(as.matrix(vcov(chem.lmer))))
  
  expect_equal(length(chem.lmer.caseG2$vcov.delete), length(unique(chem.lmer@flist[["school:lea"]])))
  expect_equal(nrow(chem.lmer.caseG2$vcov.delete[[1]]), nrow(as.matrix(vcov(chem.lmer))))
  expect_equal(ncol(chem.lmer.caseG2$vcov.delete[[1]]), ncol(as.matrix(vcov(chem.lmer))))
})

test_that("Dimensions of variance covariance matrices after deletion are correct for group deletion when delete param is NULL for nlme models", {
  
  #sleepstudy
  expect_equal(length(sleep.lme.caseG$vcov.delete), length(unique(sleep.lme$groups$Subject)))
  expect_equal(nrow(sleep.lme.caseG$vcov.delete[[1]]), nrow(as.matrix(vcov(sleep.lme))))
  expect_equal(ncol(sleep.lme.caseG$vcov.delete[[1]]), ncol(as.matrix(vcov(sleep.lme))))
  
  #chemistry
  expect_equal(length(chem.lme.caseG$vcov.delete), length(unique(chem.lme$groups[["lea"]])))
  expect_equal(nrow(chem.lme.caseG$vcov.delete[[1]]), nrow(as.matrix(vcov(chem.lme))))
  expect_equal(ncol(chem.lme.caseG$vcov.delete[[1]]), ncol(as.matrix(vcov(chem.lme))))
  
  expect_equal(length(chem.lme.caseG2$vcov.delete), length(unique(chem.lme$groups[["school"]])))
  expect_equal(nrow(chem.lme.caseG2$vcov.delete[[1]]), nrow(as.matrix(vcov(chem.lme))))
  expect_equal(ncol(chem.lme.caseG2$vcov.delete[[1]]), ncol(as.matrix(vcov(chem.lme))))
})


test_that("Dimensions of variance covariance matrices after deletion are correct when delete param is set for lme4 models", {
  
  #sleepstudy
  expect_equal(nrow(sleep.lmer.caseGD$vcov.delete), nrow(as.matrix(vcov(sleep.lmer))))
  expect_equal(ncol(sleep.lmer.caseGD$vcov.delete), ncol(as.matrix(vcov(sleep.lmer))))
  
  #chemistry 
  expect_equal(nrow(chem.lmer.caseGD$vcov.delete), nrow(as.matrix(vcov(chem.lmer))))
  expect_equal(ncol(chem.lmer.caseGD$vcov.delete), ncol(as.matrix(vcov(chem.lmer))))
  
  expect_equal(nrow(chem.lmer.caseGD2$vcov.delete), nrow(as.matrix(vcov(chem.lmer))))
  expect_equal(ncol(chem.lmer.caseGD2$vcov.delete), ncol(as.matrix(vcov(chem.lmer))))
})

test_that("Dimensions of variance covariance matrices after deletion are correct when delete param is set for nlme models", {
  
  #sleepstudy
  expect_equal(nrow(sleep.lme.caseGD$vcov.delete), nrow(as.matrix(vcov(sleep.lme))))
  expect_equal(ncol(sleep.lme.caseGD$vcov.delete), ncol(as.matrix(vcov(sleep.lme))))
  
  #chemistry 
  expect_equal(nrow(chem.lme.caseGD$vcov.delete), nrow(as.matrix(vcov(chem.lme))))
  expect_equal(ncol(chem.lme.caseGD$vcov.delete), ncol(as.matrix(vcov(chem.lme))))
  
  expect_equal(nrow(chem.lme.caseGD2$vcov.delete), nrow(as.matrix(vcov(chem.lme))))
  expect_equal(ncol(chem.lme.caseGD2$vcov.delete), ncol(as.matrix(vcov(chem.lme))))
})

context("case_delete, fitted values after deletion")

test_that("Dimensions of fitted values after deletion are correct for single case deletion for lme4 models", {
  #number of rows is number of observations * (number of observations - 1), number of columns is 2 + variables
  
  #sleepstudy
  n <- nrow(sleep.lmer@frame)
  expect_equal(nrow(sleep.lmer.case$fitted.delete), n * (n-1))
  expect_equal(ncol(sleep.lmer.case$fitted.delete), 2 + ncol(sleep.lmer@frame))
  expect_equal(length(sleep.lmer.caseD$fitted.delete), n -3)
  
  #chemistry 
  n <- nrow(chem.lmer@frame)
  expect_equal(nrow(chem.lmer.case$fitted.delete), n * (n-1))
  expect_equal(ncol(chem.lmer.case$fitted.delete), 2 + ncol(chem.lmer@frame))

})

test_that("Dimensions of fitted values after deletion are correct for single case deletion for nlme models", {
  #number of rows is number of observations * (number of observations - 1), number of columns is 2 + variables
  
  #sleepstudy
  n <- nrow(sleep.lme$data)
  expect_equal(nrow(sleep.lme.case$fitted.delete), n * (n-1))
  expect_equal(ncol(sleep.lme.case$fitted.delete), 2 + ncol(sleep.lme$data))
  expect_equal(length(sleep.lme.caseD$fitted.delete), n -3)
  
  #chemistry 
  n <- nrow(chem.lme$data)
  expect_equal(nrow(chem.lme.case$fitted.delete), n * (n-1))
  expect_equal(ncol(chem.lme.case$fitted.delete), 2 + ncol(chem.lmer@frame)) #want to match lme4
})


test_that("Dimensions of fitted values after deletion are correct for group deletion when delete param is NULL for lme4 models", {
  #number of rows is number of observations * (number of groups - 1), number of columns is 2 + variables
  
  #sleepstudy
  nobs <- nrow(sleep.lmer@frame)
  ngroups <- length(unique(sleep.lmer@flist[["Subject"]]))
  expect_equal(nrow(sleep.lmer.caseG$fitted.delete), nobs * (ngroups - 1))
  expect_equal(ncol(sleep.lmer.caseG$fitted.delete), 2 + ncol(sleep.lmer@frame))
  
  #chemistry 
  nobs <- nrow(chem.lmer@frame)
  ngroups <- length(unique(chem.lmer@flist[["lea"]]))
  expect_equal(nrow(chem.lmer.caseG$fitted.delete), nobs * (ngroups - 1))
  expect_equal(ncol(chem.lmer.caseG$fitted.delete), 2 + ncol(chem.lmer@frame))
})

test_that("Dimensions of fitted values after deletion are correct for group deletion when delete param is NULL for nlme models", {
  #number of rows is number of observations * (number of groups - 1), number of columns is 2 + variables
  
  #sleepstudy
  nobs <- nrow(sleep.lme$data)
  ngroups <- length(unique(sleep.lme$groups$Subject))
  expect_equal(nrow(sleep.lme.caseG$fitted.delete), nobs * (ngroups - 1))
  expect_equal(ncol(sleep.lme.caseG$fitted.delete), 2 + ncol(sleep.lme$data)) 
  
  #chemistry 
  nobs <- nrow(chem.lme$data)
  ngroups <- length(unique(chem.lme$groups[["lea"]]))
  expect_equal(nrow(chem.lme.caseG$fitted.delete), nobs * (ngroups - 1))
  expect_equal(ncol(chem.lme.caseG$fitted.delete), 2 + ncol(chem.lmer@frame)) #this should be equal to lmer numbers
  
  ngroups <- length(unique(chem.lme$groups[["school"]]))
  expect_equal(nrow(chem.lme.caseG2$fitted.delete), nobs * (ngroups -1))
  expect_equal(ncol(chem.lme.caseG2$fitted.delete), 2 + ncol(chem.lmer@frame))
})


test_that("Dimensions of fitted values after deletion are correct when delete parameter is set for lme4 models", {
  nobs <- nrow(sleep.lmer@frame)
  expect_equal(length(sleep.lmer.caseGD$fitted.delete), nobs - 10)
  expect_equal(length(chem.lmer.caseGD$fitted.delete), 72)
  expect_equal(length(chem.lmer.caseGD2$fitted.delete), 228)

})

test_that("Dimensions of fitted values after deletion are correct when delete parameter is set for nlme models", {
  nobs <- nrow(sleep.lmer@frame)
  expect_equal(length(sleep.lme.caseGD$fitted.delete), nobs - 10)
  expect_equal(length(chem.lme.caseGD$fitted.delete), 72)
  expect_equal(length(chem.lme.caseGD2$fitted.delete), 228)
})

context("case_delete, variance components after deletion")

test_that("Dimenstions of variance components are correct for single case deletion for lme4 models", {
  
  #sleepstudy 
  expect_equal(length(sleep.lmer.case$varcomp.delete), nrow(sleep.lmer@frame))
  expect_equal(length(sleep.lmer.case$varcomp.delete[[1]]), length(varcomp.mer(sleep.lmer)))
  expect_equal(length(sleep.lmer.caseD$varcomp.delete), length(varcomp.mer(sleep.lmer)))
  
  #chemistry 
  expect_equal(length(chem.lmer.case$varcomp.delete), nrow(chem.lmer@frame))
  expect_equal(length(chem.lmer.case$varcomp.delete[[1]]), length(varcomp.mer(chem.lmer)))
})

test_that("Dimenstions of variance components are correct for single case deletion for nlme models", {
  
  #sleepstudy 
  expect_equal(length(sleep.lme.case$varcomp.delete), nrow(sleep.lme$data))
  expect_equal(length(sleep.lme.case$varcomp.delete[[1]]), length(varcomp.lme(sleep.lme)))
  expect_equal(length(sleep.lme.caseD$varcomp.delete), length(varcomp.lme(sleep.lme)))
  
  #chemistry 
  expect_equal(length(chem.lme.case$varcomp.delete), nrow(chem.lme$data))
  expect_equal(length(chem.lme.case$varcomp.delete[[1]]), length(varcomp.lme(chem.lme)))
  
})


test_that("Dimensions of variance components are correct for group deletion when delete param is NULL for lme4 models", {
  #sleepstudy
  expect_equal(length(sleep.lmer.caseG$varcomp.delete), length(unique(sleep.lmer@flist[["Subject"]])))
  expect_equal(length(sleep.lmer.caseG$varcomp.delete[[1]]), length(varcomp.mer(sleep.lmer)))
  
  #chemistry 
  expect_equal(length(chem.lmer.caseG$varcomp.delete), length(unique(chem.lmer@flist[["lea"]])))
  expect_equal(length(chem.lmer.caseG$varcomp.delete[[1]]), length(varcomp.mer(chem.lmer)))
  
  expect_equal(length(chem.lmer.caseG2$varcomp.delete), length(unique(chem.lmer@flist[["school:lea"]])))
  expect_equal(length(chem.lmer.caseG2$varcomp.delete[[1]]), length(varcomp.mer(chem.lmer)))
  
})

test_that("Dimensions of variance components are correct for group deletion when delete param is NULL for nlme models", {
  #sleepstudy
  expect_equal(length(sleep.lme.caseG$varcomp.delete), length(unique(sleep.lme$groups$Subject)))
  expect_equal(length(sleep.lme.caseG$varcomp.delete[[1]]), length(varcomp.lme(sleep.lme)))
  
  #chemistry 
  expect_equal(length(chem.lme.caseG$varcomp.delete), length(unique(chem.lme$groups[["lea"]])))
  expect_equal(length(chem.lme.caseG$varcomp.delete[[1]]), length(varcomp.lme(chem.lme)))
  
  expect_equal(length(chem.lme.caseG2$varcomp.delete), length(unique(chem.lme$groups[["school"]])))
  expect_equal(length(chem.lme.caseG2$varcomp.delete[[1]]), length(varcomp.lme(chem.lme)))
  
})


test_that("Dimensions of variance components are correct for group deletion when delete param is set for lme4 models", {
  #sleepstudy
  expect_equal(length(sleep.lmer.caseGD$varcomp.delete), length(varcomp.mer(sleep.lmer)))
  
  #chemistry 
  expect_equal(length(chem.lmer.caseGD$varcomp.delete), length(varcomp.mer(chem.lmer)))
  expect_equal(length(chem.lmer.caseGD2$varcomp.delete), length(varcomp.mer(chem.lmer)))
})

test_that("Dimensions of variance components are correct for group deletion when delete param is set for nlme models", {
  #sleepstudy
  expect_equal(length(sleep.lme.caseGD$varcomp.delete), length(varcomp.lme(sleep.lme)))
  
  #chemistry 
  expect_equal(length(chem.lme.caseGD$varcomp.delete), length(varcomp.lme(chem.lme)))
  expect_equal(length(chem.lme.caseGD2$varcomp.delete), length(varcomp.lme(chem.lme)))
})


context("case_delete, argument restrictions")

test_that("Only correct arguments for delete parameter are allowed for lme4 models", {
  #for single case, only numeric cases are allowed 
  expect_error(case_delete(sleep.lmer, delete = "308"), "parameter should be a numeric vector")
  expect_error(case_delete(chem.lmer, delete = "1"), "parameter should be a numeric vector")

  #for group, numeric indices or group names (as a character vector) are allowed 
  expect_warning(case_delete(sleep.lmer, level = "Subject", delete = c(1,2,11)), "deleted cases do not encompass entire groups")
  expect_error(case_delete(chem.lmer, level = "school:lea", delete = c("1/1", "2/5")), "not a valid group name")

})

test_that("Only correct arguments for delete parameter are allowed for nlme models", {
  #for single case, only numeric cases are allowed 
  expect_error(case_delete(sleep.lme, delete = "308"), "parameter should be a numeric vector") 
  expect_error(case_delete(chem.lme, delete = "1"), "parameter should be a numeric vector")
  
  #for group, numeric indices or group names (as a character vector) are allowed 
  expect_warning(case_delete(sleep.lme, level = "Subject", delete = c(1,2,11)), "deleted cases do not encompass entire groups") 
  expect_error(case_delete(chem.lme, level = "school", delete = c("1:1", "5:2")), "not a valid group name")
})






