library("lme4", quietly = TRUE)
library("nlme", quietly = TRUE)

context("residuals for 2-level models")
#2 level, random intercept
bdf <- nlme::bdf
bdf.lmer <- lme4::lmer(IQ.verb ~ ses + aritPOST + langPOST + schoolSES + 
                         (1|schoolNR), data = bdf)
bdf.lme <- nlme::lme(IQ.verb ~ ses + aritPOST + langPOST + schoolSES, 
                     random = ~1|schoolNR, data = bdf)

expect_warning(bdf.resids.lmer.lvl1 <- hlm_resid(bdf.lmer))
bdf.resids.lmer.lvl2 <- hlm_resid(bdf.lmer, level = "schoolNR")
expect_warning(bdf.resids.lmer.lvl1.std <- hlm_resid(bdf.lmer, standardize = T))

expect_warning(bdf.resids.lme.lvl1 <- hlm_resid(bdf.lme))
bdf.resids.lme.lvl2 <- hlm_resid(bdf.lme, level = "schoolNR")
expect_warning(bdf.resids.lme.lvl1.std <- hlm_resid(bdf.lme, standardize = T))


test_that("correct dimentions, lme4", {
  #check lvl1
  expect_equal(nrow(bdf.resids.lmer.lvl1), nrow(bdf))
  #check lvl2
  expect_equal(nrow(bdf.resids.lmer.lvl2), length(unique(bdf$schoolNR)))
})

test_that("correct dimentions, nlme", {
  #check lvl1
  expect_equal(nrow(bdf.resids.lme.lvl1), nrow(bdf))
  #check lvl2
  expect_equal(nrow(bdf.resids.lme.lvl2), length(unique(bdf$schoolNR)))
})



test_that("standardize works, lme4", {
  nc <- ncol(bdf.resids.lmer.lvl1)
  #check column names
  expect_equal(names(bdf.resids.lmer.lvl1[(nc-5):nc]),
               c(".resid", ".fitted", ".ls.resid", ".ls.fitted", ".mar.resid", ".mar.fitted"))
  expect_equal(names(bdf.resids.lmer.lvl1.std[(nc-5):nc]),
               c(".std.resid", ".fitted", ".std.ls.resid", ".ls.fitted", ".chol.mar.resid", ".mar.fitted"))
  
  #check that raw is not the same as std
  expect_false(all(bdf.resids.lmer.lvl1$.resid == bdf.resids.lmer.lvl1.std$.std.resid))
  expect_false(all(bdf.resids.lmer.lvl1$.ls.resid == bdf.resids.lmer.lvl1.std$.std.ls.resid))
  expect_false(all(bdf.resids.lmer.lvl1$.mar.resid == bdf.resids.lmer.lvl1.std$.chol.mar.resid))
  expect_true(all(bdf.resids.lmer.lvl1$.fitted == bdf.resids.lmer.lvl1.std$.fitted))
})

test_that("standardize works, nlme", {
  nc <- ncol(bdf.resids.lme.lvl1)
  #check column names
  expect_equal(names(bdf.resids.lme.lvl1[(nc-5):nc]),
               c(".resid", ".fitted", ".ls.resid", ".ls.fitted", ".mar.resid", ".mar.fitted"))
  expect_equal(names(bdf.resids.lme.lvl1.std[(nc-5):nc]),
               c(".std.resid", ".fitted", ".std.ls.resid", ".ls.fitted", ".chol.mar.resid", ".mar.fitted"))
  
  #check that raw is not the same as std
  expect_false(all(bdf.resids.lme.lvl1$.resid == bdf.resids.lme.lvl1.std$.std.resid))
  expect_false(all(bdf.resids.lme.lvl1$.ls.resid == bdf.resids.lme.lvl1.std$.std.ls.resid))
  expect_false(all(bdf.resids.lme.lvl1$.mar.resid == bdf.resids.lme.lvl1.std$.chol.mar.resid))
  expect_true(all(bdf.resids.lme.lvl1$.fitted == bdf.resids.lme.lvl1.std$.fitted))
})




test_that("detects level 2 variables, lme4", {
  #intercept random effect and both fixed effect terms
  expect_equal(ncol(bdf.resids.lmer.lvl2), 4)
  #grabs only two level variables
  expect_equal(names(bdf.resids.lmer.lvl2)[1:2], c("schoolNR", "schoolSES"))
})

test_that("detects level 2 variables, nlme", {
  #intercept random effect and both fixed effect terms
  expect_equal(ncol(bdf.resids.lme.lvl2), 4)
  #grabs only two level variables
  expect_equal(names(bdf.resids.lme.lvl2)[1:2], c("schoolNR", "schoolSES"))
})


context("residuals for 3-level model")
#3 level, random intercept and slope
data("classroom", package = "WWGbook")

test_that("3 level model tests, lme4", {
  skip_on_cran()
  
  class.lmer <- lme4::lmer(mathgain ~ mathkind + minority + ses + housepov + 
                             (mathkind | schoolid/classid), classroom)
  
  #inner level
  class.resids <- hlm_resid(class.lmer, level = "classid:schoolid")
  #intercept and slope random effect and both fixed effect terms
  expect_equal(ncol(class.resids), 8)
  #grabs only two level variables
  expect_equal(names(class.resids)[1:4], c("group", "classid", "schoolid", "housepov"))
  
  #highest level
  class.resids <- hlm_resid(class.lmer, level = "schoolid")
  #intercept and slope random effect and both fixed effect terms
  expect_equal(ncol(class.resids), 6)
  #grabs only two level variables
  expect_equal(names(class.resids)[1:2], c("schoolid", "housepov"))
})

test_that("3 level model tests, nlme", {
  skip_on_cran()
  
  class.lme <- nlme::lme(mathgain ~ mathkind + minority + ses + housepov,
                         random = ~mathkind | schoolid/classid, classroom)
  
  #inner level
  class.resids <- hlm_resid(class.lme, level = "classid")
  #intercept and slope random effect and both fixed effect terms
  expect_equal(ncol(class.resids), 8)
  #grabs only two level variables
  expect_equal(names(class.resids)[1:4], c("group", "schoolid", "classid", "housepov"))
  
  #highest level
  class.resids <- hlm_resid(class.lme, level = "schoolid")
  #intercept and slope random effect and both fixed effect terms
  expect_equal(ncol(class.resids), 6)
  #grabs only two level variables
  expect_equal(names(class.resids)[1:2], c("schoolid", "housepov"))
})

context("NA action for 3-level model")
class.lmerNA <- lme4::lmer(mathgain ~ mathkind + minority + mathknow + 
             ses + housepov + (1 | schoolid/classid), data = classroom, 
           na.action = na.exclude)
class.lmeNA <- nlme::lme(mathgain ~ mathkind + minority + mathknow + 
                           ses + housepov, random = ~1 | schoolid/classid, 
                         data = classroom, na.action = na.exclude)

test_that("respects na.action, lme4", {
  expect_equal(nrow(expect_warning(hlm_resid(class.lmerNA, data = classroom))),
               nrow(classroom))
  expect_equal(nrow(hlm_resid(class.lmerNA, data = classroom, include.ls = FALSE)),
               nrow(classroom))
})

test_that("respects na.action, nlme", {
  expect_equal(nrow(expect_warning(hlm_resid(class.lmeNA, data = classroom))),
               nrow(classroom))
  expect_equal(nrow(hlm_resid(class.lmeNA, data = classroom, include.ls = FALSE)),
               nrow(classroom))
})


Exam <- mlmRev::Exam
exam.lmer <- lme4::lmer(normexam ~ standLRT + schgend + (1 | school), data = Exam)
exam.lme <- nlme::lme(normexam ~ standLRT + schgend, random = ~1|school, data = Exam)


test_that("correct dimentions- simple model, lme4", {
  #check lvl1
  expect_equal(nrow(hlm_resid(exam.lmer)), nrow(Exam))
  expect_equal(nrow(hlm_resid(exam.lmer, standardize = TRUE)), 
               nrow(Exam))
  expect_equal(nrow(hlm_resid(exam.lmer, standardize = TRUE, include.ls = FALSE)),
               nrow(Exam))
  expect_equal(nrow(hlm_resid(exam.lmer, include.ls = FALSE)), nrow(Exam))
  
  #check lvl2
  expect_equal(nrow(hlm_resid(exam.lmer, level = "school", include.ls = FALSE)), 
               length(unique(Exam$school)))
  expect_equal(nrow(hlm_resid(exam.lmer, level = "school", standardize = TRUE, include.ls = FALSE)), 
               length(unique(Exam$school)))
})

test_that("correct dimentions- simple model, nlme", {
  #check lvl1
  expect_equal(nrow(hlm_resid(exam.lme)), nrow(Exam))
  expect_equal(nrow(hlm_resid(exam.lme, standardize = TRUE)), 
               nrow(Exam)) #this one takes mega long for some reason
  expect_equal(nrow(hlm_resid(exam.lme, standardize = TRUE, include.ls = FALSE)),
               nrow(Exam)) #this one also takes mega long - surprising
  expect_equal(nrow(hlm_resid(exam.lme, include.ls = FALSE)), nrow(Exam))
  
  #check lvl2
  expect_equal(nrow(hlm_resid(exam.lme, level = "school", include.ls = FALSE)), 
               length(unique(Exam$school)))
  expect_equal(nrow(hlm_resid(exam.lme, level = "school", standardize = TRUE, include.ls = FALSE)), 
               length(unique(Exam$school)))
})


  