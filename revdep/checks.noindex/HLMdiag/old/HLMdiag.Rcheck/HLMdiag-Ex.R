pkgname <- "HLMdiag"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('HLMdiag')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("HLMresid.mer")
### * HLMresid.mer

flush(stderr()); flush(stdout())

### Name: HLMresid.default
### Title: Calculating residuals from HLMs
### Aliases: HLMresid.default HLMresid.mer HLMresid HLMresid.lmerMod
### Keywords: models regression

### ** Examples

## Not run: 
##D data(sleepstudy, package = "lme4")
##D fm1 <- lme4::lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
##D 
##D # level-1 residuals
##D all.equal(HLMresid(object = fm1, level = 1, type = "EB"), resid(fm1)) ## EB
##D r1LS <- HLMresid(object = fm1, level = 1, type = "LS") ## raw LS resids
##D head(r1LS)
##D r1LS.std <- HLMresid(object = fm1, level = 1, type = "LS", standardize = TRUE) ## std. LS resids
##D head(r1LS.std)
##D 
##D # level-2 residuals
##D all.equal(r2EB <- HLMresid(object = fm1, level = "Subject", type = "EB"), 
##D                            lme4::ranef(fm1)[["Subject"]])
##D r2EB.std <- HLMresid(object = fm1, level = "Subject", type = "EB", standardize = TRUE)
##D head(r2EB)
##D head(r2EB.std)
##D 
##D # marginal residuals
##D mr <- HLMresid(object = fm1, level = "marginal")
##D cholr <- HLMresid(object = fm1, level = "marginal", standardize = TRUE) # Cholesky residuals
## End(Not run)



cleanEx()
nameEx("adjust_lmList.formula")
### * adjust_lmList.formula

flush(stderr()); flush(stdout())

### Name: adjust_lmList.formula
### Title: Fitting Common Models via 'lm'
### Aliases: adjust_lmList.formula adjust_lmList
###   adjust_lmList,formula,data.frame-method
### Keywords: models regression

### ** Examples


data(Exam, package = 'mlmRev')
sepLM <- adjust_lmList(normexam ~ standLRT + sex + schgend | school, data = Exam)
confint(sepLM)




cleanEx()
nameEx("case_delete.mer")
### * case_delete.mer

flush(stderr()); flush(stdout())

### Name: case_delete.default
### Title: Case Deletion for 'mer'/'lmerMod' objects
### Aliases: case_delete.default case_delete.mer case_delete
###   case_delete.lmerMod case_delete.lme
### Keywords: models regression

### ** Examples

data(sleepstudy, package = 'lme4')
fm <- lme4::lmer(Reaction ~ Days + (Days|Subject), sleepstudy)

# Deleting every Subject
fmDel <- case_delete(model = fm, level = "Subject", type = "both")

# Deleting only subject 308
del308 <- case_delete(model = fm, level = "Subject", type = "both", delete = 308)

# Deleting a subset of subjects
delSubset <- case_delete(model = fm, level = "Subject", type = "both", delete = 308:310)




cleanEx()
nameEx("compare_eb_ls")
### * compare_eb_ls

flush(stderr()); flush(stdout())

### Name: compare_eb_ls
### Title: Visually comparing shrinkage and LS estimates
### Aliases: compare_eb_ls

### ** Examples


wages.fm1 <- lme4::lmer(lnw ~ exper + (exper | id), data = wages)
wages.sepLM <- adjust_lmList(lnw ~ exper | id, data = wages)
rancoef.eb <- coef(wages.fm1)$id
rancoef.ols <- coef(wages.sepLM)
compare_eb_ls(eb = rancoef.eb, ols = rancoef.ols, identify = 0.01)




cleanEx()
nameEx("cooks.distance")
### * cooks.distance

flush(stderr()); flush(stdout())

### Name: mdffits.default
### Title: Influence on fixed effects of HLMs
### Aliases: mdffits.default cooks.distance.mer cooks.distance
###   cooks.distance.lmerMod cooks.distance.lme mdffits.mer mdffits
###   mdffits.lmerMod mdffits.lme
### Keywords: models regression

### ** Examples

data(sleepstudy, package = 'lme4')
ss <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

# Cook's distance for individual observations
ss.cd.lev1 <- cooks.distance(ss)

# Cook's distance for each Subject
ss.cd.subject <- cooks.distance(ss, level = "Subject")

## Not run: 
##D data(Exam, package = 'mlmRev')
##D fm <- lme4::lmer(normexam ~ standLRT * schavg + (standLRT | school), Exam)
##D 
##D # Cook's distance for individual observations
##D cd.lev1 <- cooks.distance(fm)
##D 
##D # Cook's distance for each school
##D cd.school <- cooks.distance(fm, level = "school")
##D 
##D # Cook's distance when school 1 is deleted
##D cd.school1 <- cooks.distance(fm, level = "school", delete = 1)
##D 
## End(Not run)


# MDFFITS  for individual observations
ss.m1 <- mdffits(ss)

# MDFFITS for each Subject
ss.m.subject <- mdffits(ss, level = "Subject")

## Not run: 
##D   
##D 
##D # MDFFITS  for individual observations
##D m1 <- mdffits(fm)
##D 
##D # MDFFITS for each school
##D m.school <- mdffits(fm, level = "school")
## End(Not run)



cleanEx()
nameEx("covratio")
### * covratio

flush(stderr()); flush(stdout())

### Name: covratio.default
### Title: Influence on precision of fixed effects in HLMs
### Aliases: covratio.default covtrace.default covratio.mer covratio
###   covratio.lmerMod covratio.lme covtrace.mer covtrace covtrace.lmerMod
###   covtrace.lme
### Keywords: models regression

### ** Examples


data(sleepstudy, package = 'lme4')
ss <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)

# covratio for individual observations
ss.cr1 <- covratio(ss)

# covratio for subject-level deletion
ss.cr2 <- covratio(ss, level = "Subject")

## Not run: 
##D ## A larger example
##D data(Exam, package = 'mlmRev')
##D fm <- lme4::lmer(normexam ~ standLRT * schavg + (standLRT | school), data = Exam)
##D 
##D # covratio for individual observations
##D cr1 <- covratio(fm)
##D 
##D # covratio for school-level deletion
##D cr2 <- covratio(fm, level = "school")
## End(Not run)

# covtrace for individual observations
ss.ct1 <- covtrace(ss)

# covtrace for subject-level deletion
ss.ct2 <- covtrace(ss, level = "Subject")

## Not run: 
##D ## Returning to the larger example
##D # covtrace for individual observations
##D ct1 <- covtrace(fm)
##D 
##D # covtrace for school-level deletion
##D ct2 <- covtrace(fm, level = "school")
## End(Not run)



cleanEx()
nameEx("diagnostics")
### * diagnostics

flush(stderr()); flush(stdout())

### Name: diagnostics
### Title: Calculating influence diagnostics for HLMs.
### Aliases: diagnostics cooks.distance.case_delete mdffits.case_delete
###   covtrace.case_delete covratio.case_delete rvc.case_delete
### Keywords: models regression

### ** Examples

## Not run: 
##D data(sleepstudy, package = 'lme4')
##D fm <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
##D 
##D # Subject level deletion and diagnostics
##D subject.del  <- case_delete(model = fm, level = "Subject", type = "both")
##D subject.diag <- diagnostics(subject.del)
## End(Not run)



cleanEx()
nameEx("dotplot_diag")
### * dotplot_diag

flush(stderr()); flush(stdout())

### Name: dotplot_diag
### Title: Dot plots for influence diagnostics
### Aliases: dotplot_diag
### Keywords: hplot

### ** Examples

data(sleepstudy, package = 'lme4')
fm <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

#Observation level deletion and diagnostics
obs.infl <- hlm_influence(fm, level = 1)

dotplot_diag(x = obs.infl$cooksd, cutoff = "internal", name = "cooks.distance", modify = FALSE)

dotplot_diag(x = obs.infl$mdffits, cutoff = "internal", name = "cooks.distance", modify = FALSE)

# Subject level deletion and diagnostics
subject.infl  <- hlm_influence(fm, level = "Subject")

dotplot_diag(x = subject.infl$cooksd, cutoff = "internal",
             name = "cooks.distance", modify = FALSE)
             
dotplot_diag(x = subject.infl$mdffits, cutoff = "internal", name = "mdffits", modify = "dotplot")



cleanEx()
nameEx("hlm_resid.lmerMod")
### * hlm_resid.lmerMod

flush(stderr()); flush(stdout())

### Name: hlm_resid.default
### Title: Calculating residuals from HLMs
### Aliases: hlm_resid.default hlm_resid.lmerMod hlm_resid hlm_resid.lme
### Keywords: models regression

### ** Examples

data(sleepstudy, package = "lme4")
fm.lmer <- lme4::lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
fm.lme <- nlme::lme(Reaction ~ Days, random = ~Days|Subject, sleepstudy)

# level-1 and marginal residuals
fm.lmer.res1 <- hlm_resid(fm.lmer) ## raw level-1 and mar resids
fm.lmer.res1
fm.lme.std1 <- hlm_resid(fm.lme, standardize = TRUE) ## std level-1 and mar resids
fm.lme.std1

# level-2 residuals
fm.lmer.res2 <- hlm_resid(fm.lmer, level = "Subject") ## level-2 ranefs
fm.lmer.res2
fm.lme.res2 <- hlm_resid(fm.lme, level = "Subject", include.ls = FALSE) ##level-2 ranef, no LS
fm.lme.res2



cleanEx()
nameEx("leverage.mer")
### * leverage.mer

flush(stderr()); flush(stdout())

### Name: leverage.default
### Title: Leverage for HLMs
### Aliases: leverage.default leverage.mer leverage leverage.lmerMod
###   leverage.lme
### Keywords: models regression

### ** Examples

data(sleepstudy, package = 'lme4')
fm <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

# Observation level leverage
lev1 <- leverage(fm, level = 1)
head(lev1)

# Group level leverage
lev2 <- leverage(fm, level = "Subject")
head(lev2)



cleanEx()
nameEx("varcomp.mer")
### * varcomp.mer

flush(stderr()); flush(stdout())

### Name: varcomp.mer
### Title: Extracting variance components
### Aliases: varcomp.mer
### Keywords: models regression

### ** Examples

data(sleepstudy, package = "lme4") 
fm1 <- lme4::lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
varcomp.mer(fm1)



cleanEx()
nameEx("wages")
### * wages

flush(stderr()); flush(stdout())

### Name: wages
### Title: Wages for male high school dropouts
### Aliases: wages
### Keywords: datasets

### ** Examples

str(wages)
summary(wages)

## Not run: 
##D library(lme4)
##D lmer(lnw ~ exper + (exper | id), data = wages)
## End(Not run)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
