pkgname <- "MKpower"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('MKpower')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("histSimPower")
### * histSimPower

flush(stderr()); flush(stdout())

### Name: hist
### Title: Histograms
### Aliases: hist hist.sim.power.ttest hist.sim.power.wtest
### Keywords: hplot

### ** Examples

res1 <- sim.power.t.test(nx = 5, rx = rnorm, rx.H0 = rnorm, 
                        ny = 10, ry = function(x) rnorm(x, mean = 3, sd = 3), 
                        ry.H0 = function(x) rnorm(x, sd = 3))
hist(res1)
res2 <- sim.power.wilcox.test(nx = 6, rx = rnorm, rx.H0 = rnorm,
                      ny = 6, ry = function(x) rnorm(x, mean = 2), 
                      ry.H0 = rnorm)
hist(res2)



cleanEx()
nameEx("power.ancova")
### * power.ancova

flush(stderr()); flush(stdout())

### Name: power.ancova
### Title: Power Calculation for ANCOVA
### Aliases: power.ancova
### Keywords: htest

### ** Examples

## Default matrix of contrasts
## 3 groups
cbind(rep(1,2), -diag(2))
## 4 groups
cbind(rep(1,3), -diag(3))

## Table 1 in Shieh (2020)
power.ancova(mu=c(400, 450, 500), var = 9900, power = 0.8)
power.ancova(n = rep(63/3, 3), mu=c(400, 450, 500), var = 9900)
power.ancova(mu=c(400, 450, 500), var = 9900, power = 0.8, nr.covs = 10)
power.ancova(n = rep(72/3, 3), mu=c(400, 450, 500), var = 9900, nr.covs = 10)

## Table 2 in Shieh (2020)
power.ancova(mu=c(400, 450, 500), var = 7500, power = 0.8)
power.ancova(n = rep(48/3, 3), mu=c(400, 450, 500), var = 7500)
power.ancova(mu=c(400, 450, 500), var = 7500, power = 0.8, nr.covs = 10)
power.ancova(n = rep(60/3, 3), mu=c(400, 450, 500), var = 7500, nr.covs = 10)

## Table 3 in Shieh (2020)
power.ancova(mu=c(400, 450, 500), var = 1900, power = 0.8)
power.ancova(n = rep(18/3, 3), mu=c(400, 450, 500), var = 1900)
power.ancova(mu=c(400, 450, 500), var = 1900, power = 0.8, nr.covs = 10)
power.ancova(n = rep(27/3, 3), mu=c(400, 450, 500), var = 1900, nr.covs = 10)

## ANOVA approach for Table 1-3
power.anova.test(groups = 3, between.var = var(c(400, 450, 500)), 
                 within.var = 10000, power = 0.8)
power.anova.test(n = 63/3, groups = 3, between.var = var(c(400, 450, 500)), 
                 within.var = 10000)

## Table 4 in Shieh (2020)
power.ancova(mu=c(410, 450, 490), var = 9900, power = 0.8)
power.ancova(n = rep(96/3, 3), mu=c(410, 450, 490), var = 9900)
power.ancova(mu=c(410, 450, 490), var = 9900, power = 0.8, nr.covs = 10)
power.ancova(n = rep(105/3, 3), mu=c(410, 450, 490), var = 9900, nr.covs = 10)

## Table 5 in Shieh (2020)
power.ancova(mu=c(410, 450, 490), var = 7500, power = 0.8)
power.ancova(n = rep(72/3, 3), mu=c(410, 450, 490), var = 7500)
power.ancova(mu=c(410, 450, 490), var = 7500, power = 0.8, nr.covs = 10)
power.ancova(n = rep(84/3, 3), mu=c(410, 450, 490), var = 7500, nr.covs = 10)

## Table 6 in Shieh (2020)
power.ancova(mu=c(410, 450, 490), var = 1900, power = 0.8)
power.ancova(n = rep(24/3, 3), mu=c(410, 450, 490), var = 1900)
power.ancova(mu=c(410, 450, 490), var = 1900, power = 0.8, nr.covs = 10)
power.ancova(n = rep(33/3, 3), mu=c(410, 450, 490), var = 1900, nr.covs = 10)

## ANOVA approach for Table 4-6
power.anova.test(groups = 3, between.var = var(c(410, 450, 490)), 
                 within.var = 10000, power = 0.8)
power.anova.test(n = 96/3, groups = 3, between.var = var(c(410, 450, 490)), 
                 within.var = 10000)

###############################################################################
## Example from Maxwell and Delaney (2004) according to Shieh (2020)
###############################################################################
## ANCOVA (balanced design)
power.ancova(n = rep(30/3, 3), mu=c(7.5366, 11.9849, 13.9785), var = 29.0898)
power.ancova(mu=c(7.5366, 11.9849, 13.9785), var = 29.0898, power = 0.8)
power.ancova(mu=c(7.5366, 11.9849, 13.9785), var = 29.0898, power = 0.9)

## ANOVA
power.anova.test(n = 30/3, groups = 3, between.var = var(c(7.5366, 11.9849, 13.9785)), 
                 within.var = 29.0898)
power.anova.test(groups = 3, between.var = var(c(7.5366, 11.9849, 13.9785)), 
                 within.var = 29.0898, power = 0.8)
power.anova.test(groups = 3, between.var = var(c(7.5366, 11.9849, 13.9785)), 
                 within.var = 29.0898, power = 0.9)
                 
## ANCOVA - imbalanced design
power.ancova(mu=c(7.5366, 11.9849, 13.9785), var = 29.0898, power = 0.8, 
             group.ratio = c(1, 1.25, 1.5))
power.ancova(n = c(13, 16, 19), mu=c(7.5366, 11.9849, 13.9785), var = 29.0898,  
             group.ratio = c(1, 1.25, 1.5))
power.ancova(mu=c(7.5366, 11.9849, 13.9785), var = 29.0898, power = 0.8, 
             group.ratio = c(1, 0.8, 2/3))
power.ancova(n = c(17, 14, 12), mu=c(7.5366, 11.9849, 13.9785), var = 29.0898,  
             group.ratio = c(1, 0.8, 2/3))



cleanEx()
nameEx("power.diagnostic.test")
### * power.diagnostic.test

flush(stderr()); flush(stdout())

### Name: power.diagnostic.test
### Title: Power Calculations for Diagnostic Tests
### Aliases: power.diagnostic.test ssize.sens.ci ssize.spec.ci
### Keywords: htest

### ** Examples

## see n2 on page 1202 of Chu and Cole (2007)
power.diagnostic.test(sens = 0.99, delta = 0.14, power = 0.95) # 40
power.diagnostic.test(sens = 0.99, delta = 0.13, power = 0.95) # 43
power.diagnostic.test(sens = 0.99, delta = 0.12, power = 0.95) # 47

power.diagnostic.test(sens = 0.98, delta = 0.13, power = 0.95) # 50
power.diagnostic.test(sens = 0.98, delta = 0.11, power = 0.95) # 58

## see page 1201 of Chu and Cole (2007)
power.diagnostic.test(sens = 0.95, delta = 0.1, n = 93) ## 0.957
power.diagnostic.test(sens = 0.95, delta = 0.1, n = 93, power = 0.95, 
                      sig.level = NULL) ## 0.0496
power.diagnostic.test(sens = 0.95, delta = 0.1, n = 102) ## 0.968
power.diagnostic.test(sens = 0.95, delta = 0.1, n = 102, power = 0.95, 
                      sig.level = NULL) ## 0.0471
## yields 102 not 93!
power.diagnostic.test(sens = 0.95, delta = 0.1, power = 0.95)

## function only for sensitivity
ssize.sens.ci(sens = 0.99, delta = 0.14, power = 0.95) # 40

## function only for specificity
ssize.spec.ci(spec = 0.99, delta = 0.13, power = 0.95) # 43



cleanEx()
nameEx("power.hsu.t.test")
### * power.hsu.t.test

flush(stderr()); flush(stdout())

### Name: power.hsu.t.test
### Title: Power Calculations for Two-sample Hsu t Test
### Aliases: power.hsu.t.test
### Keywords: htest

### ** Examples

 ## more conservative than classical or Welch t-test
 power.hsu.t.test(n = 20, delta = 1)
 power.hsu.t.test(power = .90, delta = 1)
 power.hsu.t.test(power = .90, delta = 1, alternative = "one.sided")

 ## sd1 = 0.5, sd2 = 1
 power.welch.t.test(delta = 0.5, sd1 = 0.5, sd2 = 1, power = 0.9)
 power.hsu.t.test(delta = 0.5, sd1 = 0.5, sd2 = 1, power = 0.9)




cleanEx()
nameEx("power.mpe.atleast.one")
### * power.mpe.atleast.one

flush(stderr()); flush(stdout())

### Name: power.mpe.atleast.one
### Title: Power for at least One Endpoint with Known Covariance
### Aliases: power.mpe.atleast.one
### Keywords: multivariate

### ** Examples

## compute power
power.mpe.atleast.one(K = 2, delta = c(0.2,0.2), Sigma = diag(c(1,1)), power = 0.8)

## compute sample size
power.mpe.atleast.one(K = 2, delta = c(0.2,0.2), Sigma = diag(c(2,2)), power = 0.9)

## known covariance matrix
Sigma <- matrix(c(1.440, 0.840, 1.296, 0.840,
                  0.840, 1.960, 0.168, 1.568,
                  1.296, 0.168, 1.440, 0.420,
                  0.840, 1.568, 0.420, 1.960), ncol = 4)
## compute power
power.mpe.atleast.one(K = 4, n = 60, delta = c(0.5, 0.75, 0.5, 0.75), Sigma = Sigma)
## equivalent: known SDs and correlation rho
power.mpe.atleast.one(K = 4, n = 60, delta = c(0.5, 0.75, 0.5, 0.75),
                      SD = c(1.2, 1.4, 1.2, 1.4), 
                      rho = c(0.5, 0.9, 0.5, 0.1, 0.8, 0.25))



cleanEx()
nameEx("power.mpe.known.var")
### * power.mpe.known.var

flush(stderr()); flush(stdout())

### Name: power.mpe.known.var
### Title: Multiple Co-Primary Endpoints with Known Covariance
### Aliases: power.mpe.known.var
### Keywords: htest multivariate

### ** Examples

## compute power
power.mpe.known.var(K = 2, n = 20, delta = c(1,1), Sigma = diag(c(1,1)))

## compute sample size
power.mpe.known.var(K = 2, delta = c(1,1), Sigma = diag(c(2,2)), power = 0.9,
                    sig.level = 0.025)

## known covariance matrix
Sigma <- matrix(c(1.440, 0.840, 1.296, 0.840,
                  0.840, 1.960, 0.168, 1.568,
                  1.296, 0.168, 1.440, 0.420,
                  0.840, 1.568, 0.420, 1.960), ncol = 4)
## compute power
power.mpe.known.var(K = 4, n = 60, delta = c(0.5, 0.75, 0.5, 0.75), Sigma = Sigma)
## equivalent: known SDs and correlation rho
power.mpe.known.var(K = 4, n = 60,delta = c(0.5, 0.75, 0.5, 0.75),
                    SD = c(1.2, 1.4, 1.2, 1.4), 
                    rho = c(0.5, 0.9, 0.5, 0.1, 0.8, 0.25))



cleanEx()
nameEx("power.mpe.unknown.var")
### * power.mpe.unknown.var

flush(stderr()); flush(stdout())

### Name: power.mpe.unknown.var
### Title: Multiple Co-Primary Endpoints with Unknown Covariance
### Aliases: power.mpe.unknown.var
### Keywords: htest multivariate

### ** Examples

## compute power
## Not run: 
##D power.mpe.unknown.var(K = 2, n = 20, delta = c(1,1), Sigma = diag(c(1,1)))
##D 
##D ## To compute sample size, first assume covariance as known
##D power.mpe.known.var(K = 2, delta = c(1,1), Sigma = diag(c(2,2)), power = 0.9,
##D                   sig.level = 0.025)
##D 
##D ## The value of n, which is 51, is used as n.min and n.max must be larger
##D ## then n.min so we try 60.
##D power.mpe.unknown.var(K = 2, delta = c(1,1), Sigma = diag(c(2,2)), power = 0.9,
##D                   sig.level = 0.025, n.min = 51, n.max = 60)
##D 
##D ## More complex example with unknown covariance matrix assumed to be
##D Sigma <- matrix(c(1.440, 0.840, 1.296, 0.840,
##D                   0.840, 1.960, 0.168, 1.568,
##D                   1.296, 0.168, 1.440, 0.420,
##D                   0.840, 1.568, 0.420, 1.960), ncol = 4)
##D ## compute power
##D power.mpe.unknown.var(K = 4, n = 90, delta = c(0.5, 0.75, 0.5, 0.75), Sigma = Sigma)
##D ## equivalent: unknown SDs and correlation rho
##D power.mpe.unknown.var(K = 4, n = 90, delta = c(0.5, 0.75, 0.5, 0.75),
##D                       SD = c(1.2, 1.4, 1.2, 1.4),
##D                       rho = c(0.5, 0.9, 0.5, 0.1, 0.8, 0.25))
## End(Not run)


cleanEx()
nameEx("power.nb.test")
### * power.nb.test

flush(stderr()); flush(stdout())

### Name: power.nb.test
### Title: Power Calculation for Comparing Two Negative Binomial Rates
### Aliases: power.nb.test
### Keywords: htest

### ** Examples

## examples from Table I in Zhu and Lakkis (2014)
## theta = 1/k, RR = rr, mu0 = r0, duration = mu_t
power.nb.test(mu0 = 0.8, RR = 0.85, theta = 1/0.4, duration = 0.75, power = 0.8, approach = 1)
power.nb.test(mu0 = 0.8, RR = 0.85, theta = 1/0.4, duration = 0.75, power = 0.8, approach = 2)
power.nb.test(mu0 = 0.8, RR = 0.85, theta = 1/0.4, duration = 0.75, power = 0.8, approach = 3)

power.nb.test(mu0 = 1.4, RR = 1.15, theta = 1/1.5, duration = 0.75, power = 0.8, approach = 1)
power.nb.test(mu0 = 1.4, RR = 1.15, theta = 1/1.5, duration = 0.75, power = 0.8, approach = 2)
power.nb.test(mu0 = 1.4, RR = 1.15, theta = 1/1.5, duration = 0.75, power = 0.8, approach = 3)


## examples from Table II in Zhu and Lakkis (2014) - seem to be total sample sizes
## can reproduce the results with mu_t = 1.0 (not 0.7!)
power.nb.test(mu0 = 2.0, RR = 0.5, theta = 1, duration = 1.0, ssize.ratio = 1,
              power = 0.8, approach = 1)
power.nb.test(mu0 = 2.0, RR = 0.5, theta = 1, duration = 1.0, ssize.ratio = 1,
              power = 0.8, approach = 2)
power.nb.test(mu0 = 2.0, RR = 0.5, theta = 1, duration = 1.0, ssize.ratio = 1,
              power = 0.8, approach = 3)

power.nb.test(mu0 = 10.0, RR = 1.5, theta = 1/5, duration = 1.0, ssize.ratio = 3/2,
              power = 0.8, approach = 1)
power.nb.test(mu0 = 10.0, RR = 1.5, theta = 1/5, duration = 1.0, ssize.ratio = 3/2,
              power = 0.8, approach = 2)
power.nb.test(mu0 = 10.0, RR = 1.5, theta = 1/5, duration = 1.0, ssize.ratio = 3/2,
              power = 0.8, approach = 3)


## examples from Table III in Zhu and Lakkis (2014)
power.nb.test(mu0 = 5.0, RR = 2.0, theta = 1/0.5, duration = 1, power = 0.8, approach = 1)
power.nb.test(mu0 = 5.0, RR = 2.0, theta = 1/0.5, duration = 1, power = 0.8, approach = 2)
power.nb.test(mu0 = 5.0, RR = 2.0, theta = 1/0.5, duration = 1, power = 0.8, approach = 3)


## examples from Table IV in Zhu and Lakkis (2014)
power.nb.test(mu0 = 5.9/3, RR = 0.4, theta = 0.49, duration = 3, power = 0.9, approach = 1)
power.nb.test(mu0 = 5.9/3, RR = 0.4, theta = 0.49, duration = 3, power = 0.9, approach = 2)
power.nb.test(mu0 = 5.9/3, RR = 0.4, theta = 0.49, duration = 3, power = 0.9, approach = 3)

power.nb.test(mu0 = 13/6, RR = 0.2, theta = 0.52, duration = 6, power = 0.9, approach = 1)
power.nb.test(mu0 = 13/6, RR = 0.2, theta = 0.52, duration = 6, power = 0.9, approach = 2)
power.nb.test(mu0 = 13/6, RR = 0.2, theta = 0.52, duration = 6, power = 0.9, approach = 3)


## see Section 5 of Zhu and Lakkis (2014)
power.nb.test(mu0 = 0.66, RR = 0.8, theta = 1/0.8, duration = 0.9, power = 0.9)



cleanEx()
nameEx("power.prop1.test")
### * power.prop1.test

flush(stderr()); flush(stdout())

### Name: power.prop1.test
### Title: Power Calculations for One-Sample Test for Proportions
### Aliases: power.prop1.test
### Keywords: htest

### ** Examples

power.prop1.test(p1 = 0.4, power = 0.8)
power.prop1.test(p1 = 0.4, power = 0.8, cont.corr = FALSE)
power.prop1.test(p1 = 0.6, power = 0.8)
power.prop1.test(n = 204, power = 0.8)
power.prop1.test(n = 204, p1 = 0.4, power = 0.8, sig.level = NULL)
power.prop1.test(n = 194, p1 = 0.4, power = 0.8, sig.level = NULL, 
                 cont.corr = FALSE)

power.prop1.test(p1 = 0.1, p0 = 0.3, power = 0.8, alternative = "less")
power.prop1.test(p1 = 0.1, p0 = 0.3, power = 0.8, alternative = "less", 
                 cont.corr = FALSE)
power.prop1.test(n = 31, p0 = 0.3, power = 0.8, alternative = "less")
power.prop1.test(n = 31, p1 = 0.1, p0 = 0.3, power = 0.8, sig.level = NULL, 
                 alternative = "less")


power.prop1.test(p1 = 0.5, p0 = 0.3, power = 0.8, alternative = "greater")
power.prop1.test(p1 = 0.5, p0 = 0.3, power = 0.8, alternative = "greater", 
                 cont.corr = FALSE)
power.prop1.test(n = 40, p0 = 0.3, power = 0.8, alternative = "greater")
power.prop1.test(n = 40, p1 = 0.5, p0 = 0.3, power = 0.8, sig.level = NULL, 
                 alternative = "greater")



cleanEx()
nameEx("power.welch.t.test")
### * power.welch.t.test

flush(stderr()); flush(stdout())

### Name: power.welch.t.test
### Title: Power Calculations for Two-sample Welch t Test
### Aliases: power.welch.t.test
### Keywords: htest

### ** Examples

 ## identical results as power.t.test, since sd = sd1 = sd2 = 1
 power.welch.t.test(n = 20, delta = 1)
 power.welch.t.test(power = .90, delta = 1)
 power.welch.t.test(power = .90, delta = 1, alternative = "one.sided")

  ## sd1 = 0.5, sd2 = 1
 power.welch.t.test(delta = 2, sd1 = 0.5, sd2 = 1, power = 0.9)




cleanEx()
nameEx("print.power.mpe.test")
### * print.power.mpe.test

flush(stderr()); flush(stdout())

### Name: print.power.mpe.test
### Title: Print Methods for Hypothesis Tests, Sample size and Power
###   Calculations
### Aliases: print.power.mpe.test
### Keywords: htest power.htest

### ** Examples

(pkv <- power.mpe.known.var(K = 2, delta = c(1,1), Sigma = diag(c(2,2)), power = 0.9,
                            sig.level = 0.025))
print(pkv, digits =  4) # using less digits than default
print(pkv, digits = 12) # using more digits than default



cleanEx()
nameEx("qqunifSimPower")
### * qqunifSimPower

flush(stderr()); flush(stdout())

### Name: qqunif
### Title: qq-Plots for Uniform Distribution
### Aliases: qqunif qqunif.default qqunif.sim.power.ttest
###   qqunif.sim.power.wtest
### Keywords: hplot

### ** Examples

## default
qqunif(runif(100))

## visualization of empirical power and type-I-error
res1 <- sim.power.t.test(nx = 5, rx = rnorm, rx.H0 = rnorm, 
                        ny = 10, ry = function(x) rnorm(x, mean = 3, sd = 3), 
                        ry.H0 = function(x) rnorm(x, sd = 3))
qqunif(res1, alpha = 0.1)

res2 <- sim.power.wilcox.test(nx = 6, rx = rnorm, rx.H0 = rnorm,
                      ny = 6, ry = function(x) rnorm(x, mean = 2), 
                      ry.H0 = rnorm)
qqunif(res2)



cleanEx()
nameEx("sim.power.t.test")
### * sim.power.t.test

flush(stderr()); flush(stdout())

### Name: sim.power.t.test
### Title: Monte Carlo Simulations for Empirical Power of Two-sample
###   t-Tests
### Aliases: sim.power.t.test
### Keywords: htest

### ** Examples

## Equal variance, small sample size
power.t.test(n = 5, delta = 2)
power.welch.t.test(n = 5, delta = 2)
power.hsu.t.test(n = 5, delta = 2)
sim.power.t.test(nx = 5, rx = rnorm, rx.H0 = rnorm,
                 ny = 5, ry = function(x) rnorm(x, mean = 2), ry.H0 = rnorm)

## Equal variance, moderate sample size
power.t.test(n = 25, delta = 0.8)
power.welch.t.test(n = 25, delta = 0.8)
power.hsu.t.test(n = 25, delta = 0.8)
sim.power.t.test(nx = 25, rx = rnorm, rx.H0 = rnorm,
                 ny = 25, ry = function(x) rnorm(x, mean = 0.8), ry.H0 = rnorm)

## Equal variance, high sample size
power.t.test(n = 100, delta = 0.4)
power.welch.t.test(n = 100, delta = 0.4)
power.hsu.t.test(n = 100, delta = 0.4)
sim.power.t.test(nx = 100, rx = rnorm, rx.H0 = rnorm,
                 ny = 100, ry = function(x) rnorm(x, mean = 0.4), ry.H0 = rnorm)

## Unequal variance, small sample size
power.welch.t.test(n = 5, delta = 5, sd1 = 1, sd2 = 3)
power.hsu.t.test(n = 5, delta = 5, sd1 = 1, sd2 = 3)
sim.power.t.test(nx = 5, rx = rnorm, rx.H0 = rnorm, 
                 ny = 5, ry = function(x) rnorm(x, mean = 5, sd = 3), 
                 ry.H0 = function(x) rnorm(x, sd = 3))
                 
## Unequal variance, moderate sample size
power.welch.t.test(n = 25, delta = 1.8, sd1 = 1, sd2 = 3)
power.hsu.t.test(n = 25, delta = 1.8, sd1 = 1, sd2 = 3)
sim.power.t.test(nx = 25, rx = rnorm, rx.H0 = rnorm, 
                 ny = 25, ry = function(x) rnorm(x, mean = 1.8, sd = 3), 
                 ry.H0 = function(x) rnorm(x, sd = 3))
                 
## Unequal variance, high sample size
power.welch.t.test(n = 100, delta = 0.9, sd1 = 1, sd2 = 3)
power.hsu.t.test(n = 100, delta = 0.9, sd1 = 1, sd2 = 3)
sim.power.t.test(nx = 100, rx = rnorm, rx.H0 = rnorm, 
                 ny = 100, ry = function(x) rnorm(x, mean = 0.9, sd = 3), 
                 ry.H0 = function(x) rnorm(x, sd = 3))
                 
## Unequal variance, unequal sample sizes
## small sample sizes
sim.power.t.test(nx = 10, rx = rnorm, rx.H0 = rnorm, 
                 ny = 5, ry = function(x) rnorm(x, mean = 5, sd = 3), 
                 ry.H0 = function(x) rnorm(x, sd = 3))
sim.power.t.test(nx = 5, rx = rnorm, rx.H0 = rnorm, 
                 ny = 10, ry = function(x) rnorm(x, mean = 3, sd = 3), 
                 ry.H0 = function(x) rnorm(x, sd = 3))

## Unequal variance, unequal sample sizes
## moderate sample sizes
sim.power.t.test(nx = 25, rx = rnorm, rx.H0 = rnorm, 
                 ny = 50, ry = function(x) rnorm(x, mean = 1.5, sd = 3), 
                 ry.H0 = function(x) rnorm(x, sd = 3))

## Unequal variance, unequal sample sizes
## high sample sizes
sim.power.t.test(nx = 100, rx = rnorm, rx.H0 = rnorm, 
                 ny = 200, ry = function(x) rnorm(x, mean = 0.6, sd = 3), 
                 ry.H0 = function(x) rnorm(x, sd = 3))



cleanEx()
nameEx("sim.power.wilcox.test")
### * sim.power.wilcox.test

flush(stderr()); flush(stdout())

### Name: sim.power.wilcox.test
### Title: Monte Carlo Simulations for Empirical Power of
###   Wilcoxon-Mann-Whitney Tests
### Aliases: sim.power.wilcox.test
### Keywords: htest

### ** Examples

## Equal variance, small sample size
power.t.test(n = 5, power = 0.8)
sim.ssize.wilcox.test(rx = rnorm, ry = function(x) rnorm(x, mean = 2), 
                      power = 0.8, n.min = 3, n.max = 10, step.size = 1)
sim.power.wilcox.test(nx = 6, rx = rnorm, rx.H0 = rnorm,
                      ny = 6, ry = function(x) rnorm(x, mean = 2), 
                      ry.H0 = rnorm)



cleanEx()
nameEx("sim.ssize.wilcox.test")
### * sim.ssize.wilcox.test

flush(stderr()); flush(stdout())

### Name: sim.ssize.wilcox.test
### Title: Sample Size for Wilcoxon Rank Sum and Signed Rank Tests
### Aliases: sim.ssize.wilcox.test
### Keywords: htest

### ** Examples




cleanEx()
nameEx("ssize.auc.ci")
### * ssize.auc.ci

flush(stderr()); flush(stdout())

### Name: ssize.auc.ci
### Title: Sample Size Calculations for AUC
### Aliases: ssize.auc.ci
### Keywords: htest

### ** Examples

## compute n
ssize.auc.ci(AUC = 0.9, delta = 0.05, power = 0.8)
## compute delta
ssize.auc.ci(AUC = 0.9, n = 254, power = 0.8)
## compute power
ssize.auc.ci(AUC = 0.9, n = 254, delta = 0.05)
## compute sig.level
ssize.auc.ci(AUC = 0.9, n = 254, delta = 0.05, power = 0.8, sig.level = NULL)



cleanEx()
nameEx("ssize.pcc")
### * ssize.pcc

flush(stderr()); flush(stdout())

### Name: ssize.pcc
### Title: Sample Size Planning for Developing Classifiers Using High
###   Dimensional Data
### Aliases: ssize.pcc
### Keywords: htest

### ** Examples

## see Table 2 of Dobbin et al. (2008)
g <- 0.1
fc <- 1.6
ssize.pcc(gamma = g, stdFC = fc, nrFeatures = 22000)

## see Table 3 of Dobbin et al. (2008)
g <- 0.05
fc <- 1.1
ssize.pcc(gamma = g, stdFC = fc, nrFeatures = 22000)



cleanEx()
nameEx("ssize.propCI")
### * ssize.propCI

flush(stderr()); flush(stdout())

### Name: ssize.propCI
### Title: Sample Size Calculation for Confidence Interval of a Proportion
### Aliases: ssize.propCI ssize.prop.ci
### Keywords: htest

### ** Examples

ssize.propCI(prop = 0.1, width = 0.1)
ssize.propCI(prop = 0.3, width = 0.1)
ssize.propCI(prop = 0.3, width = 0.1, method = "wald")
ssize.propCI(prop = 0.3, width = 0.1, method = "jeffreys")
ssize.propCI(prop = 0.3, width = 0.1, method = "clopper-pearson")
ssize.propCI(prop = 0.3, width = 0.1, method = "wilson")
ssize.propCI(prop = 0.3, width = 0.1, method = "agresti-coull")



cleanEx()
nameEx("ssize.reference.range")
### * ssize.reference.range

flush(stderr()); flush(stdout())

### Name: ssize.reference.range
### Title: Power Calculations for Two-sample Hsu t Test
### Aliases: ssize.reference.range
### Keywords: htest

### ** Examples

  ## see Table 1 in Jennen-Steinmetz and Wellek (2005)
  ssize.reference.range(delta = 0.03, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "parametric", exact = TRUE)
  ## 135 vs 125 (error in Table 1)
  ssize.reference.range(delta = 0.03, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "nonparametric", exact = TRUE)
  ssize.reference.range(delta = 0.03, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "parametric", exact = FALSE)
  ssize.reference.range(delta = 0.03, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "nonparametric", exact = FALSE)
  
  ssize.reference.range(delta = 0.025, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "parametric", exact = TRUE)
  ssize.reference.range(delta = 0.025, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "nonparametric", exact = TRUE)
  ssize.reference.range(delta = 0.025, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "parametric", exact = FALSE)
  ssize.reference.range(delta = 0.025, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "nonparametric", exact = FALSE)
  
  ssize.reference.range(delta = 0.02, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "parametric", exact = TRUE)
  ## 314 vs. 305 (error Table 1?)
  ssize.reference.range(delta = 0.02, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "nonparametric", exact = TRUE)
  ssize.reference.range(delta = 0.02, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "parametric", exact = FALSE)
  ssize.reference.range(delta = 0.02, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "nonparametric", exact = FALSE)
  
  ssize.reference.range(delta = 0.015, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "parametric", exact = TRUE)
  ssize.reference.range(delta = 0.015, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "nonparametric", exact = TRUE)
  ssize.reference.range(delta = 0.015, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "parametric", exact = FALSE)
  ssize.reference.range(delta = 0.015, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "nonparametric", exact = FALSE)
  
  ssize.reference.range(delta = 0.01, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "parametric", exact = TRUE)
  ssize.reference.range(delta = 0.01, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "nonparametric", exact = TRUE)
  ssize.reference.range(delta = 0.01, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "parametric", exact = FALSE)
  ssize.reference.range(delta = 0.01, ref.prob = 0.9, conf.prob = 0.9, 
                        method = "nonparametric", exact = FALSE)
  
  ssize.reference.range(delta = 0.015, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "parametric", exact = TRUE)
  ssize.reference.range(delta = 0.015, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "nonparametric", exact = TRUE)
  ssize.reference.range(delta = 0.015, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "parametric", exact = FALSE)
  ssize.reference.range(delta = 0.015, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "nonparametric", exact = FALSE)
  
  ssize.reference.range(delta = 0.0125, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "parametric", exact = TRUE)
  ssize.reference.range(delta = 0.0125, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "nonparametric", exact = TRUE)
  ssize.reference.range(delta = 0.0125, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "parametric", exact = FALSE)
  ssize.reference.range(delta = 0.0125, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "nonparametric", exact = FALSE)
  
  ssize.reference.range(delta = 0.01, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "parametric", exact = TRUE)
  ssize.reference.range(delta = 0.01, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "nonparametric", exact = TRUE)
  ssize.reference.range(delta = 0.01, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "parametric", exact = FALSE)
  ssize.reference.range(delta = 0.01, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "nonparametric", exact = FALSE)
  
  ssize.reference.range(delta = 0.0075, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "parametric", exact = TRUE)
  ssize.reference.range(delta = 0.0075, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "nonparametric", exact = TRUE)
  ssize.reference.range(delta = 0.0075, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "parametric", exact = FALSE)
  ssize.reference.range(delta = 0.0075, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "nonparametric", exact = FALSE)
  
  ssize.reference.range(delta = 0.005, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "parametric", exact = TRUE)
  ssize.reference.range(delta = 0.005, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "nonparametric", exact = TRUE)
  ssize.reference.range(delta = 0.005, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "parametric", exact = FALSE)
  ssize.reference.range(delta = 0.005, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "nonparametric", exact = FALSE)
  
  
  ## results are equivalent to one-sided reference range with coverage of 
  ## 95 percent instead of 90 percent; for example
  ssize.reference.range(delta = 0.03, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "parametric", exact = TRUE, alternative = "one.sided")
  ## 135 vs 125 (error in Table 1)
  ssize.reference.range(delta = 0.03, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "nonparametric", exact = TRUE, alternative = "one.sided")
  ssize.reference.range(delta = 0.03, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "parametric", exact = FALSE, alternative = "one.sided")
  ssize.reference.range(delta = 0.03, ref.prob = 0.95, conf.prob = 0.9, 
                        method = "nonparametric", exact = FALSE, alternative = "one.sided")




cleanEx()
nameEx("volcanoSimPower")
### * volcanoSimPower

flush(stderr()); flush(stdout())

### Name: volcano
### Title: Volcano Plots
### Aliases: volcano volcano.sim.power.ttest volcano.sim.power.wtest
### Keywords: hplot

### ** Examples

res1 <- sim.power.t.test(nx = 5, rx = rnorm, rx.H0 = rnorm, 
                        ny = 10, ry = function(x) rnorm(x, mean = 3, sd = 3), 
                        ry.H0 = function(x) rnorm(x, sd = 3))
volcano(res1)



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
