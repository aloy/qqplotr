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
nameEx("power.diagnostic.test")
### * power.diagnostic.test

flush(stderr()); flush(stdout())

### Name: power.diagnostic.test
### Title: Power calculations for a diagnostic test
### Aliases: power.diagnostic.test
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



cleanEx()
nameEx("power.hsu.t.test")
### * power.hsu.t.test

flush(stderr()); flush(stdout())

### Name: power.hsu.t.test
### Title: Power calculations for two sample Hsu t test
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
nameEx("power.nb.test")
### * power.nb.test

flush(stderr()); flush(stdout())

### Name: power.nb.test
### Title: Power calculation for comparing two negative binomial rates
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
### Title: Power calculations for two sample Welch t test
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
nameEx("qqunifSimPower")
### * qqunifSimPower

flush(stderr()); flush(stdout())

### Name: qqunif
### Title: qq - Plots for Uniform Distribution
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
### Aliases: ssize.propCI
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
