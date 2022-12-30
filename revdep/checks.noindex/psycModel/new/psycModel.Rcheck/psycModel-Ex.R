pkgname <- "psycModel"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('psycModel')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("anova_plot")
### * anova_plot

flush(stderr()); flush(stdout())

### Name: anova_plot
### Title: ANOVA Plot
### Aliases: anova_plot

### ** Examples

# Main effect plot with 1 categorical variable
fit_1 = lavaan::HolzingerSwineford1939 %>% 
  dplyr::mutate(school = as.factor(school)) %>% 
  lm(data = ., grade ~ school)
anova_plot(fit_1,predictor = school)

# Interaction effect plot with 2 categorical variables 
fit_2 = lavaan::HolzingerSwineford1939 %>% 
  dplyr::mutate(dplyr::across(c(school,sex),as.factor)) %>% 
  lm(data = ., grade ~ school*sex)
anova_plot(fit_2)

# Interaction effect plot with 1 categorical variable and 1 continuous variable
fit_3 = lavaan::HolzingerSwineford1939 %>% 
  dplyr::mutate(school = as.factor(school)) %>% 
  dplyr::mutate(ageyr = as.numeric(ageyr)) %>% 
  lm(data = ., grade ~ ageyr*school)
anova_plot(fit_3)





cleanEx()
nameEx("cfa_groupwise")
### * cfa_groupwise

flush(stderr()); flush(stdout())

### Name: cfa_groupwise
### Title: Confirmatory Factor Analysis (groupwise)
### Aliases: cfa_groupwise

### ** Examples

# The example is used as the illustration of the function output only.
# It does not imply the data is appropriate for the analysis.
cfa_groupwise(
  data = lavaan::HolzingerSwineford1939,
  group = "school",
  x1:x3,
  x4:x6,
  x7:x9
)



cleanEx()
nameEx("cfa_summary")
### * cfa_summary

flush(stderr()); flush(stdout())

### Name: cfa_summary
### Title: Confirmatory Factor Analysis
### Aliases: cfa_summary

### ** Examples

# REMEMBER, YOU MUST NAMED ALL ARGUMENT EXCEPT THE CFA ITEMS ARGUMENT
# Fitting a multilevel single factor CFA model
fit <- cfa_summary(
  data = lavaan::HolzingerSwineford1939,
  x1:x3,
  x4:x6,
  x7:x9,
  group = "sex",
  model_variance = FALSE, # do not print the model_variance
  model_covariance = FALSE # do not print the model_covariance
)


# Fitting a CFA model by passing explicit lavaan model (equivalent to the above model)
# Note in the below function how I added `model = ` in front of the lavaan model.
# Similarly, the same rule apply for all arguments (e.g., `ordered = FALSE` instead of just `FALSE`)
## Not run: 
##D # This will fail because I did not add `model = ` in front of the lavaan model.
##D # Therefore,you must add the tag in front of all arguments
##D # For example, `return_result = 'model'` instaed of `model`
##D cfa_summary("visual  =~ x1 + x2 + x3
##D              textual =~ x4 + x5 + x6
##D              speed   =~ x7 + x8 + x9 ",
##D   data = lavaan::HolzingerSwineford1939
##D )
## End(Not run)



cleanEx()
nameEx("compare_fit")
### * compare_fit

flush(stderr()); flush(stdout())

### Name: compare_fit
### Title: Comparison of Model Fit
### Aliases: compare_fit

### ** Examples

# lme model

fit1 <- lm_model(
  data = popular,
  response_variable = popular,
  predictor_var = c(sex, extrav)
)

fit2 <- lm_model(
  data = popular,
  response_variable = popular,
  predictor_var = c(sex, extrav),
  two_way_interaction_factor = c(sex, extrav)
)

compare_fit(fit1, fit2)

# see ?measurement_invariance for measurement invariance example



cleanEx()
nameEx("cor_test")
### * cor_test

flush(stderr()); flush(stdout())

### Name: cor_test
### Title: Correlation table
### Aliases: cor_test

### ** Examples

cor_test(iris, where(is.numeric))



cleanEx()
nameEx("cronbach_alpha")
### * cronbach_alpha

flush(stderr()); flush(stdout())

### Name: cronbach_alpha
### Title: Cronbach alpha
### Aliases: cronbach_alpha

### ** Examples

cronbach_alpha(
  data = lavaan::HolzingerSwineford1939,
  var_name = c('Visual','Textual','Speed'),
  c(x1,x2,x3), # one way to pass the items of a factor is by wrapping it with c()
  x4:x6, # another way to pass the items is use tidyselect syntax 
  x7:x9)



cleanEx()
nameEx("descriptive_table")
### * descriptive_table

flush(stderr()); flush(stdout())

### Name: descriptive_table
### Title: Descriptive Statistics Table
### Aliases: descriptive_table

### ** Examples

descriptive_table(iris, cols = where(is.numeric)) # all numeric columns

descriptive_table(iris,
  cols = where(is.numeric),
  # get missing count, non-missing count, and mean & sd & correlation table
  descriptive_indicator = c("missing", "non_missing", "mean", "sd", "cor")
)



cleanEx()
nameEx("efa_summary")
### * efa_summary

flush(stderr()); flush(stdout())

### Name: efa_summary
### Title: Exploratory Factor Analysis
### Aliases: efa_summary

### ** Examples

efa_summary(lavaan::HolzingerSwineford1939, starts_with("x"), post_hoc_cfa = TRUE)



cleanEx()
nameEx("glm_model")
### * glm_model

flush(stderr()); flush(stdout())

### Name: glm_model
### Title: Generalized Linear Regression
### Aliases: glm_model

### ** Examples

fit <- glm_model(
  response_variable = incidence,
  predictor_variable = period,
  family = "poisson", # or you can enter as poisson(link = 'log'),
  data = lme4::cbpp
)



cleanEx()
nameEx("html_to_pdf")
### * html_to_pdf

flush(stderr()); flush(stdout())

### Name: html_to_pdf
### Title: Convert HTML to PDF
### Aliases: html_to_pdf

### ** Examples

## Not run: 
##D html_to_pdf(file_path = "html_name.html")
##D # all HTML files in the my_html_folder will be converted
##D html_to_pdf(dir = "Users/Desktop/my_html_folder")
## End(Not run)



cleanEx()
nameEx("interaction_plot")
### * interaction_plot

flush(stderr()); flush(stdout())

### Name: interaction_plot
### Title: Interaction plot
### Aliases: interaction_plot

### ** Examples

lm_fit_2 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length +
  Sepal.Width*Petal.Length, data = iris)
  
interaction_plot(lm_fit_2)

lm_fit_3 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + 
  Sepal.Width*Petal.Length:Petal.Width, data = iris)
  
interaction_plot(lm_fit_3)





cleanEx()
nameEx("knit_to_Rmd")
### * knit_to_Rmd

flush(stderr()); flush(stdout())

### Name: knit_to_Rmd
### Title: Knit Rmd Files Instruction
### Aliases: knit_to_Rmd

### ** Examples

knit_to_Rmd()



cleanEx()
nameEx("lm_model")
### * lm_model

flush(stderr()); flush(stdout())

### Name: lm_model
### Title: Linear Regressions / ANOVA / ANCOVA
### Aliases: lm_model

### ** Examples

fit <- lm_model(
  data = iris,
  response_variable = "Sepal.Length",
  predictor_variable = tidyselect::everything(),
  two_way_interaction_factor = c(Sepal.Width, Species)
)



cleanEx()
nameEx("lm_model_summary")
### * lm_model_summary

flush(stderr()); flush(stdout())

### Name: lm_model_summary
### Title: Model Summary for Linear Regression
### Aliases: lm_model_summary

### ** Examples

fit <- lm_model_summary(
  data = iris,
  response_variable = "Sepal.Length",
  predictor_variable = tidyselect::everything(),
  two_way_interaction_factor = c(Sepal.Width, Species),
  interaction_plot = FALSE, # you can also request the interaction plot
  simple_slope = FALSE, # you can also request simple slope estimate 
  assumption_plot = FALSE, # you can also request assumption plot
  streamline = FALSE #you can change this to get the least amount of info
)



cleanEx()
nameEx("lme_model")
### * lme_model

flush(stderr()); flush(stdout())

### Name: lme_model
### Title: Linear Mixed Effect Model
### Aliases: lme_model

### ** Examples

# two-level model with level-1 and level-2 variable with random intercept and random slope
fit1 <- lme_model(
  data = popular,
  response_variable = popular,
  random_effect_factors = c(extrav, sex),
  non_random_effect_factors = texp,
  id = class
)


# added two-way interaction factor
fit2 <- lme_model(
  data = popular,
  response_variable = popular,
  random_effect_factors = c(extrav, sex),
  non_random_effect_factors = texp,
  two_way_interaction_factor = c(extrav, texp),
  id = class
)

# pass a explicit lme model (I don't why you want to do that, but you can)
lme_fit <- lme_model(
  model = "popular ~ extrav*texp + (1 + extrav | class)",
  data = popular
)



cleanEx()
nameEx("lme_multilevel_model_summary")
### * lme_multilevel_model_summary

flush(stderr()); flush(stdout())

### Name: lme_multilevel_model_summary
### Title: Model Summary for Mixed Effect Model
### Aliases: lme_multilevel_model_summary

### ** Examples

fit <- lme_multilevel_model_summary(
  data = popular,
  response_variable = popular,
  random_effect_factors = NULL, # you can add random effect predictors here 
  non_random_effect_factors = c(extrav,texp),
  two_way_interaction_factor = NULL, # you can add two-way interaction plot here 
  graph_label_name = NULL, #you can also change graph lable name here
  id = class,
  simple_slope = FALSE, # you can also request simple slope estimate 
  assumption_plot = FALSE, # you can also request assumption plot
  plot_color = FALSE, # you can also request the plot in color
  streamline = FALSE # you can change this to get the least amount of info
)




cleanEx()
nameEx("measurement_invariance")
### * measurement_invariance

flush(stderr()); flush(stdout())

### Name: measurement_invariance
### Title: Measurement Invariance
### Aliases: measurement_invariance

### ** Examples

# REMEMBER, YOU MUST NAMED ALL ARGUMENT EXCEPT THE CFA ITEMS ARGUMENT
# Fitting a multiple-factor measurement invariance model by passing items.
measurement_invariance(
  x1:x3,
  x4:x6,
  x7:x9,
  data = lavaan::HolzingerSwineford1939,
  group = "school",
  invariance_level = "scalar" # you can change this to metric
)

# Fitting measurement invariance model by passing explicit lavaan model
# I am also going to only test for metric invariance instead of the default scalar invariance

## Not run: 
##D # This will fail because I did not add `model = ` in front of the lavaan model.
##D # Therefore,you must add the tag in front of all arguments
##D # For example, `return_result = 'model'` instaed of `model`
##D measurement_invariance(
##D   "visual  =~ x1 + x2 + x3;
##D              textual =~ x4 + x5 + x6;
##D              speed   =~ x7 + x8 + x9",
##D   data = lavaan::HolzingerSwineford1939
##D )
## End(Not run)




cleanEx()
nameEx("mediation_summary")
### * mediation_summary

flush(stderr()); flush(stdout())

### Name: mediation_summary
### Title: Mediation Analysis
### Aliases: mediation_summary

### ** Examples

mediation_summary(
  data = lmerTest::carrots,
  response_variable = Preference,
  mediator = Sweetness,
  predictor_variable = Crisp
)



cleanEx()
nameEx("model_summary")
### * model_summary

flush(stderr()); flush(stdout())

### Name: model_summary
### Title: Model Summary for Regression Models
### Aliases: model_summary

### ** Examples

# I am going to show the more generic usage of this function
# You can also use this package's built in function to fit the models
# I recommend using the integrated_multilevel_model_summary to get everything

# lme example
lme_fit <- lme4::lmer("popular ~ texp  + (1 | class)",
  data = popular
)

model_summary(lme_fit)

# lm example

lm_fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
  data = iris
)

model_summary(lm_fit, assumption_plot = TRUE)



cleanEx()
nameEx("polynomial_regression_plot")
### * polynomial_regression_plot

flush(stderr()); flush(stdout())

### Name: polynomial_regression_plot
### Title: Polynomial Regression Plot
### Aliases: polynomial_regression_plot

### ** Examples

fit = lm(data = iris, Sepal.Length ~ poly(Petal.Length,2))
polynomial_regression_plot(model = fit,predictor = 'Petal.Length')



cleanEx()
nameEx("reliability_summary")
### * reliability_summary

flush(stderr()); flush(stdout())

### Name: reliability_summary
### Title: Reliability Analysis
### Aliases: reliability_summary

### ** Examples


fit <- reliability_summary(data = lavaan::HolzingerSwineford1939, cols = x1:x3)
fit <- reliability_summary(data = lavaan::HolzingerSwineford1939, cols = x1:x9)



cleanEx()
nameEx("simple_slope")
### * simple_slope

flush(stderr()); flush(stdout())

### Name: simple_slope
### Title: Slope Estimate at Varying Level of Moderators
### Aliases: simple_slope

### ** Examples

fit <- lm_model(
  data = iris,
  response_variable = Sepal.Length,
  predictor_variable = tidyselect::everything(),
  three_way_interaction_factor = c(Sepal.Width, Petal.Width, Petal.Length)
)

simple_slope_fit <- simple_slope(
  model = fit,
)



cleanEx()
nameEx("three_way_interaction_plot")
### * three_way_interaction_plot

flush(stderr()); flush(stdout())

### Name: three_way_interaction_plot
### Title: Three-way Interaction Plot
### Aliases: three_way_interaction_plot

### ** Examples

lm_fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width +
  Sepal.Width:Petal.Length:Petal.Width, data = iris)

three_way_interaction_plot(lm_fit, data = iris)




cleanEx()
nameEx("two_way_interaction_plot")
### * two_way_interaction_plot

flush(stderr()); flush(stdout())

### Name: two_way_interaction_plot
### Title: Two-way Interaction Plot
### Aliases: two_way_interaction_plot

### ** Examples

lm_fit <- lm(Sepal.Length ~ Sepal.Width * Petal.Width,
  data = iris
)
two_way_interaction_plot(lm_fit, data = iris)




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
