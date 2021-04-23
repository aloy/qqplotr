pkgname <- "psycModel"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('psycModel')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
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
# Fitting a multiple factor CFA model
fit <- cfa_summary(
  data = lavaan::HolzingerSwineford1939,
  x1:x3,
  x4:x6,
  x7:x9,
)

# Fitting a multilevel single factor CFA model


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
nameEx("glme_model")
### * glme_model

flush(stderr()); flush(stdout())

### Name: glme_model
### Title: Generalized Linear Mixed Effect Model
### Aliases: glme_model

### ** Examples

fit <- glme_model(
  response_variable = incidence,
  random_effect_factors = period,
  family = "poisson", # or you can enter as poisson(link = 'log')
  id = herd,
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
nameEx("integrated_model_summary")
### * integrated_model_summary

flush(stderr()); flush(stdout())

### Name: integrated_model_summary
### Title: Integrated Function for Linear Regression
### Aliases: integrated_model_summary

### ** Examples

fit <- integrated_model_summary(
  data = iris,
  response_variable = "Sepal.Length",
  predictor_variable = tidyselect::everything(),
  two_way_interaction_factor = c(Sepal.Width, Species)
)



cleanEx()
nameEx("integrated_multilevel_model_summary")
### * integrated_multilevel_model_summary

flush(stderr()); flush(stdout())

### Name: integrated_multilevel_model_summary
### Title: Integrated Function for Mixed Effect Model
### Aliases: integrated_multilevel_model_summary

### ** Examples

fit <- integrated_multilevel_model_summary(
  data = popular,
  response_variable = popular,
  random_effect_factors = c(extrav),
  non_random_effect_factors = texp,
  two_way_interaction_factor = c(extrav, texp),
  graph_label_name = c("popular", "extraversion", "teacher experience"),
  id = class
)




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
  data = iris,
  model = fit,
  three_way_interaction_factor = c("Sepal.Width", "Petal.Width", "Petal.Length")
)



cleanEx()
nameEx("three_way_interaction_plot")
### * three_way_interaction_plot

flush(stderr()); flush(stdout())

### Name: three_way_interaction_plot
### Title: Three-way Interaction Plot
### Aliases: three_way_interaction_plot

### ** Examples

# I am going to show the more generic usage of this function
# You can also use this package's built in function to fit the models
# I recommend using the integrated_multilevel_model_summary to get everything

# lme example
lme_fit <- lme4::lmer("popular ~ extrav + sex + texp + extrav:sex:texp +
(1 + extrav + sex | class)", data = popular)

three_way_interaction_plot(lme_fit, data = popular)

# lm example

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

# If you pass the model directly, it can't extract the data-frame from fit object
# Therefore, for now, you must pass the data frame to the function.
# You don't need pass the data if you use `lm_model` or `lme_model`.

# lme example
lme_fit <- lme4::lmer("popular ~ extrav*texp + (1 + extrav | class)",
  data = popular
)

two_way_interaction_plot(lme_fit,
  graph_label_name = c("popular", "extraversion", "teacher experience"),
  data = popular
)

lm_fit <- lm(Sepal.Length ~ Sepal.Width * Petal.Width,
  data = iris
)

two_way_interaction_plot(lm_fit, data = iris)

# For more advanced users
label_name <- function(var_name) {
  var_name_processed <- switch(var_name,
    "extrav" = "Extroversion",
    "texp" = "Teacher Experience",
    "popular" = "popular"
  )
  if (is.null(var_name_processed)) {
    var_name_processed <- var_name
  }
  return(var_name_processed)
}

two_way_interaction_plot(lme_fit, data = popular, graph_label_name = label_name)



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
