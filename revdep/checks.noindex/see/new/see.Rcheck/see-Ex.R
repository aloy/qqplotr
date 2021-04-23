pkgname <- "see"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('see')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_plot_attributes")
### * add_plot_attributes

flush(stderr()); flush(stdout())

### Name: add_plot_attributes
### Title: Complete figure with its attributes
### Aliases: add_plot_attributes

### ** Examples

## Not run: 
##D library(rstanarm)
##D library(bayestestR)
##D library(see)
##D library(ggplot2)
##D 
##D model <- stan_glm(
##D   Sepal.Length ~ Petal.Width + Species + Sepal.Width,
##D   data = iris,
##D   chains = 2, iter = 200
##D )
##D 
##D result <- hdi(model, ci = c(0.5, 0.75, 0.9, 0.95))
##D data <- data_plot(result, data = model)
##D 
##D p <- data %>%
##D   ggplot(aes(x = x, y = y, height = height, group = y, fill = fill)) +
##D   ggridges::geom_ridgeline_gradient()
##D 
##D p
##D p + add_plot_attributes(data)
## End(Not run)



cleanEx()
nameEx("bluebrown_colors")
### * bluebrown_colors

flush(stderr()); flush(stdout())

### Name: bluebrown_colors
### Title: Extract blue-brown colors as hex codes
### Aliases: bluebrown_colors

### ** Examples

bluebrown_colors()

bluebrown_colors("blue", "brown")



cleanEx()
nameEx("coord_radar")
### * coord_radar

flush(stderr()); flush(stdout())

### Name: coord_radar
### Title: Radar coordinate system
### Aliases: coord_radar

### ** Examples

# Create a radar/spider chart with ggplot:
if (require("dplyr") && require("tidyr") && require("ggplot2")) {
  data <- iris %>%
    group_by(Species) %>%
    summarise_all(mean) %>%
    pivot_longer(-Species)

  data %>%
    ggplot(aes(x = name, y = value, color = Species, group = Species)) +
    geom_polygon(fill = NA, size = 2) +
    coord_radar(start = -pi / 4)
}



cleanEx()
nameEx("data_plot")
### * data_plot

flush(stderr()); flush(stdout())

### Name: data_plot
### Title: Prepare objects for plotting or plot objects
### Aliases: data_plot

### ** Examples

## Not run: 
##D library(bayestestR)
##D if (require("rstanarm")) {
##D   model <- stan_glm(
##D     Sepal.Length ~ Petal.Width * Species,
##D     data = iris,
##D     chains = 2, iter = 200, refresh = 0
##D   )
##D 
##D   x <- rope(model)
##D   plot(x)
##D 
##D   x <- hdi(model)
##D   plot(x) + theme_modern()
##D 
##D   data <- rnorm(1000, 1)
##D   x <- p_direction(data)
##D   plot(x)
##D 
##D   x <- p_direction(model)
##D   plot(x)
##D 
##D   model <- stan_glm(
##D     mpg ~ wt + gear + cyl + disp,
##D     chains = 2,
##D     iter = 200,
##D     refresh = 0,
##D     data = mtcars
##D   )
##D   x <- equivalence_test(model)
##D   plot(x)
##D }
## End(Not run)




cleanEx()
nameEx("flat_colors")
### * flat_colors

flush(stderr()); flush(stdout())

### Name: flat_colors
### Title: Extract Flat UI colors as hex codes
### Aliases: flat_colors

### ** Examples

flat_colors()

flat_colors("dark red", "teal")



cleanEx()
nameEx("geom_point2")
### * geom_point2

flush(stderr()); flush(stdout())

### Name: geom_point2
### Title: Better looking points
### Aliases: geom_point2 geom_jitter2 geom_pointrange2 geom_count2
###   geom_count_borderless geom_point_borderless geom_jitter_borderless
###   geom_pointrange_borderless

### ** Examples

library(ggplot2)
library(see)

normal <- ggplot(iris, aes(x = Petal.Width, y = Sepal.Length)) +
  geom_point(size = 8, alpha = 0.3) +
  theme_modern()

new <- ggplot(iris, aes(x = Petal.Width, y = Sepal.Length)) +
  geom_point2(size = 8, alpha = 0.3) +
  theme_modern()

plots(normal, new, n_columns = 2)

ggplot(iris, aes(x = Petal.Width, y = Sepal.Length, fill = Species)) +
  geom_point_borderless(size = 4) +
  theme_modern()

theme_set(theme_abyss())
ggplot(iris, aes(x = Petal.Width, y = Sepal.Length, fill = Species)) +
  geom_point_borderless(size = 4)



cleanEx()
nameEx("geom_poolpoint")
### * geom_poolpoint

flush(stderr()); flush(stdout())

### Name: geom_poolpoint
### Title: Pool ball points
### Aliases: geom_poolpoint geom_pooljitter

### ** Examples

library(ggplot2)
library(see)

ggplot(iris, aes(x = Petal.Width, y = Sepal.Length, color = Species)) +
  geom_poolpoint(label = rownames(iris)) +
  scale_color_flat_d() +
  theme_modern()


ggplot(iris, aes(x = Petal.Width, y = Sepal.Length, color = Species)) +
  geom_pooljitter(label = rownames(iris)) +
  scale_color_flat_d() +
  theme_modern()



cleanEx()
nameEx("geom_violindot")
### * geom_violindot

flush(stderr()); flush(stdout())

### Name: geom_violindot
### Title: Half-violin Half-dot plot
### Aliases: geom_violindot

### ** Examples

library(ggplot2)
library(see)

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violindot() +
  theme_modern()



cleanEx()
nameEx("geom_violinhalf")
### * geom_violinhalf

flush(stderr()); flush(stdout())

### Name: geom_violinhalf
### Title: Half-violin plot
### Aliases: geom_violinhalf

### ** Examples

library(ggplot2)
library(see)

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violinhalf() +
  theme_modern() +
  scale_fill_material_d()



cleanEx()
nameEx("golden_ratio")
### * golden_ratio

flush(stderr()); flush(stdout())

### Name: golden_ratio
### Title: Golden Ratio
### Aliases: golden_ratio

### ** Examples

golden_ratio()
golden_ratio(10)



cleanEx()
nameEx("material_colors")
### * material_colors

flush(stderr()); flush(stdout())

### Name: material_colors
### Title: Extract material design colors as hex codes
### Aliases: material_colors

### ** Examples

material_colors()

material_colors("indigo", "lime")



cleanEx()
nameEx("metro_colors")
### * metro_colors

flush(stderr()); flush(stdout())

### Name: metro_colors
### Title: Extract Metro colors as hex codes
### Aliases: metro_colors

### ** Examples

metro_colors()

metro_colors("dark red", "teal")



cleanEx()
nameEx("plot.see_bayesfactor_models")
### * plot.see_bayesfactor_models

flush(stderr()); flush(stdout())

### Name: plot.see_bayesfactor_models
### Title: Plot method for Bayes Factors for model comparison
### Aliases: plot.see_bayesfactor_models

### ** Examples

library(bayestestR)
library(see)

lm0 <- lm(qsec ~ 1, data = mtcars)
lm1 <- lm(qsec ~ drat, data = mtcars)
lm2 <- lm(qsec ~ wt, data = mtcars)
lm3 <- lm(qsec ~ drat + wt, data = mtcars)

result <- bayesfactor_models(lm1, lm2, lm3, denominator = lm0)

plot(result, n_pies = "one", value = "probability", sort = TRUE) +
  scale_fill_pizza(reverse = TRUE)

plot(result, n_pies = "many", value = "BF", log = TRUE) +
  scale_fill_pizza(reverse = FALSE)



cleanEx()
nameEx("plot.see_check_collinearity")
### * plot.see_check_collinearity

flush(stderr()); flush(stdout())

### Name: plot.see_check_collinearity
### Title: Plot method for multicollinearity checks
### Aliases: plot.see_check_collinearity

### ** Examples

library(performance)
m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
result <- check_collinearity(m)
result
plot(result)



cleanEx()
nameEx("plot.see_check_distribution")
### * plot.see_check_distribution

flush(stderr()); flush(stdout())

### Name: plot.see_check_distribution
### Title: Plot method for classifying the distribution of a model-family
### Aliases: plot.see_check_distribution

### ** Examples

library(performance)
m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
result <- check_distribution(m)
result
plot(result)



cleanEx()
nameEx("plot.see_check_heteroscedasticity")
### * plot.see_check_heteroscedasticity

flush(stderr()); flush(stdout())

### Name: plot.see_check_heteroscedasticity
### Title: Plot method for (non-)constant error variance checks
### Aliases: plot.see_check_heteroscedasticity

### ** Examples

library(performance)
m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
result <- check_heteroscedasticity(m)
result
plot(result, data = m) # data required for pkgdown



cleanEx()
nameEx("plot.see_check_homogeneity")
### * plot.see_check_homogeneity

flush(stderr()); flush(stdout())

### Name: plot.see_check_homogeneity
### Title: Plot method for homogeneity of variances checks
### Aliases: plot.see_check_homogeneity

### ** Examples

library(performance)
model <<- lm(len ~ supp + dose, data = ToothGrowth)
result <- check_homogeneity(model)
result
plot(result)



cleanEx()
nameEx("plot.see_check_normality")
### * plot.see_check_normality

flush(stderr()); flush(stdout())

### Name: plot.see_check_normality
### Title: Plot method for check model for (non-)normality of residuals
### Aliases: plot.see_check_normality

### ** Examples

library(performance)
m <<- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
result <- check_normality(m)
plot(result)



cleanEx()
nameEx("plot.see_check_outliers")
### * plot.see_check_outliers

flush(stderr()); flush(stdout())

### Name: plot.see_check_outliers
### Title: Plot method for checking outliers
### Aliases: plot.see_check_outliers

### ** Examples

library(performance)
data(mtcars)
mt1 <- mtcars[, c(1, 3, 4)]
mt2 <- rbind(
  mt1,
  data.frame(mpg = c(37, 40), disp = c(300, 400), hp = c(110, 120))
)
model <- lm(disp ~ mpg + hp, data = mt2)
plot(check_outliers(model))



cleanEx()
nameEx("plot.see_cluster_analysis")
### * plot.see_cluster_analysis

flush(stderr()); flush(stdout())

### Name: plot.see_cluster_analysis
### Title: Plot method for computing cluster analysis
### Aliases: plot.see_cluster_analysis

### ** Examples

library(parameters)
groups <- cluster_analysis(iris[, 1:4], 3)
plot(groups)



cleanEx()
nameEx("plot.see_compare_parameters")
### * plot.see_compare_parameters

flush(stderr()); flush(stdout())

### Name: plot.see_compare_parameters
### Title: Plot method for comparison of model parameters
### Aliases: plot.see_compare_parameters

### ** Examples

if (require("insight") &&
  require("parameters") &&
  packageVersion("insight") >= "0.13.0") {
  data(iris)
  lm1 <- lm(Sepal.Length ~ Species, data = iris)
  lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
  result <- compare_parameters(lm1, lm2, lm3)
  plot(result)
}



cleanEx()
nameEx("plot.see_compare_performance")
### * plot.see_compare_performance

flush(stderr()); flush(stdout())

### Name: plot.see_compare_performance
### Title: Plot method for comparing model performances
### Aliases: plot.see_compare_performance

### ** Examples

library(performance)
data(iris)
lm1 <- lm(Sepal.Length ~ Species, data = iris)
lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
result <- compare_performance(lm1, lm2, lm3)
result
plot(result)



cleanEx()
nameEx("plot.see_easycormatrix")
### * plot.see_easycormatrix

flush(stderr()); flush(stdout())

### Name: plot.see_easycormatrix
### Title: Plot method for correlation matrices
### Aliases: plot.see_easycormatrix

### ** Examples

library(correlation)
data(mtcars)
result <- correlation(mtcars[, -c(8:9)])
s <- summary(result)
plot(s)



cleanEx()
nameEx("plot.see_easycorrelation")
### * plot.see_easycorrelation

flush(stderr()); flush(stdout())

### Name: plot.see_easycorrelation
### Title: Plot method for Gaussian Graphical Models
### Aliases: plot.see_easycorrelation

### ** Examples

## Not run: 
##D library(correlation)
##D library(ggraph)
##D result <- correlation(mtcars, partial = TRUE)
##D plot(result)
## End(Not run)



cleanEx()
nameEx("plot.see_effectsize_table")
### * plot.see_effectsize_table

flush(stderr()); flush(stdout())

### Name: plot.see_effectsize_table
### Title: Plot method for effect size tables
### Aliases: plot.see_effectsize_table

### ** Examples

library(effectsize)
m <- aov(mpg ~ factor(am) * factor(cyl), data = mtcars)
result <- eta_squared(m)
plot(result)



cleanEx()
nameEx("plot.see_equivalence_test")
### * plot.see_equivalence_test

flush(stderr()); flush(stdout())

### Name: plot.see_equivalence_test_effectsize
### Title: Plot method for (conditional) equivalence testing
### Aliases: plot.see_equivalence_test_effectsize plot.see_equivalence_test
###   plot.see_equivalence_test_lm

### ** Examples

library(effectsize)
m <- aov(mpg ~ factor(am) * factor(cyl), data = mtcars)
result <- eta_squared(m)
plot(result)



cleanEx()
nameEx("plot.see_estimate_contrasts")
### * plot.see_estimate_contrasts

flush(stderr()); flush(stdout())

### Name: plot.see_estimate_contrasts
### Title: Plot method for estimating contrasts
### Aliases: plot.see_estimate_contrasts

### ** Examples




cleanEx()
nameEx("plot.see_estimate_density")
### * plot.see_estimate_density

flush(stderr()); flush(stdout())

### Name: plot.see_estimate_density
### Title: Plot method for density estimation of posterior samples
### Aliases: plot.see_estimate_density

### ** Examples




cleanEx()
nameEx("plot.see_hdi")
### * plot.see_hdi

flush(stderr()); flush(stdout())

### Name: plot.see_hdi
### Title: Plot method for uncertainty or credible intervals
### Aliases: plot.see_hdi

### ** Examples




cleanEx()
nameEx("plot.see_n_factors")
### * plot.see_n_factors

flush(stderr()); flush(stdout())

### Name: plot.see_n_factors
### Title: Plot method for numbers of clusters to extract or factors to
###   retain
### Aliases: plot.see_n_factors

### ** Examples

if (require("parameters") && require("nFactors")) {
  data(mtcars)
  result <- n_factors(mtcars, type = "PCA")
  result
  plot(result, type = "line")
}



cleanEx()
nameEx("plot.see_p_direction")
### * plot.see_p_direction

flush(stderr()); flush(stdout())

### Name: plot.see_p_direction
### Title: Plot method for probability of direction
### Aliases: plot.see_p_direction

### ** Examples




cleanEx()
nameEx("plot.see_p_significance")
### * plot.see_p_significance

flush(stderr()); flush(stdout())

### Name: plot.see_p_significance
### Title: Plot method for practical significance
### Aliases: plot.see_p_significance

### ** Examples




cleanEx()
nameEx("plot.see_parameters_brms_meta")
### * plot.see_parameters_brms_meta

flush(stderr()); flush(stdout())

### Name: plot.see_parameters_brms_meta
### Title: Plot method for Model Parameters from Bayesian Meta-Analysis
### Aliases: plot.see_parameters_brms_meta

### ** Examples

## Not run: 
##D if (require("bayestestR") && require("brms") && require("metafor")) {
##D   +
##D     # data
##D     data(dat.bcg)
##D   dat <- escalc(
##D     measure = "RR",
##D     ai = tpos,
##D     bi = tneg,
##D     ci = cpos,
##D     di = cneg,
##D     data = dat.bcg
##D   )
##D   dat$author <- make.unique(dat$author)
##D 
##D   # model
##D   set.seed(123)
##D   priors <- c(
##D     prior(normal(0, 1), class = Intercept),
##D     prior(cauchy(0, 0.5), class = sd)
##D   )
##D   model <- brm(yi | se(vi) ~ 1 + (1 | author), data = dat)
##D 
##D   # result
##D   mp <- model_parameters(model)
##D   plot(mp)
##D }
## End(Not run)



cleanEx()
nameEx("plot.see_parameters_distribution")
### * plot.see_parameters_distribution

flush(stderr()); flush(stdout())

### Name: plot.see_parameters_distribution
### Title: Plot method for describing distributions of vectors
### Aliases: plot.see_parameters_distribution

### ** Examples

library(parameters)
set.seed(333)
x <- sample(1:100, 1000, replace = TRUE)
result <- describe_distribution(x)
result
plot(result)



cleanEx()
nameEx("plot.see_parameters_model")
### * plot.see_parameters_model

flush(stderr()); flush(stdout())

### Name: plot.see_parameters_model
### Title: Plot method for model parameters
### Aliases: plot.see_parameters_model plot.see_parameters_sem

### ** Examples

library(parameters)
m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
result <- model_parameters(m)
result
plot(result)



cleanEx()
nameEx("plot.see_parameters_pca")
### * plot.see_parameters_pca

flush(stderr()); flush(stdout())

### Name: plot.see_parameters_pca
### Title: Plot method for principal component analysis
### Aliases: plot.see_parameters_pca

### ** Examples

library(parameters)
data(mtcars)
result <- principal_components(mtcars[, 1:7], n = "all", threshold = 0.2)
result
plot(result)



cleanEx()
nameEx("plot.see_parameters_simulate")
### * plot.see_parameters_simulate

flush(stderr()); flush(stdout())

### Name: plot.see_parameters_simulate
### Title: Plot method for simulated model parameters
### Aliases: plot.see_parameters_simulate

### ** Examples

library(parameters)
m <- lm(mpg ~ wt + cyl + gear, data = mtcars)
result <- simulate_parameters(m)
result
plot(result)



cleanEx()
nameEx("plot.see_performance_roc")
### * plot.see_performance_roc

flush(stderr()); flush(stdout())

### Name: plot.see_performance_roc
### Title: Plot method for ROC curves
### Aliases: plot.see_performance_roc

### ** Examples

if (packageVersion("performance") > "0.7.0") {
  library(performance)
  data(iris)
  set.seed(123)
  iris$y <- rbinom(nrow(iris), size = 1, .3)

  folds <- sample(nrow(iris), size = nrow(iris) / 8, replace = FALSE)
  test_data <- iris[folds, ]
  train_data <- iris[-folds, ]

  model <- glm(y ~ Sepal.Length + Sepal.Width, data = train_data, family = "binomial")
  result <- performance_roc(model, new_data = test_data)
  result
  plot(result)
}



cleanEx()
nameEx("plot.see_point_estimate")
### * plot.see_point_estimate

flush(stderr()); flush(stdout())

### Name: plot.see_point_estimate
### Title: Plot method for point estimates of posterior samples
### Aliases: plot.see_point_estimate

### ** Examples




cleanEx()
nameEx("plot.see_rope")
### * plot.see_rope

flush(stderr()); flush(stdout())

### Name: plot.see_rope
### Title: Plot method for Region of Practical Equivalence
### Aliases: plot.see_rope

### ** Examples




cleanEx()
nameEx("plot.see_si")
### * plot.see_si

flush(stderr()); flush(stdout())

### Name: plot.see_si
### Title: Plot method for support intervals
### Aliases: plot.see_si

### ** Examples




cleanEx()
nameEx("plots")
### * plots

flush(stderr()); flush(stdout())

### Name: plots
### Title: Multiple plots side by side
### Aliases: plots

### ** Examples

library(ggplot2)
library(see)

p1 <- ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) +
  geom_point()
p2 <- ggplot(iris, aes(x = Petal.Length)) +
  geom_density()

plots(p1, p2)
plots(p1, p2, n_columns = 2, tags = TRUE)
plots(p1, p2, n_columns = 2, tags = c("Fig. 1", "Fig. 2"))



cleanEx()
nameEx("print.see_performance_pp_check")
### * print.see_performance_pp_check

flush(stderr()); flush(stdout())

### Name: print.see_performance_pp_check
### Title: Plot method for posterior predictive checks
### Aliases: print.see_performance_pp_check plot.see_performance_pp_check

### ** Examples

if (require("performance")) {
  model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
  pp_check(model)
}



cleanEx()
nameEx("scale_color_bluebrown")
### * scale_color_bluebrown

flush(stderr()); flush(stdout())

### Name: scale_color_bluebrown
### Title: Blue-brown color palette
### Aliases: scale_color_bluebrown scale_color_bluebrown_d
###   scale_color_bluebrown_c scale_colour_bluebrown
###   scale_colour_bluebrown_c scale_colour_bluebrown_d
###   scale_fill_bluebrown scale_fill_bluebrown_d scale_fill_bluebrown_c

### ** Examples

library(ggplot2)
library(see)

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  theme_modern() +
  scale_fill_bluebrown_d()



cleanEx()
nameEx("scale_color_flat")
### * scale_color_flat

flush(stderr()); flush(stdout())

### Name: scale_color_flat
### Title: Flat UI color palette
### Aliases: scale_color_flat scale_color_flat_d scale_color_flat_c
###   scale_colour_flat scale_colour_flat_c scale_colour_flat_d
###   scale_fill_flat scale_fill_flat_d scale_fill_flat_c

### ** Examples

library(ggplot2)
library(see)

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  theme_modern() +
  scale_fill_flat_d()

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin() +
  theme_modern() +
  scale_fill_flat_d(palette = "ice")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
  geom_point() +
  theme_modern() +
  scale_color_flat_c(palette = "rainbow")



cleanEx()
nameEx("scale_color_material")
### * scale_color_material

flush(stderr()); flush(stdout())

### Name: scale_color_material
### Title: Material design color palette
### Aliases: scale_color_material scale_color_material_d
###   scale_color_material_c scale_colour_material scale_colour_material_c
###   scale_colour_material_d scale_fill_material scale_fill_material_d
###   scale_fill_material_c

### ** Examples

library(ggplot2)
library(see)

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  theme_modern() +
  scale_fill_material_d()

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin() +
  theme_modern() +
  scale_fill_material_d(palette = "ice")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
  geom_point() +
  theme_modern() +
  scale_color_material_c(palette = "rainbow")



cleanEx()
nameEx("scale_color_metro")
### * scale_color_metro

flush(stderr()); flush(stdout())

### Name: scale_color_metro
### Title: Metro color palette
### Aliases: scale_color_metro scale_color_metro_d scale_color_metro_c
###   scale_colour_metro scale_colour_metro_c scale_colour_metro_d
###   scale_fill_metro scale_fill_metro_d scale_fill_metro_c

### ** Examples

library(ggplot2)
library(see)

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  theme_modern() +
  scale_fill_metro_d()

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin() +
  theme_modern() +
  scale_fill_metro_d(palette = "ice")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
  geom_point() +
  theme_modern() +
  scale_color_metro_c(palette = "rainbow")



cleanEx()
nameEx("scale_color_pizza")
### * scale_color_pizza

flush(stderr()); flush(stdout())

### Name: scale_color_pizza
### Title: Pizza color palette
### Aliases: scale_color_pizza scale_color_pizza_d scale_color_pizza_c
###   scale_colour_pizza scale_colour_pizza_c scale_colour_pizza_d
###   scale_fill_pizza scale_fill_pizza_d scale_fill_pizza_c

### ** Examples

library(ggplot2)
library(see)

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  theme_modern() +
  scale_fill_pizza_d()

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
  geom_point() +
  theme_modern() +
  scale_color_pizza_c()



cleanEx()
nameEx("scale_color_see")
### * scale_color_see

flush(stderr()); flush(stdout())

### Name: scale_color_see
### Title: See color palette
### Aliases: scale_color_see scale_color_see_d scale_color_see_c
###   scale_colour_see scale_colour_see_c scale_colour_see_d scale_fill_see
###   scale_fill_see_d scale_fill_see_c

### ** Examples

library(ggplot2)
library(see)

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  theme_modern() +
  scale_fill_see_d()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_point() +
  theme_abyss() +
  scale_colour_see(palette = "light")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
  geom_point() +
  theme_modern() +
  scale_color_see_c(palette = "rainbow")



cleanEx()
nameEx("scale_color_social")
### * scale_color_social

flush(stderr()); flush(stdout())

### Name: scale_color_social
### Title: Social color palette
### Aliases: scale_color_social scale_color_social_d scale_color_social_c
###   scale_colour_social scale_colour_social_c scale_colour_social_d
###   scale_fill_social scale_fill_social_d scale_fill_social_c

### ** Examples

library(ggplot2)
library(see)

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  theme_modern() +
  scale_fill_social_d()

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin() +
  theme_modern() +
  scale_fill_social_d(palette = "ice")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
  geom_point() +
  theme_modern() +
  scale_color_social_c(palette = "rainbow")



cleanEx()
nameEx("see_colors")
### * see_colors

flush(stderr()); flush(stdout())

### Name: see_colors
### Title: Extract See colors as hex codes
### Aliases: see_colors

### ** Examples

see_colors()

see_colors("indigo", "lime")



cleanEx()
nameEx("social_colors")
### * social_colors

flush(stderr()); flush(stdout())

### Name: social_colors
### Title: Extract Social colors as hex codes
### Aliases: social_colors

### ** Examples

social_colors()

social_colors("dark red", "teal")



cleanEx()
nameEx("theme_abyss")
### * theme_abyss

flush(stderr()); flush(stdout())

### Name: theme_abyss
### Title: Abyss theme
### Aliases: theme_abyss

### ** Examples

library(ggplot2)
library(see)

ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point(color = "white") +
  theme_abyss()



cleanEx()
nameEx("theme_blackboard")
### * theme_blackboard

flush(stderr()); flush(stdout())

### Name: theme_blackboard
### Title: Blackboard dark theme
### Aliases: theme_blackboard

### ** Examples

library(ggplot2)
library(see)

ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point(color = "white") +
  theme_blackboard()



cleanEx()
nameEx("theme_lucid")
### * theme_lucid

flush(stderr()); flush(stdout())

### Name: theme_lucid
### Title: Lucid theme
### Aliases: theme_lucid

### ** Examples

library(ggplot2)
library(see)

ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point(color = "white") +
  theme_lucid()



cleanEx()
nameEx("theme_modern")
### * theme_modern

flush(stderr()); flush(stdout())

### Name: theme_modern
### Title: The easystats' minimal theme
### Aliases: theme_modern

### ** Examples

library(ggplot2)
library(see)

ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
  geom_point() +
  theme_modern()



cleanEx()
nameEx("theme_radar")
### * theme_radar

flush(stderr()); flush(stdout())

### Name: theme_radar
### Title: Themes for radar plots
### Aliases: theme_radar theme_radar_dark

### ** Examples

if (require("ggplot2") && require("dplyr") && require("tidyr")) {
  data <- iris %>%
    group_by(Species) %>%
    summarise_all(mean) %>%
    pivot_longer(-Species)

  data %>%
    ggplot(aes(
      x = name,
      y = value,
      color = Species,
      group = Species,
      fill = Species
    )) +
    geom_polygon(size = 1, alpha = .1) +
    coord_radar() +
    theme_radar()
}



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
