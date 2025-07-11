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

## Don't show: 
if (require("rstanarm", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



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

library(ggplot2)

# Create a radar/spider chart with ggplot:
data(iris)
data <- aggregate(iris[-5], list(Species = iris$Species), mean)
data <- datawizard::data_to_long(
  data,
  c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
)

ggplot(data, aes(x = name, y = value, color = Species, group = Species)) +
  geom_polygon(fill = NA, linewidth = 2) +
  coord_radar(start = -pi / 4)



cleanEx()
nameEx("data_plot")
### * data_plot

flush(stderr()); flush(stdout())

### Name: data_plot
### Title: Prepare objects for plotting or plot objects
### Aliases: data_plot data_plot.compare_performance

### ** Examples

## Don't show: 
if (identical(Sys.getenv("NOT_CRAN"), "true") && require("rstanarm")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(bayestestR)
library(rstanarm)

model <<- suppressWarnings(stan_glm(
  Sepal.Length ~ Petal.Width * Species,
  data = iris,
  chains = 2, iter = 200, refresh = 0
))

x <- rope(model, verbose = FALSE)
plot(x)

x <- hdi(model)
plot(x) + theme_modern()

x <- p_direction(model, verbose = FALSE)
plot(x)

model <<- suppressWarnings(stan_glm(
  mpg ~ wt + gear + cyl + disp,
  chains = 2,
  iter = 200,
  refresh = 0,
  data = mtcars
))
x <- equivalence_test(model, verbose = FALSE)
plot(x)
## Don't show: 
}) # examplesIf
## End(Don't show)



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
nameEx("geom_binomdensity")
### * geom_binomdensity

flush(stderr()); flush(stdout())

### Name: geom_binomdensity
### Title: Add dot-densities for binary 'y' variables
### Aliases: geom_binomdensity

### ** Examples

## Don't show: 
if (require("ggdist")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(ggplot2)
library(see)

data <- iris[1:100, ]

ggplot() +
  geom_binomdensity(data,
    x = "Sepal.Length",
    y = "Species",
    fill = "red",
    color = NA
  )

# Different scales
data[1:70, "Species"] <- "setosa" # Create unbalanced proportions

ggplot() +
  geom_binomdensity(data, x = "Sepal.Length", y = "Species", scale = "auto")
ggplot() +
  geom_binomdensity(data, x = "Sepal.Length", y = "Species", scale = "density")
ggplot() +
  geom_binomdensity(data, x = "Sepal.Length", y = "Species", scale = "proportion")
ggplot() +
  geom_binomdensity(data,
    x = "Sepal.Length", y = "Species",
    scale = list("setosa" = 0.4, "versicolor" = 0.6)
  )
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("geom_from_list")
### * geom_from_list

flush(stderr()); flush(stdout())

### Name: geom_from_list
### Title: Create ggplot2 geom(s) from a list
### Aliases: geom_from_list geoms_from_list

### ** Examples

## Don't show: 
if (require("ggside") && require("ggplot2")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(ggplot2)

# Example 1 (basic geoms and labels) --------------------------
l1 <- list(
  geom = "point",
  data = mtcars,
  aes = list(x = "mpg", y = "wt", size = "hp", color = "hp"),
  show.legend = c("size" = FALSE)
)
l2 <- list(
  geom = "labs",
  title = "A Title"
)

ggplot() +
  geom_from_list(l1) +
  geom_from_list(l2)

ggplot() +
  geoms_from_list(list(l1 = l1, l2 = l2))

# Example 2 (Violin, boxplots, ...) --------------------------
l1 <- list(
  geom = "violin",
  data = iris,
  aes = list(x = "Species", y = "Sepal.Width")
)
l2 <- list(
  geom = "boxplot",
  data = iris,
  aes = list(x = "Species", y = "Sepal.Width"),
  outlier.shape = NA
)
l3 <- list(
  geom = "jitter",
  data = iris,
  width = 0.1,
  aes = list(x = "Species", y = "Sepal.Width")
)

ggplot() +
  geom_from_list(l1) +
  geom_from_list(l2) +
  geom_from_list(l3)

# Example 3 (2D density) --------------------------
ggplot() +
  geom_from_list(list(
    geom = "density_2d", data = iris,
    aes = list(x = "Sepal.Width", y = "Petal.Length")
  ))
ggplot() +
  geom_from_list(list(
    geom = "density_2d_filled", data = iris,
    aes = list(x = "Sepal.Width", y = "Petal.Length")
  ))
ggplot() +
  geom_from_list(list(
    geom = "density_2d_polygon", data = iris,
    aes = list(x = "Sepal.Width", y = "Petal.Length")
  ))
ggplot() +
  geom_from_list(list(
    geom = "density_2d_raster", data = iris,
    aes = list(x = "Sepal.Width", y = "Petal.Length")
  )) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

# Example 4 (facet and coord flip) --------------------------

ggplot(iris, aes(x = Sepal.Length, y = Petal.Width)) +
  geom_point() +
  geom_from_list(list(geom = "hline", yintercept = 2)) +
  geom_from_list(list(geom = "coord_flip")) +
  geom_from_list(list(geom = "facet_wrap", facets = "~ Species", scales = "free"))

# Example 5 (theme and scales) --------------------------
ggplot(iris, aes(x = Sepal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  geom_from_list(list(geom = "scale_color_viridis_d", option = "inferno")) +
  geom_from_list(list(geom = "theme", legend.position = "top"))

ggplot(iris, aes(x = Sepal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  geom_from_list(list(geom = "scale_color_material_d", palette = "rainbow")) +
  geom_from_list(list(geom = "theme_void"))

# Example 5 (Smooths and side densities) --------------------------

ggplot(iris, aes(x = Sepal.Length, y = Petal.Width)) +
  geom_from_list(list(geom = "point")) +
  geom_from_list(list(geom = "smooth", color = "red")) +
  geom_from_list(list(aes = list(x = "Sepal.Length"), geom = "ggside::geom_xsidedensity")) +
  geom_from_list(list(geom = "ggside::scale_xsidey_continuous", breaks = NULL))
## Don't show: 
}) # examplesIf
## End(Don't show)



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

## Don't show: 
if (requireNamespace("patchwork", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
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
## Don't show: 
}) # examplesIf
## End(Don't show)



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

# To flip all half-violin geoms, use `flip = TRUE`:
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violinhalf(flip = TRUE) +
  theme_modern() +
  scale_fill_material_d()

# To flip the half-violin geoms for the first and third groups only
# by passing a numeric vector
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violinhalf(flip = c(1, 3)) +
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
nameEx("okabeito_colors")
### * okabeito_colors

flush(stderr()); flush(stdout())

### Name: okabeito_colors
### Title: Extract Okabe-Ito colors as hex codes
### Aliases: okabeito_colors oi_colors

### ** Examples

okabeito_colors()

okabeito_colors(c("red", "light blue", "orange"))

okabeito_colors(original_names = TRUE)

okabeito_colors(black_first = TRUE)



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
nameEx("plot.see_check_dag")
### * plot.see_check_dag

flush(stderr()); flush(stdout())

### Name: plot.see_check_dag
### Title: Plot method for check DAGs
### Aliases: plot.see_check_dag

### ** Examples

## Don't show: 
if (require("ggdag", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(performance)
# incorrect adjustment
dag <- check_dag(
  y ~ x + b + c,
  x ~ b,
  outcome = "y",
  exposure = "x"
)
dag
plot(dag)

# plot only model with required adjustments
plot(dag, which = "required")

# collider-bias?
dag <- check_dag(
  y ~ x + c + d,
  x ~ c + d,
  b ~ x,
  b ~ y,
  outcome = "y",
  exposure = "x",
  adjusted = "c"
)
plot(dag)

# longer labels, automatic detection of outcome and exposure
dag <- check_dag(
  QoL ~ age + education + gender,
  age ~ education
)
plot(dag)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("plot.see_check_distribution")
### * plot.see_check_distribution

flush(stderr()); flush(stdout())

### Name: plot.see_check_distribution
### Title: Plot method for classifying the distribution of a model-family
### Aliases: plot.see_check_distribution

### ** Examples

## Don't show: 
if (identical(Sys.getenv("NOT_CRAN"), "true") && require("randomForest")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(performance)
m <<- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
result <- check_distribution(m)
result
plot(result)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("plot.see_check_heteroscedasticity")
### * plot.see_check_heteroscedasticity

flush(stderr()); flush(stdout())

### Name: plot.see_check_heteroscedasticity
### Title: Plot method for (non-)constant error variance checks
### Aliases: plot.see_check_heteroscedasticity

### ** Examples

m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
result <- performance::check_heteroscedasticity(m)
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
nameEx("plot.see_check_model")
### * plot.see_check_model

flush(stderr()); flush(stdout())

### Name: plot.see_check_model
### Title: Plot method for checking model assumptions
### Aliases: plot.see_check_model

### ** Examples

## Don't show: 
if (require("patchwork")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(performance)

model <- lm(qsec ~ drat + wt, data = mtcars)
plot(check_model(model))
## Don't show: 
}) # examplesIf
## End(Don't show)



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

## Don't show: 
if (require("qqplotr")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
plot(result, type = "qq", detrend = TRUE)
## Don't show: 
}) # examplesIf
## End(Don't show)



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
nameEx("plot.see_compare_parameters")
### * plot.see_compare_parameters

flush(stderr()); flush(stdout())

### Name: plot.see_compare_parameters
### Title: Plot method for comparison of model parameters
### Aliases: plot.see_compare_parameters

### ** Examples

data(iris)
lm1 <- lm(Sepal.Length ~ Species, data = iris)
lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
result <- parameters::compare_parameters(lm1, lm2, lm3)
plot(result)



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

## Don't show: 
if (all(insight::check_if_installed(c("marginaleffects", "Formula"), quietly = TRUE))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("plot.see_estimate_density")
### * plot.see_estimate_density

flush(stderr()); flush(stdout())

### Name: plot.see_estimate_density
### Title: Plot method for density estimation of posterior samples
### Aliases: plot.see_estimate_density

### ** Examples

## Don't show: 
if (identical(Sys.getenv("NOT_CRAN"), "true") && require("rstanarm")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(rstanarm)
library(bayestestR)
set.seed(123)
m <<- suppressWarnings(stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0))
result <- estimate_density(m)
plot(result)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("plot.see_hdi")
### * plot.see_hdi

flush(stderr()); flush(stdout())

### Name: plot.see_hdi
### Title: Plot method for uncertainty or credible intervals
### Aliases: plot.see_hdi

### ** Examples

## Don't show: 
if (identical(Sys.getenv("NOT_CRAN"), "true") && require("rstanarm")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(rstanarm)
library(bayestestR)
set.seed(123)
m <- suppressWarnings(stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0))
result <- bayestestR::hdi(m)
result
plot(result)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("plot.see_n_factors")
### * plot.see_n_factors

flush(stderr()); flush(stdout())

### Name: plot.see_n_factors
### Title: Plot method for numbers of clusters to extract or factors to
###   retain
### Aliases: plot.see_n_factors

### ** Examples

## Don't show: 
if (require("nFactors")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
data(mtcars)
result <- parameters::n_factors(mtcars, type = "PCA")
result

plot(result) # type = "bar" by default
plot(result, type = "line")
plot(result, type = "area")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("plot.see_p_direction")
### * plot.see_p_direction

flush(stderr()); flush(stdout())

### Name: plot.see_p_direction
### Title: Plot method for probability of direction
### Aliases: plot.see_p_direction

### ** Examples

## Don't show: 
if (identical(Sys.getenv("NOT_CRAN"), "true") && require("rstanarm")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(rstanarm)
library(bayestestR)
set.seed(123)
m <<- suppressWarnings(stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0))
result <- p_direction(m)
plot(result)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("plot.see_p_function")
### * plot.see_p_function

flush(stderr()); flush(stdout())

### Name: plot.see_p_function
### Title: Plot method for plotting p-functions (aka consonance functions)
### Aliases: plot.see_p_function

### ** Examples

library(parameters)
model <- lm(Sepal.Length ~ Species + Sepal.Width + Petal.Length, data = iris)
result <- p_function(model)
plot(result, n_columns = 2, show_labels = FALSE)

result <- p_function(model, keep = "Sepal.Width")
plot(result)



cleanEx()
nameEx("plot.see_p_significance")
### * plot.see_p_significance

flush(stderr()); flush(stdout())

### Name: plot.see_p_significance
### Title: Plot method for practical significance
### Aliases: plot.see_p_significance

### ** Examples

## Don't show: 
if (identical(Sys.getenv("NOT_CRAN"), "true") && require("rstanarm")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(rstanarm)
library(bayestestR)
set.seed(123)
m <<- suppressWarnings(stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0))
result <- p_significance(m)
plot(result)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("plot.see_parameters_brms_meta")
### * plot.see_parameters_brms_meta

flush(stderr()); flush(stdout())

### Name: plot.see_parameters_brms_meta
### Title: Plot method for Model Parameters from Bayesian Meta-Analysis
### Aliases: plot.see_parameters_brms_meta

### ** Examples

## Don't show: 
if (require("brms") && require("metafor") && require("RcppEigen") && require("BH")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



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
m <<- lm(mpg ~ wt + cyl + gear, data = mtcars)
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



cleanEx()
nameEx("plot.see_performance_simres")
### * plot.see_performance_simres

flush(stderr()); flush(stdout())

### Name: plot.see_performance_simres
### Title: Plot method for check model for (non-)normality of residuals
### Aliases: plot.see_performance_simres

### ** Examples

## Don't show: 
if (require("glmmTMB") && require("qqplotr") && require("DHARMa")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
data(Salamanders, package = "glmmTMB")
model <- glmmTMB::glmmTMB(
  count ~ mined + spp + (1 | site),
  family = poisson(),
  data = Salamanders
)
simulated_residuals <- performance::simulate_residuals(model)
plot(simulated_residuals)

# or
simulated_residuals <- performance::simulate_residuals(model)
result <- performance::check_residuals(simulated_residuals)
plot(result)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("plot.see_point_estimate")
### * plot.see_point_estimate

flush(stderr()); flush(stdout())

### Name: plot.see_point_estimate
### Title: Plot method for point estimates of posterior samples
### Aliases: plot.see_point_estimate

### ** Examples

## Don't show: 
if (identical(Sys.getenv("NOT_CRAN"), "true") && require("rstanarm")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(rstanarm)
library(bayestestR)
set.seed(123)
m <<- suppressWarnings(stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0))
result <- point_estimate(m, centrality = "median")
result
plot(result)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("plot.see_rope")
### * plot.see_rope

flush(stderr()); flush(stdout())

### Name: plot.see_rope
### Title: Plot method for Region of Practical Equivalence
### Aliases: plot.see_rope

### ** Examples

## Don't show: 
if (identical(Sys.getenv("NOT_CRAN"), "true") && require("rstanarm")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(rstanarm)
library(bayestestR)
set.seed(123)
m <<- suppressWarnings(stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0))
result <- rope(m)
result
plot(result)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("plot.see_si")
### * plot.see_si

flush(stderr()); flush(stdout())

### Name: plot.see_si
### Title: Plot method for support intervals
### Aliases: plot.see_si

### ** Examples

## Don't show: 
if (identical(Sys.getenv("NOT_CRAN"), "true") && require("rstanarm")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(rstanarm)
library(bayestestR)
set.seed(123)
m <<- suppressWarnings(stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0))
result <- si(m, verbose = FALSE)
result
plot(result)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("plots")
### * plots

flush(stderr()); flush(stdout())

### Name: plots
### Title: Multiple plots side by side
### Aliases: plots

### ** Examples

## Don't show: 
if (require("patchwork", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(ggplot2)
library(see)

p1 <- ggplot(mtcars, aes(x = disp, y = mpg)) +
  geom_point()
p2 <- ggplot(mtcars, aes(x = mpg)) +
  geom_density()
p3 <- ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar() +
  scale_x_discrete("cyl")

plots(p1, p2)
plots(p1, p2, n_columns = 2, tags = "A")
plots(
  p1, p2, p3,
  n_columns = 1, tags = c("Fig. 1", "Fig. 2", "Fig. 3"),
  title = "The surprising truth about mtcars"
)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("print.see_performance_pp_check")
### * print.see_performance_pp_check

flush(stderr()); flush(stdout())

### Name: print.see_performance_pp_check
### Title: Plot method for posterior predictive checks
### Aliases: print.see_performance_pp_check plot.see_performance_pp_check

### ** Examples

library(performance)

model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
check_predictions(model)

# dot-plot style for count-models
d <- iris
d$poisson_var <- rpois(150, 1)
model <- glm(
  poisson_var ~ Species + Petal.Length + Petal.Width,
  data = d,
  family = poisson()
)
out <- check_predictions(model)
plot(out, type = "discrete_dots")



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
nameEx("scale_color_colorhex")
### * scale_color_colorhex

flush(stderr()); flush(stdout())

### Name: scale_color_colorhex
### Title: Color palettes from color-hex
### Aliases: scale_color_colorhex scale_color_colorhex_d
###   scale_color_colorhex_c scale_colour_colorhex scale_colour_colorhex_c
###   scale_colour_colorhex_d scale_fill_colorhex scale_fill_colorhex_d
###   scale_fill_colorhex_c

### ** Examples

library(ggplot2)
library(see)

ggplot(iris, aes(x = Species, y = Sepal.Length, color = Species)) +
  geom_boxplot() +
  theme_modern() +
  scale_color_colorhex_d(palette = 1014416)

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin() +
  theme_modern() +
  scale_fill_colorhex_d(palette = 1014416)

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
  geom_point() +
  theme_modern() +
  scale_color_colorhex_c(palette = 1014416)



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
  scale_fill_flat()

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin() +
  theme_modern() +
  scale_fill_flat(palette = "ice")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
  geom_point() +
  theme_modern() +
  scale_color_flat(discrete = FALSE)



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
  scale_fill_material()

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin() +
  theme_modern() +
  scale_fill_material(palette = "ice")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
  geom_point() +
  theme_modern() +
  scale_color_material(discrete = FALSE)



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
  scale_fill_metro()

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin() +
  theme_modern() +
  scale_fill_metro(palette = "ice")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
  geom_point() +
  theme_modern() +
  scale_color_metro(discrete = FALSE)



cleanEx()
nameEx("scale_color_okabeito")
### * scale_color_okabeito

flush(stderr()); flush(stdout())

### Name: scale_color_okabeito
### Title: Okabe-Ito color palette
### Aliases: scale_color_okabeito scale_fill_okabeito scale_colour_okabeito
###   scale_colour_oi scale_color_oi scale_fill_oi

### ** Examples

library(ggplot2)
library(see)

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  theme_modern() +
  scale_fill_okabeito()

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin() +
  theme_modern() +
  scale_fill_oi(palette = "black_first")

# for the original brighter yellow color suggested by Okabe and Ito
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin() +
  theme_modern() +
  scale_fill_oi(palette = "full")

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin() +
  theme_modern() +
  scale_fill_oi(order = c(1, 5, 6, 2, 4, 3, 7))



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
  scale_fill_see()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_point() +
  theme_abyss() +
  scale_colour_see(palette = "light")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
  geom_point() +
  theme_modern() +
  scale_color_see(discrete = FALSE)



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
  scale_fill_social()

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin() +
  theme_modern() +
  scale_fill_social(palette = "ice")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
  geom_point() +
  theme_modern() +
  scale_color_social(discrete = FALSE)



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
nameEx("theme_azurelight")
### * theme_azurelight

flush(stderr()); flush(stdout())

### Name: theme_azurelight
### Title: Azurelight theme
### Aliases: theme_azurelight

### ** Examples

library(ggplot2)
library(see)

data(iris)

ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) +
  geom_point2(size = 2.5) +
  scale_color_social() +
  theme_azurelight()



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
  geom_point(color = see_colors("lime")) +
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
  geom_point() +
  scale_color_metro() +
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
  scale_color_see() +
  theme_modern()

# for a slightly better orientation, tick marks can be added
ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
  geom_point() +
  scale_color_see() +
  theme_modern(show.ticks = TRUE)



cleanEx()
nameEx("theme_radar")
### * theme_radar

flush(stderr()); flush(stdout())

### Name: theme_radar
### Title: Themes for radar plots
### Aliases: theme_radar theme_radar_dark

### ** Examples

library(ggplot2)

data <- datawizard::reshape_longer(
  aggregate(iris[-5], list(Species = iris$Species), mean),
  c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
)

ggplot(
  data,
  aes(
    x = name,
    y = value,
    color = Species,
    group = Species,
    fill = Species
  )
) +
  geom_polygon(linewidth = 1, alpha = 0.1) +
  coord_radar() +
  theme_radar()



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
