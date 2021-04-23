# qqplotr 0.0.5

* Updated defaults in `StatQqPoint` to allow labeling of points without hassle

* Updated dependencies on rmarkdown for vignette

# qqplotr 0.0.4

* Changed order of arguments in `geom_*` and `stat_*` functions so that mapping is first, then data.

* Fixed fill and color mismatch bug.


# qqplotr 0.0.3

* Added `bandType = "ks"`, which draws simultaneous confidence bands based on an inversion of the Kolmogorov-Smirnov test.

* `bandType = "pointwise"` now produces pointwise confidence bands based on normal theory rather than `bandType = "normal"`.

# qqplotr 0.0.2

* Added the `identity` argument to `*_qq_*` functions, which permits to draw the
identity line (instead of the usual Q-Q line).

* Changed `"bs"` to `"boot"` as one of the possible values of the `bandType`
argument, for obvious reasons. :)

* By default, distributional parameters are now automatically estimated with MLE
for `stat_qq_point`, `stat_qq_line`, `stat_pp_point`, and `stat_pp_band`.

* Created a Shiny app to showcase the package functions and its parameters.

* Included error handling for most of the main parameters from all functions.

* Added partial match for most of the character arguments.

* Fixed a bug caused by `stat_qq_line` default colors when facetting.

* Created the vignette `introduction.Rmd`.

* Removed most inheritances from Q-Q functions, as they were not actually
essential.

* Added detrend option for P-P functions.

* Changed the P-P functions labels in the examples from the documentation.

* Implemented the P-P band stat `stat_pp_band` via parametric bootstrap.

* Created a description for the package itself, i.e., now `?qqplotr` works.

* Implemented the P-P plot stats: `stat_pp_point` and `stat_pp_line`.

# qqplotr 0.0.1

* Initial release of the package.

* Implementation of three ggplot2 stats: `stat_qq_point`, `stat_qq_line` and
`stat_qq_band`.
