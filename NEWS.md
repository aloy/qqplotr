# qqplotr 0.0.1.9000 (to be 0.0.2)

* Distributional parameters are now automatically estimated with MLE for
`stat_qq_point`, `stat_qq_line`, `stat_pp_point`, and `stat_pp_band`, when
`dparams` is not used (default).

* Created a Shiny app to showcase the package functions and its parameters.

* Included error handling for most of the main parameters from all functions.

* Added partial match for some character arguments.

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
