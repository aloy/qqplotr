# qqplotr 0.0.1.9000

* Created the Shiny app and commited its first version (WIP).


* Error handling for the parameters in all implemented functions.

* Option to partially match some arguments.

* Fixed a bug caused by `stat_qq_line` default colors when facetting.


* Created the vignette `introduction.Rmd`.

* Removed most inheritances from Q-Q functions, as they were not essential.

* Detrended option on P-P functions.

* Changed the P-P functions labels in the examples from the documentation.


* Implemented the P-P band stat `stat_pp_band` via parametric bootstrap.

* Created a description for the package itself, i.e., now `?qqplotr` works.


* Implemented the P-P plot stats: `stat_pp_point` and `stat_pp_line`.

# qqplotr 0.0.1

* Initial release of the package.

* Implementation of three ggplot2 stats: `stat_qq_point`, `stat_qq_line` and
`stat_qq_band`.
