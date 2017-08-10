# qqplotr 0.0.1.9001

* Removed the hacky way of forcing `stat_pp_point` plot to remain inside the
unit square. It was giving inheritance problems in `stat_qq_band`.

* Implemented the P-P band stat `stat_pp_band` via parametric bootstrap.

* Created a description for the package itself, i.e., now `?qqplotr` works.

* Included and automated the task of placing a dev version badge on the README.

* Implemented the P-P plot stats: `stat_pp_point` and `stat_pp_line`.

# qqplotr 0.0.1

* Initial release of the package.

* Implementation of three ggplot2 stats: `stat_qq_point`, `stat_qq_line` and
`stat_qq_band`.
