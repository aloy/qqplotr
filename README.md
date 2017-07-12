
<!-- README.md is generated from README.Rmd. Please edit that file -->
qqplotr
=======

[![Build Status](https://travis-ci.org/aloy/qqplotr.svg?branch=master)](https://travis-ci.org/aloy/qqplotr)

Overview
--------

This package extends the functionality of `ggplot2` quantile-quantile plots by permitting the drawing Q-Q lines, confidence bands, and also a detrend option.

Development Version
-------------------

If you would like to download the development version of `qqplotr`, you may do so by using, for example, `devtools`:

``` r
# install.packages("devtools")
library(devtools)
install_github("aloy/qqplotr")
```

Usage
-----

Three Stats are available:

-   `stat_qq_point` Which is essentially a modified version of `ggplot2::stat_qq` with some a parameters adjustments and a new option to detrend the points.
-   `stat_qq_line` Plots a reference line based on the first and third quartiles of the data (as in `stats::qqline`).
-   `stat_qq_band` Plots confidence bands based on three methods: `"normal"`, `"bs"`, and `"ts"`. `"normal"` constructs simultaneous confidence bands based on Normal confidence intervals. `"bs"` creates pointwise confidence bands based on a parametric boostrap. Finally, `"ts"` constructs tail-sensitive confidence bands, as described in Aldor-Noiman et al. (2013).

References
----------

-   [Aldor-Noiman, S. et al. 2013. The Power to See: A New Graphical Test of Normality. The American Statistician.](http://www.tandfonline.com/doi/abs/10.1080/00031305.2013.847865)
