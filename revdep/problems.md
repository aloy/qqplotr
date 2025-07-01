# plotthis

<details>

* Version: 0.7.1
* GitHub: https://github.com/pwwang/plotthis
* Source code: https://github.com/cran/plotthis
* Date/Publication: 2025-06-20 18:40:02 UTC
* Number of recursive dependencies: 212

Run `revdepcheck::revdep_details(, "plotthis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘plotthis-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: QQPlot
    > ### Title: QQ plot
    > ### Aliases: QQPlot
    > 
    > ### ** Examples
    > 
    > set.seed(8525)
    ...
    > QQPlot(data, val = "norm", band = list(
    +     list(bandType = "ks", mapping = ggplot2::aes(fill = "KS"), alpha = 0.3),
    +     list(bandType = "ts", mapping = ggplot2::aes(fill = "TS")),
    +     list(bandType = "pointwise", mapping = ggplot2::aes(fill = "Normal")),
    +     list(bandType = "boot", mapping = ggplot2::aes(fill = "Bootstrap"))
    + ), band_alpha = 0.6)
    Error in gg_par(col = data$colour %||% NA, fill = fill_alpha(data$fill %||%  : 
      could not find function "gg_par"
    Calls: <Anonymous> ... lapply -> FUN -> <Anonymous> -> draw_key -> rectGrob -> grob
    Execution halted
    ```

