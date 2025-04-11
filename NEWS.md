# ggsurveillance 0.3.0

-   new `ggplot2` theme modification helpers:
    -   rotate axis labels
    -   legend positioning
    -   remove minor panel grid lines (x, y or both) or all grind lines
-   `geom_vline_year()` now also supports year_breaks based on weeks. Since weeks don't fall on the same date every year.
-   `tsibble` now optional dependency to improve speed of first install
-   add `plotly` compatibility for `geom_epicurve()` and `geom_epigantt()`
-   rename `min.n` (from `n.min`) in `scale_y_cases_5er()` for correct `base::pretty()` compatibility
-   Bug fixes and minor improvements

# ggsurveillance 0.2.0

-   Update `geom_epigantt()`: Add auto-scaling for linewidth and update documentation
-   Add `scale_y_discrete_reverse()`
-   New dataset of a fictional hospital outbreak
-   `geometric_mean()`: Add an option to disable warnings
-   Minor fixes

# ggsurveillance 0.1.2

-   Documentation improvements and bug fixes for `geom_epicurve()`

# ggsurveillance 0.1.1

-   First release
