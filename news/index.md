# Changelog

## ggsurveillance 0.5.2

CRAN release: 2025-11-27

- [ggplot2](https://ggplot2.tidyverse.org) 4.0.0 compatibility fixes.
- [`geom_epicurve()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md):
  Now honours the `width` parameter correctly.
- [`label_power10()`](https://ggsurveillance.biostats.dev/reference/label_power10.md):
  Add “cdot” (e.g. \\2 \cdot 10^5\\) as an option for the multiplication
  symbol. Improve `NA` handling.
- [`stat_last_value()`](https://ggsurveillance.biostats.dev/reference/stat_last_value.md):
  Apply `labeller` if a numeric value is provided for `label` aesthetic.
- [`create_agegroups()`](https://ggsurveillance.biostats.dev/reference/create_agegroups.md):
  Improve handling of age 0. Negative ages are now treated as `NA` or
  will be labelled according to `na_label`. Improve number padding.

## ggsurveillance 0.5.1

CRAN release: 2025-07-02

- Bug fix: Fix corner cases for the `fill_gaps` option in
  [`bin_by_date()`](https://ggsurveillance.biostats.dev/reference/bin_by_date.md)

## ggsurveillance 0.5.0

CRAN release: 2025-07-01

### New Features

- [`bin_by_date()`](https://ggsurveillance.biostats.dev/reference/bin_by_date.md):
  New `tidyverse`-compatible function for flexible date-based
  aggregation (binning). This function was previously internal to
  [`geom_epicurve()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md)
  and
  [`stat_bin_date()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md).
  - It includes a simpler and faster `fill_gaps` argument to
    automatically fill gaps in a time series with 0s.
- [`guide_axis_nested_date()`](https://ggsurveillance.biostats.dev/reference/guide_axis_nested_date.md):
  New axis guide for creating nested date labels for hierarchical time
  periods (e.g., year \> month \> day). This feature is powered by the
  [legendry](https://teunbrand.github.io/legendry/) package.
- [`label_power10()`](https://ggsurveillance.biostats.dev/reference/label_power10.md):
  New `ggplot2`-compatible labeling function to format numbers in
  scientific notation with powers of 10 (e.g., \\2 \times 10^5\\).
- [`geom_epicurve_text()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md)
  and
  [`geom_epicurve_point()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md):
  New geoms to easily add text annotations or points to cases in
  epidemic curves created with
  [`geom_epicurve()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md).

### Breaking Changes

- [`scale_y_cases_5er()`](https://ggsurveillance.biostats.dev/reference/scale_y_cases_5er.md)
  now defaults to starting at 0, providing more intuitive and accurate
  case count visualizations. The previous behaviour can be restored by
  setting `scale_y_cases_5er(limits = NULL)`.

## ggsurveillance 0.4.0

CRAN release: 2025-05-09

- [`geom_bar_diverging()`](https://ggsurveillance.biostats.dev/reference/geom_bar_diverging.md)
  for diverging bar charts, including:
  - [`stat_diverging()`](https://ggsurveillance.biostats.dev/reference/geom_bar_diverging.md)
    for easy labeling of these charts
  - [`scale_x_continuous_diverging()`](https://ggsurveillance.biostats.dev/reference/scale_continuous_diverging.md):
    Creates symmetric diverging scales
  - [`geom_area_diverging()`](https://ggsurveillance.biostats.dev/reference/geom_bar_diverging.md)
    for continuous variables.
  - [`geom_col_range()`](https://ggsurveillance.biostats.dev/reference/geom_col_range.md):
    The underlying geom which creates bars from `x`, `ymin` and `ymax`
    (or flipped).
- [`geom_label_last_value()`](https://ggsurveillance.biostats.dev/reference/stat_last_value.md)
  for labeling of the last value of a time series (like
  [`geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html))
  - [`stat_last_value()`](https://ggsurveillance.biostats.dev/reference/stat_last_value.md)
    pulls the coordinates of the last value. E.g. can be used to add a
    point to the end of the line.
  - [`geom_label_last_value_repel()`](https://ggsurveillance.biostats.dev/reference/stat_last_value.md):
    [ggrepel](https://ggrepel.slowkow.com/) versions for crowded plots
    with multiple lines
- New dataset: `population_german_states`
- [`label_skip()`](https://ggsurveillance.biostats.dev/reference/label_skip.md)
  for skipping axis labels, e.g. only label every second tick
- re-export
  [`label_date()`](https://ggsurveillance.biostats.dev/reference/label_date.md)
  and
  [`label_date_short()`](https://ggsurveillance.biostats.dev/reference/label_date.md)
  from scales for date labels with a custom locale.
- Improvements and bug fixes

## ggsurveillance 0.3.0

CRAN release: 2025-04-11

- new `ggplot2` theme modification helpers:
  - [`theme_mod_rotate_x_axis_labels()`](https://ggsurveillance.biostats.dev/reference/theme_mod_rotate_axis_labels.md):
    rotate axis labels
  - [`theme_mod_legend_position()`](https://ggsurveillance.biostats.dev/reference/theme_mod_disable_legend.md):
    legend positioning
  - [`theme_mod_remove_minor_grid()`](https://ggsurveillance.biostats.dev/reference/theme_mod_remove_minor_grid.md)
    : remove minor panel grid lines (x, y or both) or all grind lines
- [`geom_vline_year()`](https://ggsurveillance.biostats.dev/reference/geom_vline_year.md)
  now also supports year_breaks based on weeks. Since weeks don’t fall
  on the same date every year.
- `tsibble` now optional dependency to improve speed of first install
- add `plotly` compatibility for
  [`geom_epicurve()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md)
  and
  [`geom_epigantt()`](https://ggsurveillance.biostats.dev/reference/geom_epigantt.md)
- [`scale_y_cases_5er()`](https://ggsurveillance.biostats.dev/reference/scale_y_cases_5er.md):
  rename to `min.n` (from `n.min`) for correct
  [`base::pretty()`](https://rdrr.io/r/base/pretty.html) compatibility
- Bug fixes and minor improvements

## ggsurveillance 0.2.0

CRAN release: 2025-03-02

- Update
  [`geom_epigantt()`](https://ggsurveillance.biostats.dev/reference/geom_epigantt.md):
  Add auto-scaling for linewidth and update documentation
- Add
  [`scale_y_discrete_reverse()`](https://ggsurveillance.biostats.dev/reference/scale_y_discrete_reverse.md)
- New dataset of a fictional hospital outbreak
- [`geometric_mean()`](https://ggsurveillance.biostats.dev/reference/geometric_mean.md):
  Add an option to disable warnings
- Minor fixes

## ggsurveillance 0.1.2

CRAN release: 2025-02-11

- Documentation improvements and bug fixes for
  [`geom_epicurve()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md)

## ggsurveillance 0.1.1

CRAN release: 2025-01-31

- First release
