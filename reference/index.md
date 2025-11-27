# Package index

## Epidemic Curves

Functions for creating and styling epidemic curves

- [`geom_epicurve()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md)
  [`stat_bin_date()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md)
  [`stat_date_count()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md)
  [`geom_epicurve_text()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md)
  [`geom_epicurve_point()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md)
  : Create an epidemic curve plot or bin/count observations by date
  periods
- [`scale_y_cases_5er()`](https://ggsurveillance.biostats.dev/reference/scale_y_cases_5er.md)
  [`scale_x_cases_5er()`](https://ggsurveillance.biostats.dev/reference/scale_y_cases_5er.md)
  : Continuous x-axis and y-axis scale for (case) counts
- [`geom_vline_year()`](https://ggsurveillance.biostats.dev/reference/geom_vline_year.md)
  [`geom_hline_year()`](https://ggsurveillance.biostats.dev/reference/geom_vline_year.md)
  : Automatically create lines at the turn of every year
- [`label_date()`](https://ggsurveillance.biostats.dev/reference/label_date.md)
  [`label_date_short()`](https://ggsurveillance.biostats.dev/reference/label_date.md)
  : Date labeller
- [`guide_axis_nested_date()`](https://ggsurveillance.biostats.dev/reference/guide_axis_nested_date.md)
  : Nested axis guide for date scales

## Diverging Bar Charts and Diverging Area Charts

Functions for diverging plots for population pyramids, likert scales,
etc.

- [`geom_bar_diverging()`](https://ggsurveillance.biostats.dev/reference/geom_bar_diverging.md)
  [`geom_area_diverging()`](https://ggsurveillance.biostats.dev/reference/geom_bar_diverging.md)
  [`stat_diverging()`](https://ggsurveillance.biostats.dev/reference/geom_bar_diverging.md)
  : Create diverging bar charts, diverging area charts or other plots
  for opposing categorical data.
- [`scale_x_continuous_diverging()`](https://ggsurveillance.biostats.dev/reference/scale_continuous_diverging.md)
  [`scale_y_continuous_diverging()`](https://ggsurveillance.biostats.dev/reference/scale_continuous_diverging.md)
  : Diverging continuous scales for diverging bar charts with
  symmetrical limits
- [`scale_y_discrete_reverse()`](https://ggsurveillance.biostats.dev/reference/scale_y_discrete_reverse.md)
  [`scale_x_discrete_reverse()`](https://ggsurveillance.biostats.dev/reference/scale_y_discrete_reverse.md)
  : Reversed discrete scale for 'ggplot2'

## Date Binning and Seasonal Datr Alignment

Functions for binning cases by date intervalls and aligning and
comparing data across seasons

- [`bin_by_date()`](https://ggsurveillance.biostats.dev/reference/bin_by_date.md)
  : Aggregate data by time periods
- [`align_dates_seasonal()`](https://ggsurveillance.biostats.dev/reference/align_dates_seasonal.md)
  [`align_and_bin_dates_seasonal()`](https://ggsurveillance.biostats.dev/reference/align_dates_seasonal.md)
  : Align dates for seasonal comparison

## EpiGantt Charts

Functions for EpiGantt Charts

- [`geom_epigantt()`](https://ggsurveillance.biostats.dev/reference/geom_epigantt.md)
  : Epi Gantt Chart: Visualize Epidemiological Time Intervals
- [`scale_y_discrete_reverse()`](https://ggsurveillance.biostats.dev/reference/scale_y_discrete_reverse.md)
  [`scale_x_discrete_reverse()`](https://ggsurveillance.biostats.dev/reference/scale_y_discrete_reverse.md)
  : Reversed discrete scale for 'ggplot2'

## Data Manipulation Helpers

Various functions helper functions for common epi tasks

- [`create_agegroups()`](https://ggsurveillance.biostats.dev/reference/create_agegroups.md)
  : Create Age Groups from Numeric Values
- [`geometric_mean()`](https://ggsurveillance.biostats.dev/reference/geometric_mean.md)
  : Compute a Geometric Mean
- [`uncount()`](https://ggsurveillance.biostats.dev/reference/uncount.md)
  [`expand_counts()`](https://ggsurveillance.biostats.dev/reference/uncount.md)
  : Duplicate rows according to a weighting variable

## Datasets

Included datasets for examples and demonstrations

- [`influenza_germany`](https://ggsurveillance.biostats.dev/reference/influenza_germany.md)
  : German Influenza (FLU) Surveillance data
- [`linelist_hospital_outbreak`](https://ggsurveillance.biostats.dev/reference/linelist_hospital_outbreak.md)
  : Line list of a fictional hospital outbreak (Data)
- [`population_german_states`](https://ggsurveillance.biostats.dev/reference/population_german_states.md)
  : Population of the German states (2023)

## ggplot2 Layers and Functions

### Geoms and Stats

Geoms and Stats in this package

- [`geom_bar_diverging()`](https://ggsurveillance.biostats.dev/reference/geom_bar_diverging.md)
  [`geom_area_diverging()`](https://ggsurveillance.biostats.dev/reference/geom_bar_diverging.md)
  [`stat_diverging()`](https://ggsurveillance.biostats.dev/reference/geom_bar_diverging.md)
  : Create diverging bar charts, diverging area charts or other plots
  for opposing categorical data.
- [`geom_col_range()`](https://ggsurveillance.biostats.dev/reference/geom_col_range.md)
  : Create a ranged bar chart
- [`geom_epicurve()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md)
  [`stat_bin_date()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md)
  [`stat_date_count()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md)
  [`geom_epicurve_text()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md)
  [`geom_epicurve_point()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md)
  : Create an epidemic curve plot or bin/count observations by date
  periods
- [`geom_epigantt()`](https://ggsurveillance.biostats.dev/reference/geom_epigantt.md)
  : Epi Gantt Chart: Visualize Epidemiological Time Intervals
- [`geom_vline_year()`](https://ggsurveillance.biostats.dev/reference/geom_vline_year.md)
  [`geom_hline_year()`](https://ggsurveillance.biostats.dev/reference/geom_vline_year.md)
  : Automatically create lines at the turn of every year
- [`stat_last_value()`](https://ggsurveillance.biostats.dev/reference/stat_last_value.md)
  [`geom_label_last_value()`](https://ggsurveillance.biostats.dev/reference/stat_last_value.md)
  [`geom_text_last_value()`](https://ggsurveillance.biostats.dev/reference/stat_last_value.md)
  [`geom_label_last_value_repel()`](https://ggsurveillance.biostats.dev/reference/stat_last_value.md)
  [`geom_text_last_value_repel()`](https://ggsurveillance.biostats.dev/reference/stat_last_value.md)
  : Add labels or points to the last value of a line chart

### Scales and Guides

ggplot2 scales and guides in this package

- [`scale_x_continuous_diverging()`](https://ggsurveillance.biostats.dev/reference/scale_continuous_diverging.md)
  [`scale_y_continuous_diverging()`](https://ggsurveillance.biostats.dev/reference/scale_continuous_diverging.md)
  : Diverging continuous scales for diverging bar charts with
  symmetrical limits
- [`scale_y_cases_5er()`](https://ggsurveillance.biostats.dev/reference/scale_y_cases_5er.md)
  [`scale_x_cases_5er()`](https://ggsurveillance.biostats.dev/reference/scale_y_cases_5er.md)
  : Continuous x-axis and y-axis scale for (case) counts
- [`scale_y_discrete_reverse()`](https://ggsurveillance.biostats.dev/reference/scale_y_discrete_reverse.md)
  [`scale_x_discrete_reverse()`](https://ggsurveillance.biostats.dev/reference/scale_y_discrete_reverse.md)
  : Reversed discrete scale for 'ggplot2'
- [`guide_axis_nested_date()`](https://ggsurveillance.biostats.dev/reference/guide_axis_nested_date.md)
  : Nested axis guide for date scales

### Labeling and Formatting

Functions for formatting labels and annotations

- [`label_date()`](https://ggsurveillance.biostats.dev/reference/label_date.md)
  [`label_date_short()`](https://ggsurveillance.biostats.dev/reference/label_date.md)
  : Date labeller
- [`label_skip()`](https://ggsurveillance.biostats.dev/reference/label_skip.md)
  : Skip labels on an axis
- [`label_power10()`](https://ggsurveillance.biostats.dev/reference/label_power10.md)
  : Format numbers as power-of-10 R expressions

### Theming and Styling

Functions for customizing plot appearance

- [`theme_mod_disable_legend()`](https://ggsurveillance.biostats.dev/reference/theme_mod_disable_legend.md)
  [`theme_mod_legend_position()`](https://ggsurveillance.biostats.dev/reference/theme_mod_disable_legend.md)
  [`theme_mod_legend_top()`](https://ggsurveillance.biostats.dev/reference/theme_mod_disable_legend.md)
  [`theme_mod_legend_bottom()`](https://ggsurveillance.biostats.dev/reference/theme_mod_disable_legend.md)
  [`theme_mod_legend_left()`](https://ggsurveillance.biostats.dev/reference/theme_mod_disable_legend.md)
  [`theme_mod_legend_right()`](https://ggsurveillance.biostats.dev/reference/theme_mod_disable_legend.md)
  [`theme_mod_remove_legend_title()`](https://ggsurveillance.biostats.dev/reference/theme_mod_disable_legend.md)
  : Quickly adjust the legend position
- [`theme_mod_remove_minor_grid()`](https://ggsurveillance.biostats.dev/reference/theme_mod_remove_minor_grid.md)
  [`theme_mod_remove_minor_grid_y()`](https://ggsurveillance.biostats.dev/reference/theme_mod_remove_minor_grid.md)
  [`theme_mod_remove_minor_grid_x()`](https://ggsurveillance.biostats.dev/reference/theme_mod_remove_minor_grid.md)
  [`theme_mod_remove_panel_grid()`](https://ggsurveillance.biostats.dev/reference/theme_mod_remove_minor_grid.md)
  : Quickly remove the minor lines of the panel grid
- [`theme_mod_rotate_x_axis_labels()`](https://ggsurveillance.biostats.dev/reference/theme_mod_rotate_axis_labels.md)
  [`theme_mod_rotate_x_axis_labels_90()`](https://ggsurveillance.biostats.dev/reference/theme_mod_rotate_axis_labels.md)
  [`theme_mod_rotate_x_axis_labels_45()`](https://ggsurveillance.biostats.dev/reference/theme_mod_rotate_axis_labels.md)
  [`theme_mod_rotate_x_axis_labels_30()`](https://ggsurveillance.biostats.dev/reference/theme_mod_rotate_axis_labels.md)
  [`theme_mod_rotate_x_axis_labels_60()`](https://ggsurveillance.biostats.dev/reference/theme_mod_rotate_axis_labels.md)
  [`theme_mod_rotate_y_axis_labels()`](https://ggsurveillance.biostats.dev/reference/theme_mod_rotate_axis_labels.md)
  : Rotate axis labels
