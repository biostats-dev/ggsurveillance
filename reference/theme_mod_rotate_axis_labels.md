# Rotate axis labels

Rotate axis labels by 90°, 45° or any angle. Has to be called after
setting the theme.

## Usage

``` r
theme_mod_rotate_x_axis_labels(
  angle = 90,
  margin_top = 2,
  vjust = 0.4,
  hjust = 0,
  ...
)

theme_mod_rotate_x_axis_labels_90(angle = 90, ...)

theme_mod_rotate_x_axis_labels_45(angle = 45, ...)

theme_mod_rotate_x_axis_labels_30(angle = 30, ...)

theme_mod_rotate_x_axis_labels_60(angle = 60, ...)

theme_mod_rotate_y_axis_labels(angle = 90, hjust = 0.5, vjust = 0, ...)
```

## Arguments

- angle:

  Angle of rotation. Should be between 10 and 90 degrees.

- margin_top:

  Used to move the tick labels downwards to prevent text intersecting
  the x-axis. Increase for angled multiline text (e.g. 5 for two lines
  at 45°).

- hjust, vjust:

  Text justification within the rotated text element. Just ignore.

- ...:

  Arguments passed to `theme_mod_rotate_x_axis_labels` and
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html).

## Value

Changes the rotation of the axis labels by modifying the `axis.text` of
the
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).
