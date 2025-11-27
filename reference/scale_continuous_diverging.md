# Diverging continuous scales for diverging bar charts with symmetrical limits

These scales automatically create symmetrical limits around a centre
point (zero by default). They're useful for diverging continuous
variables where the visual encoding should be balanced around a center
point, such as positive and negative values. They are intended to be
used with
[`geom_bar_diverging()`](https://ggsurveillance.biostats.dev/reference/geom_bar_diverging.md),
[`geom_area_diverging()`](https://ggsurveillance.biostats.dev/reference/geom_bar_diverging.md)
and
[`stat_diverging()`](https://ggsurveillance.biostats.dev/reference/geom_bar_diverging.md).

## Usage

``` r
scale_x_continuous_diverging(
  name = waiver(),
  limits = waiver(),
  labels = NULL,
  transform = "identity",
  ...,
  breaks = waiver(),
  n.breaks = NULL,
  expand = waiver(),
  position = "bottom"
)

scale_y_continuous_diverging(
  name = waiver(),
  limits = NULL,
  labels = NULL,
  transform = "identity",
  ...,
  breaks = waiver(),
  n.breaks = NULL,
  expand = waiver(),
  position = "left"
)
```

## Arguments

- name:

  The name of the scale. Used as the axis or legend title. If
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html), the
  default, the name of the scale is taken from the first mapping used
  for that aesthetic. If `NULL`, the legend title will be omitted.

- limits:

  Numeric vector of length two providing limits of the scale. If
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html) (the
  default), limits are automatically computed to be symmetrical around
  zero. Use `NULL` for default `ggplot2` limits.

- labels:

  Either
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html), a
  character vector or a function that takes the breaks as input and
  returns labels as output. By default, absolute values are displayed or
  passed to the label function.

- transform:

  Defaults to "identity". Use "reverse" to invert the scale. Especially
  useful to flip the direction of diverging bar charts.

- ...:

  Other arguments passed on to `scale_(x|y)_continuous()`

- breaks:

  One of:

  - `NULL` for no breaks

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    for the default breaks computed by the [transformation
    object](https://scales.r-lib.org/reference/new_transform.html)

  - A numeric vector of positions

  - A function that takes the limits as input and returns breaks as
    output (e.g., a function returned by
    [`scales::extended_breaks()`](https://scales.r-lib.org/reference/breaks_extended.html)).
    Note that for position scales, limits are provided after scale
    expansion. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

- n.breaks:

  An integer guiding the number of major breaks. The algorithm may
  choose a slightly different number to ensure nice break labels. Will
  only have an effect if `breaks = waiver()`. Use `NULL` to use the
  default number of breaks given by the transformation.

- expand:

  For position scales, a vector of range expansion constants used to add
  some padding around the data to ensure that they are placed some
  distance away from the axes. Use the convenience function
  [`expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html)
  to generate the values for the `expand` argument. The defaults are to
  expand the scale by 5% on each side for continuous variables, and by
  0.6 units on each side for discrete variables.

- position:

  For position scales, The position of the axis. `left` or `right` for y
  axes, `top` or `bottom` for x axes.

## Value

A `ggplot2` scale object that can be added to a plot.

## See also

[`geom_bar_diverging()`](https://ggsurveillance.biostats.dev/reference/geom_bar_diverging.md),
[`geom_area_diverging()`](https://ggsurveillance.biostats.dev/reference/geom_bar_diverging.md),
[`stat_diverging()`](https://ggsurveillance.biostats.dev/reference/geom_bar_diverging.md)

## Examples

``` r
library(ggplot2)

# Create sample data with positive and negative values
df <- data.frame(
  x = c(-5, -2, 0, 3, 7),
  y = c(2, -1, 0, -3, 5)
)

# Basic usage
ggplot(df, aes(x, y)) +
  geom_point() +
  scale_x_continuous_diverging() +
  scale_y_continuous_diverging()

```
