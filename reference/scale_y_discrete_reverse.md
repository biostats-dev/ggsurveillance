# Reversed discrete scale for 'ggplot2'

`scale_y_discrete_reverse()` and `scale_x_discrete_reverse()` are
standard discrete 'ggplot2' scales with a reversed order of values.
Since the ggplot2 coordinate system starts with 0 in the lower left
corner, factors on the y-axis are sorted is descending order by default
(i.e. alphabetically from Z to A). With this scale the the y-axis will
start with the first factor level at the top or with alphabetically
correctly ordered values

## Usage

``` r
scale_y_discrete_reverse(
  name = waiver(),
  limits = NULL,
  ...,
  expand = waiver(),
  position = "left"
)

scale_x_discrete_reverse(
  name = waiver(),
  limits = NULL,
  ...,
  expand = waiver(),
  position = "bottom"
)
```

## Arguments

- name:

  The name of the scale. Used as the axis or legend title. If
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html), the
  default, the name of the scale is taken from the first mapping used
  for that aesthetic. If `NULL`, the legend title will be omitted.

- limits:

  Can be either NULL which uses the default reversed scale values or a
  character vector which will be reversed.

- ...:

  Arguments passed on to
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)

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

[`geom_epigantt()`](https://ggsurveillance.biostats.dev/reference/geom_epigantt.md),
[`ggplot2::scale_y_discrete()`](https://ggplot2.tidyverse.org/reference/scale_discrete.html)

## Examples

``` r
library(ggplot2)

# Create sample data
df <- data.frame(
  category = factor(c("A", "B", "C", "D")),
  value = c(10, 5, 8, 3)
)

# Basic plot with reversed y-axis
ggplot(df, aes(x = value, y = category)) +
  geom_col() +
  scale_y_discrete_reverse()

```
