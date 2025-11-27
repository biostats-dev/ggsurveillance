# Continuous x-axis and y-axis scale for (case) counts

A continuous ggplot scale for count data with sane defaults for breaks.
It uses [`base::pretty()`](https://rdrr.io/r/base/pretty.html) to
increase the default number of breaks and prefers 5er breaks.
Additionally, the first tick (i.e. zero) is aligned to the lower left
corner.

## Usage

``` r
scale_y_cases_5er(
  name = waiver(),
  n = 8,
  min.n = 5,
  u5.bias = 4,
  expand = NULL,
  limits = c(0, NA),
  labels = waiver(),
  oob = scales::censor,
  na.value = NA_real_,
  transform = "identity",
  position = "left",
  sec.axis = waiver(),
  guide = waiver(),
  ...
)

scale_x_cases_5er(
  name = waiver(),
  n = 8,
  min.n = 5,
  u5.bias = 4,
  expand = NULL,
  limits = c(0, NA),
  labels = waiver(),
  oob = scales::censor,
  na.value = NA_real_,
  transform = "identity",
  position = "bottom",
  sec.axis = waiver(),
  guide = waiver(),
  ...
)
```

## Arguments

- name:

  The name of the scale. Used as the axis or legend title. If
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html), the
  default, the name of the scale is taken from the first mapping used
  for that aesthetic. If `NULL`, the legend title will be omitted.

- n:

  Target number of breaks passed to
  [`base::pretty()`](https://rdrr.io/r/base/pretty.html). Defaults to 8.

- min.n:

  Minimum number of breaks passed to
  [`base::pretty()`](https://rdrr.io/r/base/pretty.html). Defaults to 5.

- u5.bias:

  The "5-bias" parameter passed to
  [`base::pretty()`](https://rdrr.io/r/base/pretty.html); higher values
  push the breaks more strongly toward multiples of 5. Defaults to 4.

- expand:

  Uses own expansion logic. Use `expand = waiver()` to restore ggplot
  defaults or
  [`ggplot2::expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html)
  to modify

- limits:

  The lower limit defaults to 0 and the upper limits is chosen based on
  the data. This is the recommended approach for visualizing case
  numbers and incidences, i.e. the scale starts at 0 and is only
  positive. To use the default `ggplot2` limits use `limits = NULL`.

- labels:

  One of the options below. Please note that when `labels` is a vector,
  it is highly recommended to also set the `breaks` argument as a vector
  to protect against unintended mismatches.

  - `NULL` for no labels

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    for the default labels computed by the transformation object

  - A character vector giving labels (must be same length as `breaks`)

  - An expression vector (must be the same length as breaks). See
    ?plotmath for details.

  - A function that takes the breaks as input and returns labels as
    output. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

- oob:

  One of:

  - Function that handles limits outside of the scale limits (out of
    bounds). Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

  - The default
    ([`scales::censor()`](https://scales.r-lib.org/reference/oob.html))
    replaces out of bounds values with `NA`.

  - [`scales::squish()`](https://scales.r-lib.org/reference/oob.html)
    for squishing out of bounds values into range.

  - [`scales::squish_infinite()`](https://scales.r-lib.org/reference/oob.html)
    for squishing infinite values into range.

- na.value:

  Missing values will be replaced with this value.

- transform:

  For continuous scales, the name of a transformation object or the
  object itself. Built-in transformations include "asn", "atanh",
  "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p",
  "log2", "logit", "modulus", "probability", "probit", "pseudo_log",
  "reciprocal", "reverse", "sqrt" and "time".

  A transformation object bundles together a transform, its inverse, and
  methods for generating breaks and labels. Transformation objects are
  defined in the scales package, and are called `transform_<name>`. If
  transformations require arguments, you can call them from the scales
  package, e.g.
  [`scales::transform_boxcox(p = 2)`](https://scales.r-lib.org/reference/transform_boxcox.html).
  You can create your own transformation with
  [`scales::new_transform()`](https://scales.r-lib.org/reference/new_transform.html).

- position:

  For position scales, The position of the axis. `left` or `right` for y
  axes, `top` or `bottom` for x axes.

- sec.axis:

  [`sec_axis()`](https://ggplot2.tidyverse.org/reference/sec_axis.html)
  is used to specify a secondary axis.

- guide:

  A function used to create a guide or its name. See
  [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html) for
  more information.

- ...:

  Additional arguments passed on to
  [`base::pretty()`](https://rdrr.io/r/base/pretty.html).

## Value

A `ggplot2` scale object that can be added to a plot.

## See also

[`geom_epicurve()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md),
[`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html),
[`base::pretty()`](https://rdrr.io/r/base/pretty.html),
[`theme_mod_remove_minor_grid_y()`](https://ggsurveillance.biostats.dev/reference/theme_mod_remove_minor_grid.md)

## Examples

``` r
library(ggplot2)

data <- data.frame(date = as.Date("2024-01-01") + 0:30)
ggplot(data, aes(x = date)) +
  geom_epicurve(date_resolution = "week") +
  scale_y_cases_5er() +
  theme_mod_remove_minor_grid_y()
```
