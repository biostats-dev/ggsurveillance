# Quickly adjust the legend position

Convenience functions to control the legend position for `ggplot2`. Has
to be called after setting the theme.

## Usage

``` r
theme_mod_disable_legend()

theme_mod_legend_position(
  position = c("top", "bottom", "left", "right", "none", "inside"),
  position.inside = NULL
)

theme_mod_legend_top()

theme_mod_legend_bottom()

theme_mod_legend_left()

theme_mod_legend_right()

theme_mod_remove_legend_title()
```

## Arguments

- position:

  Position of the ggplot2 legend. Options are
  `("top", "bottom", "left", "right", "none", "inside")`

- position.inside:

  Coordinates for the legend inside the plot. If set overwrites
  `position` to `inside`.

## Value

Changes the `legend.position` of the
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).
