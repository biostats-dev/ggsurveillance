# Quickly remove the minor lines of the panel grid

`theme_mod_remove_minor_grid()`, `theme_mod_remove_minor_grid_x()`,
`theme_mod_remove_minor_grid_y()` are convenience functions remove the
minor lines of the panel grid. Has to be called after setting the theme.

## Usage

``` r
theme_mod_remove_minor_grid()

theme_mod_remove_minor_grid_y()

theme_mod_remove_minor_grid_x()

theme_mod_remove_panel_grid()
```

## Value

Changes the `panel.grid.minor` of the
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).
