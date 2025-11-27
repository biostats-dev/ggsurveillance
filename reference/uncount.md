# Duplicate rows according to a weighting variable

`uncount()` is provided by the tidyr package, and re-exported by
ggsurveillance. See
[`tidyr::uncount()`](https://tidyr.tidyverse.org/reference/uncount.html)
for more details.

`uncount()` and its alias `expand_counts()` are complements of
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html):
they take a data.frame with a column of frequencies and duplicate each
row according to those frequencies.

## Usage

``` r
uncount(data, weights, ..., .remove = TRUE, .id = NULL)

expand_counts(data, weights, ..., .remove = TRUE, .id = NULL)
```

## Arguments

- data:

  A data frame, tibble, or grouped tibble.

- weights:

  A vector of weights. Evaluated in the context of `data`; supports
  quasiquotation.

- ...:

  Additional arguments passed on to methods.

- .remove:

  If `TRUE`, and `weights` is the name of a column in `data`, then this
  column is removed.

- .id:

  Supply a string to create a new variable which gives a unique
  identifier for each created row.

## Value

A `data.frame` with rows duplicated according to weights.

## Examples

``` r
df <- data.frame(x = c("a", "b"), n = c(2, 3))
df |> uncount(n)
#>   x
#> 1 a
#> 2 a
#> 3 b
#> 4 b
#> 5 b
# Or equivalently:
df |> expand_counts(n)
#>   x
#> 1 a
#> 2 a
#> 3 b
#> 4 b
#> 5 b
```
