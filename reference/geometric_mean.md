# Compute a Geometric Mean

The geometric mean is typically defined for strictly positive values.
This function computes the geometric mean of a numeric vector, with the
option to replace certain values (e.g., zeros, non-positive values, or
values below a user-specified threshold) before computation.

## Usage

``` r
geometric_mean(
  x,
  na.rm = FALSE,
  replace_value = NULL,
  replace = c("all", "non-positive", "zero"),
  warning = TRUE
)
```

## Arguments

- x:

  A numeric or complex vector of values.

- na.rm:

  Logical. If `FALSE` (default), the presence of zero or negative values
  triggers a warning and returns `NA`. If `TRUE`, such values (and any
  `NA`) are removed before computing the geometric mean.

- replace_value:

  Numeric or `NULL`. The value used for replacement, depending on
  `replace` (e.g., a detection limit (LOD) or quantification limit
  (LOQ)). If `NULL`, no replacement is performed. For `replace = "all"`,
  this value is also used as the threshold. For recommendations how to
  use, see details.

- replace:

  Character string indicating which values to replace:

  `"all"`

  :   Replaces all values less than `replace_value` with
      `replace_value`. This is useful if you have a global threshold
      (such as a limit of detection) below which any measurement is
      replaced.

  `"non-positive"`

  :   Replaces all non-positive values (`x <= 0`) with `replace_value`.
      This is helpful if zeros or negative values are known to be
      invalid or below a certain limit.

  `"zero"`

  :   Replaces only exact zeros (`x == 0`) with `replace_value`. Useful
      if negative values should be treated as missing.

- warning:

  Disable warnings by setting it to `FALSE`. Defaults to `TRUE`.

## Value

A single numeric value representing the geometric mean of the processed
vector `x`, or `NA` if the resulting vector is empty (e.g., if
`na.rm = TRUE` removes all positive values) or if non-positive values
exist when `na.rm = FALSE`.

## Details

**Replacement Considerations**: The geometric mean is only defined for
strictly positive numbers (\\x \> 0\\). Despite this, the geometric mean
can be useful for laboratory measurements which can contain 0 or
negative values. If these values are treated as NA and are removed, this
results in an upward bias due to missingness. To reduce this, values
below the limit of detection (LOD) or limit of quantification (LOQ) are
often replaced with the chosen limit, making this limit the practical
lower limit of the measurement scale. This is therefore an often
recommended approach.

There are also alternatives approaches, where values are replaced by
either \\\frac{LOD}{2}\\ or \\\frac{LOD}{\sqrt{2}}\\ (or LOQ). These
approaches create a gap in the distribution of values (e.g. no values
for \\\frac{LOD}{2} \< x \< LOD\\) and should therefore be used with
caution.

**If the replacement approach for values below LOD or LOQ has a material
effect on the interpretation of the results, the values should be
treated as statistically censored. In this case, proper statistical
methods to handle (left) censored data should be used.**

When `replace_value` is provided, the function will *first* perform the
specified replacements, then proceed with the geometric mean
calculation. If no replacements are requested but zero or negative
values remain and `na.rm = FALSE`, an `NA` will be returned with a
warning.

## Examples

``` r
# Basic usage with no replacements:
x <- c(1, 2, 3, 4, 5)
geometric_mean(x)
#> [1] 2.605171

# Replace all values < 0.5 with 0.5 (common in LOD scenarios):
x3 <- c(0.1, 0.2, 0.4, 1, 5)
geometric_mean(x3, replace_value = 0.5, replace = "all")
#> Warning: 3 values were substituted with 0.5.
#> [1] 0.9102821

# Remove zero or negative values, since log(0) = -Inf and log(-1) = NaN
x4 <- c(-1, 0, 1, 2, 3)
geometric_mean(x4, na.rm = TRUE)
#> [1] 1.817121
```
