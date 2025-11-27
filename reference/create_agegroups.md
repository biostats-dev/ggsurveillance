# Create Age Groups from Numeric Values

Creates age groups from numeric values using customizable break points
and formatting options. The function allows for flexible formatting and
customization of age group labels.

If a factor is returned, this factor includes factor levels of
unobserved age groups. This allows for reproducible age groups, which
can be used for joining data (e.g. adding age grouped population numbers
for incidence calculation).

## Usage

``` r
create_agegroups(
  values,
  age_breaks = c(5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90),
  breaks_as_lower_bound = TRUE,
  first_group_format = "0-{x}",
  interval_format = "{x}-{y}",
  last_group_format = "{x}+",
  pad_numbers = FALSE,
  pad_with = "0",
  collapse_single_year_groups = FALSE,
  na_label = NA,
  return_factor = FALSE
)
```

## Arguments

- values:

  Numeric vector of ages to be grouped

- age_breaks:

  Numeric vector of break points for age groups.  
  Default: `c(5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90)`

- breaks_as_lower_bound:

  Logical; if `TRUE` (default), breaks define the the lower bounds of
  the intervals (e.g., a break at 5 starts the '5-9' group). If `FALSE`,
  breaks define the upper bound (e.g., a break at 5 ends the '0-5'
  group).

- first_group_format:

  Character string template for the first age group. Uses
  [glue::glue](https://glue.tidyverse.org/reference/glue.html) syntax.  
  The variable `x` represents the upper bound of the first interval.
  Ignored if the first group is collapsed using
  `collapse_single_year_groups`.  
  Default: `"0-{x}"`. Other common styles: `"<={x}", "<{x+1}"`

- interval_format:

  Character string template for intermediate age groups. Uses
  [glue::glue](https://glue.tidyverse.org/reference/glue.html) syntax.  
  The variables `x` and `y` represent the lower and upper bounds of the
  interval, respectively.  
  Default: `"{x}-{y}"`. Other common styles: `"{x} to {y}"`

- last_group_format:

  Character string template for the last age group. Uses
  [glue::glue](https://glue.tidyverse.org/reference/glue.html) syntax.  
  The variable `x` represents the lower bound of the last interval.  
  Default: `"{x}+"`. Other common styles: `">={x}",">{x-1}"`

- pad_numbers:

  Logical or numeric; if numeric, pad numbers up to the specified length
  (Tip: use `2`, `TRUE` will be treated as `2`). Not compatible with
  calculations within glue formats. Default: `FALSE`

- pad_with:

  Character to use for padding numbers. Default: `"0"`

- collapse_single_year_groups:

  Logical; if `TRUE`, groups spanning a single year (e.g., from
  `age_breaks = c(1, 2)`) are formatted as a single number (e.g., "1")
  instead of a range (e.g., "1-1"). Default: `FALSE`

- na_label:

  Label for `NA` values (including negative ages). If `NA`, keeps
  default `NA` handling. Default: `NA`

- return_factor:

  Logical; if `TRUE`, returns a factor, if `FALSE` returns character
  vector. Can be used to keep the ordering and all possible levels of
  the age groups. Default: `FALSE`

## Value

Vector of age group labels (character or factor depending on
return_factor)

## Details

Values below 0 are treated as `NA`.

## Examples

``` r
# Basic usage
create_agegroups(1:100)
#>   [1] "0-4"   "0-4"   "0-4"   "0-4"   "5-9"   "5-9"   "5-9"   "5-9"   "5-9"  
#>  [10] "10-14" "10-14" "10-14" "10-14" "10-14" "15-19" "15-19" "15-19" "15-19"
#>  [19] "15-19" "20-24" "20-24" "20-24" "20-24" "20-24" "25-29" "25-29" "25-29"
#>  [28] "25-29" "25-29" "30-39" "30-39" "30-39" "30-39" "30-39" "30-39" "30-39"
#>  [37] "30-39" "30-39" "30-39" "40-49" "40-49" "40-49" "40-49" "40-49" "40-49"
#>  [46] "40-49" "40-49" "40-49" "40-49" "50-59" "50-59" "50-59" "50-59" "50-59"
#>  [55] "50-59" "50-59" "50-59" "50-59" "50-59" "60-69" "60-69" "60-69" "60-69"
#>  [64] "60-69" "60-69" "60-69" "60-69" "60-69" "60-69" "70-79" "70-79" "70-79"
#>  [73] "70-79" "70-79" "70-79" "70-79" "70-79" "70-79" "70-79" "80-89" "80-89"
#>  [82] "80-89" "80-89" "80-89" "80-89" "80-89" "80-89" "80-89" "80-89" "90+"  
#>  [91] "90+"   "90+"   "90+"   "90+"   "90+"   "90+"   "90+"   "90+"   "90+"  
#> [100] "90+"  

# Custom formatting with upper bounds
create_agegroups(1:100,
  breaks_as_lower_bound = FALSE,
  interval_format = "{x} to {y}",
  first_group_format = "0 to {x}"
)
#>   [1] "0 to 5"   "0 to 5"   "0 to 5"   "0 to 5"   "0 to 5"   "6 to 10" 
#>   [7] "6 to 10"  "6 to 10"  "6 to 10"  "6 to 10"  "11 to 15" "11 to 15"
#>  [13] "11 to 15" "11 to 15" "11 to 15" "16 to 20" "16 to 20" "16 to 20"
#>  [19] "16 to 20" "16 to 20" "21 to 25" "21 to 25" "21 to 25" "21 to 25"
#>  [25] "21 to 25" "26 to 30" "26 to 30" "26 to 30" "26 to 30" "26 to 30"
#>  [31] "31 to 40" "31 to 40" "31 to 40" "31 to 40" "31 to 40" "31 to 40"
#>  [37] "31 to 40" "31 to 40" "31 to 40" "31 to 40" "41 to 50" "41 to 50"
#>  [43] "41 to 50" "41 to 50" "41 to 50" "41 to 50" "41 to 50" "41 to 50"
#>  [49] "41 to 50" "41 to 50" "51 to 60" "51 to 60" "51 to 60" "51 to 60"
#>  [55] "51 to 60" "51 to 60" "51 to 60" "51 to 60" "51 to 60" "51 to 60"
#>  [61] "61 to 70" "61 to 70" "61 to 70" "61 to 70" "61 to 70" "61 to 70"
#>  [67] "61 to 70" "61 to 70" "61 to 70" "61 to 70" "71 to 80" "71 to 80"
#>  [73] "71 to 80" "71 to 80" "71 to 80" "71 to 80" "71 to 80" "71 to 80"
#>  [79] "71 to 80" "71 to 80" "81 to 90" "81 to 90" "81 to 90" "81 to 90"
#>  [85] "81 to 90" "81 to 90" "81 to 90" "81 to 90" "81 to 90" "81 to 90"
#>  [91] "91+"      "91+"      "91+"      "91+"      "91+"      "91+"     
#>  [97] "91+"      "91+"      "91+"      "91+"     

# Ages 1 to 5 are kept as numbers by collapsing single year groups
create_agegroups(1:10,
  age_breaks = c(1, 2, 3, 4, 5, 10),
  collapse_single_year_groups = TRUE
)
#>  [1] "1"   "2"   "3"   "4"   "5-9" "5-9" "5-9" "5-9" "5-9" "10+"
```
