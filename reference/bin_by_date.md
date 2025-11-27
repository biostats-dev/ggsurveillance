# Aggregate data by time periods

Aggregates data by specified time periods (e.g., weeks, months) and
calculates (weighted) counts. Incidence rates are also calculated using
the provided population numbers.  
  
This function is the core date binning engine used by
[`geom_epicurve()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md)
and
[`stat_bin_date()`](https://ggsurveillance.biostats.dev/reference/geom_epicurve.md)
for creating epidemiological time series visualizations.

## Usage

``` r
bin_by_date(
  x,
  dates_from,
  n = 1,
  population = 1,
  fill_gaps = FALSE,
  date_resolution = "week",
  week_start = 1,
  .groups = "drop"
)
```

## Arguments

- x:

  Either a data frame with a date column, or a date vector.  
  Supported date formats are `date` and `datetime` and also commonly
  used character strings:

  - ISO dates `"2024-03-09"`

  - Month `"2024-03"`

  - Week `"2024-W09"` or `"2024-W09-1"`

- dates_from:

  Column name containing the dates to bin. Used when x is a data.frame.

- n:

  Numeric column with case counts (or weights). Supports quoted and
  unquoted column names.

- population:

  A number or a numeric column with the population size. Used to
  calculate the incidence.

- fill_gaps:

  Logical; If `TRUE`, gaps in the time series will be filled with 0
  cases. Useful for ensuring complete time series without missing
  periods. Defaults to `FALSE`.

- date_resolution:

  Character string specifying the time unit for date aggregation.
  Possible values include: `"hour"`, `"day"`, `"week"`, `"month"`,
  `"bimonth"`, `"season"`, `"quarter"`, `"halfyear"`, `"year"`. Special
  values:

  - `"isoweek"`: ISO week standard (week starts Monday,
    `week_start = 1`)

  - `"epiweek"`: US CDC epiweek standard (week starts Sunday,
    `week_start = 7`)

  - `"isoyear"`: ISO year (corresponding year of the ISO week, differs
    from year by 1-3 days)

  - `"epiyear"`: Epidemiological year (corresponding year of the
    epiweek, differs from year by 1-3 days) Defaults to `"week"`.

- week_start:

  Integer specifying the start of the week (1 = Monday, 7 = Sunday).
  Only used when `date_resolution` involves weeks. Defaults to 1
  (Monday). Overridden by `"isoweek"` (1) and `"epiweek"` (7) settings.

- .groups:

  See
  [`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html).

## Value

A data frame with the following columns:

- A date column with the same name as `dates_from`, where values are
  binned to the start of the specified time period.

- `n`: Count of observations (sum of weights) for each time period

- `incidence`: Incidence rate calculated as `n / population` for each
  time period

- Any existing grouping variables are preserved

## Details

The function performs several key operations:

1.  **Date coercion**: Converts the date column to proper Date format

2.  **Gap filling** (optional): Generates complete temporal sequences to
    fill missing time periods with zeros

3.  **Date binning**: Rounds dates to the specified resolution using
    [`lubridate::floor_date()`](https://lubridate.tidyverse.org/reference/round_date.html)

4.  **Weight and population handling**: Processes count weights and
    population denominators

5.  **Aggregation**: Groups by binned dates and sums weights to get
    counts and incidence

**Grouping behaviour**: The function respects existing grouping in the
input data frame.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

# Create sample data
outbreak_data <- data.frame(
  onset_date = as.Date("2024-12-10") + sample(0:100, 50, replace = TRUE),
  cases = sample(1:5, 50, replace = TRUE)
)

# Basic weekly binning
bin_by_date(outbreak_data, dates_from = onset_date)
#> # A tibble: 15 × 3
#>    onset_date     n incidence
#>    <date>     <dbl>     <dbl>
#>  1 2024-12-09     5         5
#>  2 2024-12-16     3         3
#>  3 2024-12-23     1         1
#>  4 2024-12-30     4         4
#>  5 2025-01-06     7         7
#>  6 2025-01-13     1         1
#>  7 2025-01-20     6         6
#>  8 2025-01-27     4         4
#>  9 2025-02-03     4         4
#> 10 2025-02-10     4         4
#> 11 2025-02-17     4         4
#> 12 2025-02-24     2         2
#> 13 2025-03-03     3         3
#> 14 2025-03-10     1         1
#> 15 2025-03-17     1         1

# Weekly binning with case weights
bin_by_date(outbreak_data, onset_date, n = cases)
#> # A tibble: 15 × 3
#>    onset_date     n incidence
#>    <date>     <int>     <dbl>
#>  1 2024-12-09    14        14
#>  2 2024-12-16    15        15
#>  3 2024-12-23     5         5
#>  4 2024-12-30    12        12
#>  5 2025-01-06    13        13
#>  6 2025-01-13     2         2
#>  7 2025-01-20    17        17
#>  8 2025-01-27    12        12
#>  9 2025-02-03    10        10
#> 10 2025-02-10    15        15
#> 11 2025-02-17     7         7
#> 12 2025-02-24     5         5
#> 13 2025-03-03     7         7
#> 14 2025-03-10     1         1
#> 15 2025-03-17     2         2

# Monthly binning
bin_by_date(outbreak_data, onset_date,
  date_resolution = "month"
)
#> # A tibble: 4 × 3
#>   onset_date     n incidence
#>   <date>     <dbl>     <dbl>
#> 1 2024-12-01    11        11
#> 2 2025-01-01    19        19
#> 3 2025-02-01    15        15
#> 4 2025-03-01     5         5

# ISO week binning (Monday start)
bin_by_date(outbreak_data, onset_date,
  date_resolution = "isoweek"
) |>
  mutate(date_formatted = strftime(onset_date, "%G-W%V")) # Add correct date labels
#> # A tibble: 15 × 4
#>    onset_date     n incidence date_formatted
#>    <date>     <dbl>     <dbl> <chr>         
#>  1 2024-12-09     5         5 2024-W50      
#>  2 2024-12-16     3         3 2024-W51      
#>  3 2024-12-23     1         1 2024-W52      
#>  4 2024-12-30     4         4 2025-W01      
#>  5 2025-01-06     7         7 2025-W02      
#>  6 2025-01-13     1         1 2025-W03      
#>  7 2025-01-20     6         6 2025-W04      
#>  8 2025-01-27     4         4 2025-W05      
#>  9 2025-02-03     4         4 2025-W06      
#> 10 2025-02-10     4         4 2025-W07      
#> 11 2025-02-17     4         4 2025-W08      
#> 12 2025-02-24     2         2 2025-W09      
#> 13 2025-03-03     3         3 2025-W10      
#> 14 2025-03-10     1         1 2025-W11      
#> 15 2025-03-17     1         1 2025-W12      

# US CDC epiweek binning (Sunday start)
bin_by_date(outbreak_data, onset_date,
  date_resolution = "epiweek"
)
#> # A tibble: 15 × 3
#>    onset_date     n incidence
#>    <date>     <dbl>     <dbl>
#>  1 2024-12-08     5         5
#>  2 2024-12-15     3         3
#>  3 2024-12-22     1         1
#>  4 2024-12-29     4         4
#>  5 2025-01-05     3         3
#>  6 2025-01-12     5         5
#>  7 2025-01-19     6         6
#>  8 2025-01-26     3         3
#>  9 2025-02-02     3         3
#> 10 2025-02-09     5         5
#> 11 2025-02-16     5         5
#> 12 2025-02-23     2         2
#> 13 2025-03-02     3         3
#> 14 2025-03-09     1         1
#> 15 2025-03-16     1         1

# With population data for incidence calculation
outbreak_data$population <- 10000
bin_by_date(outbreak_data, onset_date,
  n = cases,
  population = population
)
#> # A tibble: 15 × 3
#>    onset_date     n incidence
#>    <date>     <int>     <dbl>
#>  1 2024-12-09    14    0.0014
#>  2 2024-12-16    15    0.0015
#>  3 2024-12-23     5    0.0005
#>  4 2024-12-30    12    0.0012
#>  5 2025-01-06    13    0.0013
#>  6 2025-01-13     2    0.0002
#>  7 2025-01-20    17    0.0017
#>  8 2025-01-27    12    0.0012
#>  9 2025-02-03    10    0.001 
#> 10 2025-02-10    15    0.0015
#> 11 2025-02-17     7    0.0007
#> 12 2025-02-24     5    0.0005
#> 13 2025-03-03     7    0.0007
#> 14 2025-03-10     1    0.0001
#> 15 2025-03-17     2    0.0002
```
