#' Aggregate data by time periods
#'
#' Aggregates data by specified time periods (e.g., weeks, months) and calculates (weighted)
#' counts. Incidence rates are also calculated using the provided population numbers.\cr\cr
#' This function is the core date binning engine
#' used by [geom_epicurve()] and [stat_bin_date()] for creating epidemiological
#' time series visualizations.
#'
#' @param x Either a data frame with a date column, or a date vector.\cr
#' Supported date formats are `date` and `datetime` and also commonly used character strings:
#'   * ISO dates `"2024-03-09"`
#'   * Month `"2024-03"`
#'   * Week `"2024-W09"` or `"2024-W09-1"`
#' @param dates_from Column name containing the dates to bin. Used when x is a data.frame.
#' @param n Numeric column with case counts (or weights). Supports quoted and unquoted column names.
#' @param population A number or a numeric column with the population size. Used to calculate the incidence.
#' @param fill_gaps Logical; If `TRUE`, gaps in the time series will be filled with 0 cases.
#' Useful for ensuring complete time series without missing periods. Defaults to `FALSE`.
#' @param date_resolution Character string specifying the time unit for date aggregation.
#'   Possible values include: `"hour"`, `"day"`, `"week"`, `"month"`, `"bimonth"`, `"season"`,
#'   `"quarter"`, `"halfyear"`, `"year"`. Special values:
#'   - `"isoweek"`: ISO week standard (week starts Monday, `week_start = 1`)
#'   - `"epiweek"`: US CDC epiweek standard (week starts Sunday, `week_start = 7`)
#'   - `"isoyear"`: ISO year (corresponding year of the ISO week, differs from year by 1-3 days)
#'   - `"epiyear"`: Epidemiological year (corresponding year of the epiweek, differs from year by 1-3 days)
#'   Defaults to `"week"`.
#' @param week_start Integer specifying the start of the week (1 = Monday, 7 = Sunday).
#'   Only used when `date_resolution` involves weeks. Defaults to 1 (Monday).
#'   Overridden by `"isoweek"` (1) and `"epiweek"` (7) settings.
#' @param .groups See [dplyr::summarise()].
#'
#' @return A data frame with the following columns:
#'   - A date column with the same name as `dates_from`, where values are binned to the start of the specified time period.
#'   - `n`: Count of observations (sum of weights) for each time period
#'   - `incidence`: Incidence rate calculated as `n / population` for each time period
#'   - Any existing grouping variables are preserved
#'
#' @details
#' The function performs several key operations:
#' 1. **Date coercion**: Converts the date column to proper Date format
#' 2. **Gap filling** (optional): Generates complete temporal sequences to fill missing time periods with zeros
#' 3. **Date binning**: Rounds dates to the specified resolution using [lubridate::floor_date()]
#' 4. **Weight and population handling**: Processes count weights and population denominators
#' 5. **Aggregation**: Groups by binned dates and sums weights to get counts and incidence
#'
#' **Grouping behaviour**:
#' The function respects existing grouping in the input data frame.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Create sample data
#' outbreak_data <- data.frame(
#'   onset_date = as.Date("2024-12-10") + sample(0:100, 50, replace = TRUE),
#'   cases = sample(1:5, 50, replace = TRUE)
#' )
#'
#' # Basic weekly binning
#' bin_by_date(outbreak_data, dates_from = onset_date)
#'
#' # Weekly binning with case weights
#' bin_by_date(outbreak_data, onset_date, n = cases)
#'
#' # Monthly binning
#' bin_by_date(outbreak_data, onset_date,
#'   date_resolution = "month"
#' )
#'
#' # ISO week binning (Monday start)
#' bin_by_date(outbreak_data, onset_date,
#'   date_resolution = "isoweek"
#' ) |>
#'   mutate(date_formatted = strftime(onset_date, "%G-W%V")) # Add correct date labels
#'
#' # US CDC epiweek binning (Sunday start)
#' bin_by_date(outbreak_data, onset_date,
#'   date_resolution = "epiweek"
#' )
#'
#' # With population data for incidence calculation
#' outbreak_data$population <- 10000
#' bin_by_date(outbreak_data, onset_date,
#'   n = cases,
#'   population = population
#' )
bin_by_date <- function(
    x, dates_from, n = 1, population = 1, fill_gaps = FALSE,
    date_resolution = "week", week_start = 1, .groups = "drop") {
  wt <- incidence <- NULL

  # Enframe if vector supplied
  if (!is.data.frame(x) & rlang::is_vector(x)) {
    # If vector, setup as data.frame
    x <- data.frame(date = x)
    dates_from <- rlang::sym("date")
  }

  # rlang check quo to detect character column names
  if (!rlang::quo_is_symbol(rlang::enquo(dates_from))) dates_from <- rlang::sym(dates_from)
  date_var <- rlang::as_name(rlang::enquo(dates_from))
  # Check if col exists
  tidyselect::eval_select(date_var, data = x)
  # Check grouping
  if (date_var %in% (dplyr::group_vars(x))) {
    cli::cli_warn(
      "Data.frame grouped by date column: { date_var }. Please remove variable from grouping."
    )
  }

  if (!rlang::quo_is_symbol(rlang::enquo(n))) {
    if (is.character(n)) n <- rlang::sym(n)
  }

  if (!rlang::quo_is_symbol(rlang::enquo(population))) {
    if (is.character(population)) population <- rlang::sym(population)
  }

  if (!is.na(date_resolution) & date_resolution == "isoweek") {
    date_resolution <- "week"
    week_start <- 1
  } # ISO
  if (!is.na(date_resolution) & date_resolution == "epiweek") {
    date_resolution <- "week"
    week_start <- 7
  } # US

  # Transform data, calc incidence
  x |>
    dplyr::mutate(
      {{ dates_from }} := .coerce_to_date({{ dates_from }}),
      # Set wt and pop
      wt = !!rlang::enquo(n),
      population = !!rlang::enquo(population),
      # Calc incidence
      incidence = wt / population
    ) -> x

  # Fill gaps in time series with 0
  if (fill_gaps) {
    # Generate complete date sequence
    df_full_dates <- x |>
      dplyr::select({{ dates_from }}) |>
      .fill_date_gaps(date_resolution = date_resolution)
    
    # Join with data and create observations with weight 0
    suppressMessages(x <- x |>
      dplyr::full_join(df_full_dates) |>
      tidyr::replace_na(list(wt = 0, incidence = 0)))
  }


  if (!is.na(date_resolution) & date_resolution %in% c("isoyear", "epiyear")) {
    if (date_resolution == "isoyear") year_func <- lubridate::isoyear
    if (date_resolution == "epiyear") year_func <- lubridate::epiyear

    x |>
      dplyr::mutate(
        {{ dates_from }} := year_func({{ dates_from }})
      ) -> x
  } else {
    # Floor Date
    x |>
      dplyr::mutate(
        {{ dates_from }} := lubridate::floor_date({{ dates_from }}, unit = date_resolution, week_start = week_start)
      ) -> x
  }

  # Count
  x |>
    dplyr::group_by({{ dates_from }}, .add = TRUE) |>
    dplyr::summarise(
      n = sum(wt, na.rm = TRUE),
      incidence = sum(incidence, na.rm = TRUE),
      .groups = .groups
    )
}


# Internal utility function for filling date gaps
.fill_date_gaps <- function(dates, date_resolution = "week") {

  suppressMessages(group_cols <- dplyr::group_keys(dates))
 
  # Filter out rows with NA dates
  clean_dates <- dates |>
    dplyr::ungroup() |>
    dplyr::select(-colnames(group_cols)) |>
    tidyr::drop_na()

  if (nrow(clean_dates) == 0) return(dates)
  
  date_col_name <- colnames(clean_dates)[1]
  date_vector <- clean_dates[[1]]
  
  # Convert lubridate units to seq() compatible units - simple mapping
  seq_by <- dplyr::case_when(
    grepl("sec", date_resolution) & inherits(date_vector, c("POSIXct", "POSIXt")) ~ "sec",
    grepl("min", date_resolution) & inherits(date_vector, c("POSIXct", "POSIXt")) ~ "min",
    grepl("hour", date_resolution) & inherits(date_vector, c("POSIXct", "POSIXt")) ~ "hour",
    grepl("day", date_resolution) ~ "day",
    grepl("week", date_resolution) ~ "week",
    grepl("month", date_resolution) ~ "month",
    date_resolution == "bimonth" ~ "month",    # Map to base unit
    date_resolution == "quarter" ~ "month",    # Map to base unit
    date_resolution == "season" ~ "month",     # Map to base unit
    date_resolution == "halfyear" ~ "month",   # Map to base unit
    grepl("year", date_resolution) ~ "year",
    TRUE ~ "day"  # default
  )
  
  # Get date range
  min_date <- min(date_vector, na.rm = TRUE)
  max_date <- max(date_vector, na.rm = TRUE)
  
  # Generate complete sequence based on date type
  # TODO: Is this needed?
  if (inherits(date_vector, "Date")) {
    full_sequence <- seq.Date(from = min_date, to = max_date, by = seq_by)
  } else if (inherits(date_vector, c("POSIXct", "POSIXt"))) {
    full_sequence <- seq.POSIXt(from = min_date, to = max_date, by = seq_by)
  } else {
    cli::cli_abort("Unsupported date type for gap filling.")
  }

  # Create result data frame
  result <- group_cols |> dplyr::group_by_all() |> dplyr::reframe(!!date_col_name := full_sequence)
  
  return(result)
}