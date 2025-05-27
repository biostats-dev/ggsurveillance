#' Align dates for seasonal comparison
#'
#' @description
#' Standardizes dates from multiple years to enable comparison of epidemic curves
#' and visualization of seasonal patterns in infectious disease surveillance data.
#' Commonly used for creating periodicity plots of respiratory diseases like
#' influenza, RSV, or COVID-19.
#'
#' @details
#' This function helps create standardized epidemic curves by aligning surveillance
#' data from different years. This enables:
#' * Comparison of disease patterns across multiple seasons
#' * Identification of typical seasonal trends
#' * Detection of unusual disease activity
#' * Assessment of current season against historical patterns
#'
#' The alignment can be done at different temporal resolutions (daily, weekly,
#' monthly) with customizable season start points to match different disease
#' patterns or surveillance protocols.
#'
#' @param x Either a data frame with a date column, or a date vector.\cr
#' Supported date formats are `date` and `datetime` and also commonly used character strings:
#'   * ISO dates `"2024-03-09"`
#'   * Month `"2024-03"`
#'   * Week `"2024-W09"` or `"2024-W09-1"`
#' @param n Numeric column with case counts (or weights). Supports quoted and unquoted column names.
#' @param dates_from Column name containing the dates to align. Used when x is a data.frame.
#' @param date_resolution Character string specifying the temporal resolution.
#'   One of:
#'   * `"week"` or `"isoweek"` - Calendar weeks (ISO 8601), reporting weeks as used by the ECDC.
#'   * `"epiweek"` - Epidemiological weeks (US CDC), i.e. ISO weeks with Sunday as week start.
#'   * `"month"` - Calendar months
#'   * `"day"` - Daily resolution
#' @param start Numeric value indicating epidemic season start:
#'   * For `week/epiweek`: week number (default: 28, approximately July)
#'   * For `month`: month number (default: 7 for July)
#'   * For `day`: day of year (default: 150, approximately June)
#' If start is set to "1" the alignment is done for yearly comparison and the shift in dates for seasonality is skipped.
#' @param target_year Numeric value for the reference year to align dates to. The default target year
#'  is the start of the most recent season in the data. This way the most recent dates stay unchanged.
#' @param population A number or a numeric column with the population size. Used to calculate the incidence.
#' @param fill_gaps Logical; If `TRUE`, gaps in the time series will be filled with 0 cases.
#'
#' @param drop_leap_week If `TRUE` and date_resolution is `week`, `isoweek` or `epiweek`, leap weeks (week 53)
#' are dropped if they are not in the most recent season. Disable if data should be returned.
#' Dropping week 53 from historical data is the most common approach. Otherwise historical data for week 53 would
#' map to week 52 if the target season has no leap week, resulting in a doubling of the case counts.
#'
#' @param .groups See [dplyr::summarise()].
#'
#' @return A data frame with standardized date columns:
#'   * `year`: Calendar year from original date
#'   * `week/month/day`: Time unit based on chosen resolution
#'   * `date_aligned`: Date standardized to target year
#'   * `season`: Epidemic season identifier (e.g., "2023/24"), if `start = 1` this is the year only (e.g. 2023).
#'   * `current_season`: Logical flag for most recent season
#'
#' Binning also creates the columns:
#'   * `n`: Sum of cases in bin
#'   * `incidence`: Incidence calculated using n/population
#'
#' @examples
#' # Sesonal Visualization of Germany Influenza Surveillance Data
#' library(ggplot2)
#'
#' influenza_germany |>
#'   align_dates_seasonal(
#'     dates_from = ReportingWeek, date_resolution = "epiweek", start = 28
#'   ) -> df_flu_aligned
#'
#' ggplot(df_flu_aligned, aes(x = date_aligned, y = Incidence, color = season)) +
#'   geom_line() +
#'   facet_wrap(~AgeGroup) +
#'   theme_bw() +
#'   theme_mod_rotate_x_axis_labels_45()
#'
#' @export
align_dates_seasonal <- function(
    x, dates_from = NULL, date_resolution = c("week", "isoweek", "epiweek", "day", "month"),
    start = NULL, target_year = NULL, drop_leap_week = TRUE) {
  date_resolution <- rlang::arg_match(date_resolution)

  # Enframe if vector supplied
  if (!is.data.frame(x) & rlang::is_vector(x)) {
    # If vector, setup as data.frame
    x <- data.frame(date = x)
    dates_from <- rlang::sym("date")
  }

  # rlang check quo to detect character column names
  if (!rlang::quo_is_symbol(rlang::enquo(dates_from))) dates_from <- rlang::sym(dates_from)
  tidyselect::eval_select(rlang::as_name(rlang::enquo(dates_from)), data = x)

  df <- x |>
    dplyr::mutate(
      {{ dates_from }} := .coerce_to_date({{ dates_from }})
    )

  last_date <- df |>
    dplyr::pull({{ dates_from }}) |>
    max(na.rm = TRUE)

  if (date_resolution %in% c("week", "isoweek")) {
    start <- start %||% 28
    # Default target year is chosen to keep most recent dates unchanged.
    target_year <- target_year %||% (
      lubridate::isoyear(last_date) - ifelse(lubridate::isoweek(last_date) < start, 1, 0))
    return(.align_dates_seasonal_week(
      df = df, dates_from = {{ dates_from }},
      start = start, target_year = target_year, drop_leap_week = drop_leap_week
    ))
  }

  if (date_resolution == "epiweek") {
    start <- start %||% 28
    # Default target year is chosen to keep most recent dates unchanged.
    target_year <- target_year %||% (
      lubridate::epiyear(last_date) - ifelse(lubridate::epiweek(last_date) < start, 1, 0))
    return(.align_dates_seasonal_epiweek(
      df = df, dates_from = {{ dates_from }},
      start = start %||% 28, target_year = target_year, drop_leap_week = drop_leap_week
    ))
  }

  if (date_resolution == "month") {
    start <- start %||% 7
    # Default target year is chosen to keep most recent dates unchanged.
    target_year <- target_year %||% (
      lubridate::year(last_date) - ifelse(lubridate::month(last_date) < start, 1, 0))
    return(.align_dates_seasonal_month(
      df = df, dates_from = {{ dates_from }},
      start = start %||% 7, target_year = target_year
    ))
  }

  if (date_resolution == "day") {
    start <- start %||% 150
    # Default target year is chosen to keep most recent dates unchanged.
    target_year <- target_year %||% (
      lubridate::year(last_date) - ifelse(lubridate::yday(last_date) < start, 1, 0))
    return(.align_dates_seasonal_day(
      df = df, dates_from = {{ dates_from }},
      start = start %||% 150, target_year = target_year
    ))
  }
}

#' @rdname align_dates_seasonal
#' @import dplyr
#' @import lubridate
#' @import ISOweek
#' @import rlang
#' @importFrom tidyselect eval_select
#' @importFrom stringr str_pad
#' @importFrom utils tail
#' @export

align_and_bin_dates_seasonal <- function(
    x, dates_from, n = 1, population = 1, fill_gaps = FALSE,
    date_resolution = c("week", "isoweek", "epiweek", "day", "month"),
    start = NULL, target_year = NULL, drop_leap_week = TRUE, .groups = "drop") {
  x |>
    bin_dates(
      dates_from = {{ dates_from }}, n = {{ n }}, population = {{ population }},
      date_resolution = date_resolution, fill_gaps = fill_gaps, .groups = .groups
    ) |>
    align_dates_seasonal(
      dates_from = {{ dates_from }}, date_resolution = date_resolution,
      start = start, target_year = target_year, drop_leap_week = drop_leap_week
    )
}

.is_current_season <- function(season) (season == (sort(season) |> utils::tail(n = 1)))

.align_dates_seasonal_week <- function(df, dates_from, start = 28, target_year = 2024, drop_leap_week = TRUE) {
  # Make R CMD Check happy, prevent global var
  season <- current_season <- NULL

  df |>
    dplyr::mutate(
      year = lubridate::isoyear({{ dates_from }}),
      week = lubridate::isoweek({{ dates_from }}) |>
        stringr::str_pad(width = 2, side = "left", pad = "0"),
      year_week = paste0(year, "-W", week),
      # binned_date = ISOweek::ISOweek2date(paste(year_week, "-1))),
      date_aligned = paste0(ifelse(as.numeric(week) >= start, target_year, target_year + 1), "-W", week, "-1") |>
        ISOweek::ISOweek2date(),
      season = case_when(
        # Only full years, if start = 1
        start <= 1 ~ paste0(year),
        # End of year
        as.numeric(week) >= start ~ paste0(year, "/", (year + 1) %% 100),
        # Beginning of following year
        T ~ paste0(year - 1, "/", year %% 100)
      ),
      # use arrange for current/last season
      current_season = .is_current_season(season),
      .after = {{ dates_from }}
    ) |>
    dplyr::filter(!(drop_leap_week & week == 53 & !current_season))
}

.align_dates_seasonal_epiweek <- function(df, dates_from, start = 28, target_year = 2024, drop_leap_week = TRUE) {
  # Make R CMD Check happy, prevent global var
  season <- current_season <- NULL

  df |>
    dplyr::mutate(
      year = lubridate::epiyear({{ dates_from }}),
      epiweek = lubridate::epiweek({{ dates_from }}) |>
        stringr::str_pad(width = 2, side = "left", pad = "0"),
      # binned_date = ISOweek::ISOweek2date(paste(year_week, "-1))),
      year_week = paste0(year, "-W", epiweek),
      date_aligned = paste0(ifelse(as.numeric(epiweek) >= start, target_year, target_year + 1), "-W", epiweek, "-1") |>
        ISOweek::ISOweek2date(),
      season = case_when(
        # Only full years, if start = 1
        start <= 1 ~ paste0(year),
        # End of year
        as.numeric(epiweek) >= start ~ paste0(year, "/", (year + 1) %% 100),
        # Beginning of following year
        T ~ paste0(year - 1, "/", year %% 100)
      ),
      # use arrange for current/last season
      current_season = .is_current_season(season),
      .after = {{ dates_from }}
    ) |>
    dplyr::filter(!(drop_leap_week & epiweek == 53 & !current_season))
}


.align_dates_seasonal_month <- function(df, dates_from, start = 28, target_year = 2024) {
  # Make R CMD Check happy, prevent global var
  season <- NULL

  df |>
    dplyr::mutate(
      year = lubridate::year({{ dates_from }}),
      month = lubridate::month({{ dates_from }}) |>
        stringr::str_pad(width = 2, side = "left", pad = "0"),
      year_month = paste0(year, "-", month),
      # binned_date = lubridate::as_date()(paste(year_month, "-1))),
      date_aligned = paste0(ifelse(as.numeric(month) >= start, target_year, target_year + 1), "-", month, "-1") |>
        lubridate::as_date(),
      # if start <= 1
      season = case_when(
        # Only full years, if start = 1
        start <= 1 ~ paste0(year),
        # End of year
        as.numeric(month) >= start ~ paste0(year, "/", (year + 1) %% 100),
        # Beginning of following year
        T ~ paste0(year - 1, "/", year %% 100)
      ),
      # use arrange for current/last season
      current_season = .is_current_season(season),
      .after = {{ dates_from }}
    )
}

.align_dates_seasonal_day <- function(df, dates_from, start = 150, target_year = 2024) {
  # Make R CMD Check happy, prevent global var
  dayofyear <- season <- NULL

  df |>
    dplyr::mutate(
      year = lubridate::year({{ dates_from }}),
      # Replace with better logic -> leap year
      dayofyear = lubridate::yday({{ dates_from }}),
      date_aligned = {{ dates_from }} %m+% lubridate::years((target_year - year)),
      # if start <= 1
      season = case_when(
        # Only full years, if start = 1
        start <= 1 ~ paste0(year),
        # End of year
        as.numeric(dayofyear) >= start ~ paste0(year, "/", (year + 1) %% 100),
        # Beginning of following year
        T ~ paste0(year - 1, "/", year %% 100)
      ),
      # use arrange for current/last season
      current_season = .is_current_season(season),
      .after = {{ dates_from }}
    )
}

.coerce_to_date <- function(dates) {
  date_iso_pattern <- "^\\d{4}-\\d{2}-\\d{2}$"
  year_month_pattern <- "^\\d{4}-\\d{2}$"
  year_week_day_pattern <- "^\\d{4}-W\\d{2}-\\d{1}$"
  year_week_pattern <- "^\\d{4}-W\\d{2}$"

  if (lubridate::is.Date(dates)) {
    return(dates)
  } else if (all(lubridate::is.POSIXct(dates), na.rm = TRUE)) {
    return(lubridate::as_datetime(dates))
  } else if (is.character(dates) && all(stringr::str_detect(dates, date_iso_pattern), na.rm = TRUE)) {
    return(lubridate::as_date(dates))
  } else if (is.character(dates) && all(stringr::str_detect(dates, year_month_pattern), na.rm = TRUE)) {
    return(lubridate::ym(dates))
  } else if (is.character(dates) && all(stringr::str_detect(dates, year_week_day_pattern), na.rm = TRUE)) {
    return(ISOweek::ISOweek2date(dates))
  } else if (is.character(dates) && all(stringr::str_detect(dates, year_week_pattern), na.rm = TRUE)) {
    return(ISOweek::ISOweek2date(paste0(dates, "-1")))
  } else {
    cli::cli_abort("Not a valid date column.")
  }
}
