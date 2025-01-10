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
#' @param x A data frame with a date column or a date vector
#' @param dates_from Column name containing the dates to align. Used when x is a data.frame.
#'   supported date formats are date and datetime and also commonly used character strings:
#'   * ISO dates "2024-03-09"
#'   * Month "2024-03"
#'   * Week "2024-W09" or "2024-W09-1"
#' @param date.resolution Character string specifying the temporal resolution.
#'   One of:
#'   * "week" or "isoweek" - Calendar weeks (ISO), reporting weeks according to the ECDC.
#'   * "epiweek" - Epidemiological weeks (CDC), i.e. ISO weeks with Sunday as week start.
#'   * "month" - Calendar months
#'   * "day" - Daily resolution
#' @param start Numeric value indicating epidemic season start:
#'   * For week/epiweek: week number (default: 28, approximately July)
#'   * For month: month number (default: 7 for July)
#'   * For day: day of year (default: 150, approximately June)
#' @param target_year Numeric value for the reference year to align dates to (default: 2024).
#'
#' @return A data frame with standardized date columns:
#'   * year: Calendar year from original date
#'   * week/month/day: Time unit based on chosen resolution
#'   * date_aligned: Date standardized to target year
#'   * season: Epidemic season identifier (e.g., "2023/24")
#'   * current_season: Logical flag for most recent season
#'
#' @examples
#' # Sesonal Visualization of Germany Influenza Surveillance Data
#' library(ggplot2)
#'
#' influenza_germany |>
#'   align_dates_seasonal(
#'     dates_from = ReportingWeek, date.resolution = "epiweek", start = 28
#'   ) -> df_flu_aligned
#'
#' ggplot(df_flu_aligned, aes(x = date_aligned, y = Incidence, color = season)) +
#'   geom_line() +
#'   facet_wrap(~AgeGroup) +
#'   theme_bw()
#'
#' @export

align_dates_seasonal <- function(x, dates_from, date.resolution = "week", start = NULL, target_year = 2024) {
  # Enframe if vector supplied
  if (!is.data.frame(x) & rlang::is_vector(x)) {
    # TODO: try as_date()
    x <- data.frame(date = x)
    dates_from <- rlang::sym("date")
  }

  df <- x |>
    mutate(
      {{ dates_from }} := .coerce_to_date({{ dates_from }})
    )

  # If not df error with typeof x?

  # Check for valid date resolution
  if (!date.resolution %in% c("week", "epiweek", "month", "day")) {
    stop("Invalid date resolution. Choose from 'week', 'isoweek', 'epiweek', 'month', or 'day'.")
  }

  .check_align_df(df, {{ dates_from }})

  if (date.resolution %in% c("week", "isoweek")) {
    return(.align_dates_seasonal_week(
      df = df, dates_from = {{ dates_from }},
      start = start %||% 28, target_year = target_year
    ))
  }

  if (date.resolution == "epiweek") {
    return(.align_dates_seasonal_epiweek(
      df = df, dates_from = {{ dates_from }},
      start = start %||% 28, target_year = target_year
    ))
  }

  if (date.resolution == "month") {
    return(.align_dates_seasonal_month(
      df = df, dates_from = {{ dates_from }},
      start = start %||% 7, target_year = target_year
    ))
  }

  if (date.resolution == "day") {
    return(.align_dates_seasonal_day(
      df = df, dates_from = {{ dates_from }},
      start = start %||% 150, target_year = target_year
    ))
  }
}

#' @import dplyr
#' @import lubridate
#' @import ISOweek
#' @import rlang
#' @importFrom tidyselect eval_select
#' @importFrom stringr str_pad
#' @importFrom utils tail

.is_current_season <- function(season) (season == (sort(season) |> utils::tail(n = 1)))

.check_align_df <- function(df, dates_from) {
  dates_from <- rlang::as_name(rlang::enquo(dates_from))

  tidyselect::eval_select(dates_from, data = df)
  if (dates_from %in% (df |> dplyr::group_vars())) {
    cli::cli_alert_warning(
      "Data.frame grouped by date column: { dates_from }. Please remove variable from grouping."
    )
  }
}

.align_dates_seasonal_week <- function(df, dates_from, start = 28, target_year = 2024) {
  # Make R CMD Check happy, prevent global var
  season <- NULL

  df |>
    dplyr::mutate(
      year = lubridate::isoyear({{ dates_from }}),
      week = lubridate::isoweek({{ dates_from }}) |>
        stringr::str_pad(width = 2, side = "left", pad = "0"),
      year_week = paste0(year, "-W", week),
      date_aligned = paste0(ifelse(as.numeric(week) >= start, target_year, target_year + 1), "-W", week, "-1") |>
        ISOweek::ISOweek2date(),
      # if start <= 1
      season = ifelse(as.numeric(week) >= start,
        # End of year
        paste0(year, "/", (year + 1) %% 100),
        # Beginning of following year
        paste0(year - 1, "/", year %% 100)
      ),
      # use arrange for current/last season
      current_season = .is_current_season(season),
      .after = {{ dates_from }}
    )
}

.align_dates_seasonal_epiweek <- function(df, dates_from, start = 28, target_year = 2024) {
  # Make R CMD Check happy, prevent global var
  season <- NULL

  df |>
    dplyr::mutate(
      year = lubridate::epiyear({{ dates_from }}),
      epiweek = lubridate::epiweek({{ dates_from }}) |>
        stringr::str_pad(width = 2, side = "left", pad = "0"),
      year_week = paste0(year, "-W", epiweek),
      date_aligned = paste0(ifelse(as.numeric(epiweek) >= start, target_year, target_year + 1), "-W", epiweek, "-2") |>
        ISOweek::ISOweek2date(),
      # if start <= 1
      season = ifelse(as.numeric(epiweek) >= start,
        # End of year
        paste0(year, "/", (year + 1) %% 100),
        # Beginning of following year
        paste0(year - 1, "/", year %% 100)
      ),
      # use arrange for current/last season
      current_season = .is_current_season(season),
      .after = {{ dates_from }}
    )
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
      date_aligned = paste0(ifelse(as.numeric(month) >= start, target_year, target_year + 1), "-", month, "-1") |>
        lubridate::as_date(),
      # if start <= 1
      season = ifelse(as.numeric(month) >= start,
        # End of year
        paste0(year, "/", (year + 1) %% 100),
        # Beginning of following year
        paste0(year - 1, "/", year %% 100)
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
      season = ifelse(as.numeric(dayofyear) >= start,
        # End of year
        paste0(year, "/", (year + 1) %% 100),
        # Beginning of following year
        paste0(year - 1, "/", year %% 100)
      ),
      # use arrange for current/last season
      current_season = .is_current_season(season),
      .after = {{ dates_from }}
    )
}


.coerce_to_date <- function(dates_from) {
  date_iso_pattern <- "^\\d{4}-\\d{2}-\\d{2}$"
  year_month_pattern <- "^\\d{4}-\\d{2}$"
  year_week_day_pattern <- "^\\d{4}-W\\d{2}-\\d{1}$"
  year_week_pattern <- "^\\d{4}-W\\d{2}$"


  if (lubridate::is.Date(dates_from)) {
    return(dates_from)
  } else if (all(lubridate::is.POSIXct(dates_from), na.rm = TRUE)) {
    return(lubridate::as_date(dates_from))
  } else if (is.character(dates_from) && all(stringr::str_detect(dates_from, date_iso_pattern), na.rm = TRUE)) {
    return(lubridate::as_date(dates_from))
  } else if (is.character(dates_from) && all(stringr::str_detect(dates_from, year_month_pattern), na.rm = TRUE)) {
    return(lubridate::ym(dates_from))
  } else if (is.character(dates_from) && all(stringr::str_detect(dates_from, year_week_day_pattern), na.rm = TRUE)) {
    return(ISOweek::ISOweek2date(dates_from))
  } else if (is.character(dates_from) && all(stringr::str_detect(dates_from, year_week_pattern), na.rm = TRUE)) {
    return(ISOweek::ISOweek2date(paste0(dates_from, "-1")))
  } else {
    cli::cli_abort("Not a valid date column.")
  }
}



# dplyr::group_by(dplyr::pick(PANEL:just)) |>


# Given the specific use case for infectious disease surveillance, here are some suggested function names that might better reflect its purpose:
# align_surveillance_seasons - Emphasizes the surveillance aspect and seasonal nature
# create_disease_seasons - Focuses on the disease surveillance context
# make_epi_seasons - Uses "epi" to indicate epidemiological context
# align_flu_seasons - If it's specifically for influenza (though this might be too specific)
# standardize_surveillance_dates - Emphasizes the date standardization aspect
# 6. create_surveillance_timeline - Focuses on the timeline creation aspect
# My top recommendation would be align_surveillance_seasons because it:
