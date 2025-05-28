bin_dates <- function(
    x, dates_from, n = 1, population = 1, fill_gaps = FALSE,
    date_resolution = "week", week_start = 1, .groups = "drop") {
  wt <- incidence <- NULL

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
    rlang::check_installed("tsibble", reason = "to fill the gaps in the time series.")
    # Save existing grouping of the df
    group_list <- dplyr::group_vars(x)
    # Create full time index for all groups
    # This has to be done before binning, to avoid wrong auto detection of resolution
    x |>
      select({{ dates_from }}) |> # TODO: add warning
      mutate_if(is.POSIXct, floor_date) |> # select + mutate are fixes for double precission problems in tsibble
      distinct({{ dates_from }}) |>
      tsibble::as_tsibble(index = {{ dates_from }}, key = all_of(group_list)) |>
      tsibble::fill_gaps(.full = TRUE) |>
      as.data.frame() -> df_full_dates
    # Join with data and create observations with weight 0
    # Suppress joining by message
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
