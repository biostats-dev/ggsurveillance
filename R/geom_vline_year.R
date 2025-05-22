#' Automatically create lines at the turn of every year
#'
#'  Determines turn of year dates based on the range of either the x or y axis of the ggplot.
#'  * `geom_vline_year()` draws vertical lines at the turn of each year
#'  * `geom_hline_year()` draws horizontal lines at the turn of each year
#'
#' @param mapping Mapping created using [ggplot2::aes()]. Can be used to add the lines to the legend.
#' E.g. `aes(linetype = 'End of Year')`. Cannot access data specified in [ggplot2::ggplot()].
#' Panels created by [ggplot2::facet_wrap()] or [ggplot2::facet_grid()] are available with `aes(linetype = PANEL)`.
#' @param position Position adjustment, either as a string, or the result of a call to
#'   a position adjustment function.
#' @param year_break String specifying the month and day ("MM-DD") or week ("W01") of the year break .
#' Defaults to: `"01-01"` for January 1.
#' "Week" and "MM-DD" are converted automatically based on a leap year (366 days) which starts on Monday.
#' @param break_type String specifying the type of break to use. Options are:
#'   * `"day"` (default): Line drawn based on the specified day for each visible year.
#'   * `"week"` or `"isoweek"`: Line drawn based on the Monday of the specified week for each visible year.
#'  (e.g., "W01" for new year or "W40" for start of influenza season)
#'   * `"epiweek"`: same as week, but the line is drawn one day earlier (Sunday).
#' @param just Numeric offset in days (justification). Shifts the lines from the year break date.
#' Defaults to `-0.5` for `day`, which shifts the line by half a day so it falls
#'  between December 31 and January 1 by default.
#' Defaults to `-3.5` (i.e. half a week) for `week`, `isoweek` and `epiweek`.
#' @param ... Other arguments passed to \code{\link[ggplot2]{layer}}. For example:
#'   * \code{colour} Colour of the line. Try: `colour = "grey50"`
#'   * \code{linetype} Linetype. Try: `linetype = "dashed"` or `linetype = "dotted"`
#'   * \code{linewidth} Width of the line.
#'   * \code{alpha}  Transparency of the line.
#'   used to set an aesthetic to a fixed value.
#' @param show.legend logical. Should this layer be included in the legends? `NA`, the default,
#'   includes if any aesthetics are mapped. `FALSE` never includes, and `TRUE` always includes.
#'
#' @return A ggplot2 layer that can be added to a plot.
#' @seealso [geom_epicurve()], [ggplot2::geom_vline()]
#' @examples
#' library(ggplot2)
#' set.seed(1)
#'
#' plot_data_epicurve_imp <- data.frame(
#'   date = rep(as.Date("2023-12-01") + ((0:300) * 1), times = rpois(301, 0.5))
#' )
#'
#' # Break type day
#' ggplot(plot_data_epicurve_imp, aes(x = date, weight = 2)) +
#'   geom_epicurve(date_resolution = "week") +
#'   geom_vline_year() +
#'   labs(title = "Epicurve Example") +
#'   scale_y_cases_5er() +
#'   scale_x_date(date_breaks = "4 weeks", date_labels = "W%V'%g") + # Correct ISOWeek labels week'year
#'   theme_bw()
#'
#' # Break type week
#' ggplot(plot_data_epicurve_imp, aes(x = date, weight = 2)) +
#'   geom_epicurve(date_resolution = "week") +
#'   geom_vline_year(break_type = "week") +
#'   labs(title = "Epicurve Example") +
#'   scale_y_cases_5er() +
#'   scale_x_date(date_breaks = "4 weeks", date_labels = "W%V'%g") + # Correct ISOWeek labels week'year
#'   theme_bw()
#'
#' @export
geom_vline_year <- function(mapping = NULL, position = "identity",
                            year_break = "01-01", break_type = c("day", "week", "isoweek", "epiweek"),
                            just = NULL,
                            ..., show.legend = NA) {
  ggplot2::layer(
    data = data.frame(x = 1),
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomVlineYear,
    position = position,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list2(
      na.rm = FALSE,
      flipped_aes = FALSE,
      year_break = year_break,
      break_type = break_type,
      just = just,
      ...
    )
  )
}

#' @rdname geom_vline_year
#' @export
geom_hline_year <- function(mapping = NULL, position = "identity",
                            year_break = "01-01", break_type = c("day", "week", "isoweek", "epiweek"),
                            just = NULL,
                            ..., show.legend = NA) {
  ggplot2::layer(
    data = data.frame(y = 1),
    mapping = NULL,
    stat = StatIdentity,
    geom = GeomVlineYear,
    position = position,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list2(
      na.rm = FALSE,
      flipped_aes = TRUE,
      year_break = year_break,
      break_type = break_type,
      just = just,
      ...
    )
  )
}

# TODO: VlineYear from Geom to Stat for plotly
# TODO: add flip_aes function for handling flipping (maybe stat needed for this)
# TODO: Shaded area for repeating seasons (Mosquito season)
GeomVlineYear <- ggplot2::ggproto("GeomVlineYear", Geom,
  extra_params = c(GeomSegment$extra_params, "flipped_aes", "year_break", "break_type", "just"),
  draw_panel = function(data, panel_params, coord, lineend = "butt",
                        flipped_aes, year_break, break_type, just, debug = FALSE) {
    # Setup vline (x) oder hline (y)
    # Check for CoordFlip since it flips some thing and not others
    if (xor(!flipped_aes, (inherits(coord, "CoordFlip")))) {
      sel_scale <- panel_params$x$scale
    } else {
      sel_scale <- panel_params$y$scale
    }

    break_type <- match.arg(break_type, choices = c("day", "week", "isoweek", "epiweek"))

    # isoweek and epiweek defaults
    if (break_type == "isoweek") { # ISO
      break_type <- "week"
    }
    if (break_type == "epiweek") { # US
      break_type <- "week"
      # Set default
      just <- just %||% -3.5
      # Move to one day earlier
      just <- just - 1
    }

    # Set just if NULL
    just <- just %||% case_when(
      break_type == "day" ~ -0.5,
      break_type == "week" ~ -3.5,
      T ~ 0
    )

    # Check scale class to detect date or datetime
    if (inherits(sel_scale, "ScaleContinuousDate")) {
      is_date <- TRUE
    } else if (inherits(sel_scale, "ScaleContinuousDatetime")) {
      is_date <- FALSE
      just <- just * (24 * 60 * 60) # Transform just from days to seconds
    } else {
      is_date <- TRUE
      cli::cli_warn("{sel_scale$aesthetics[1]}-axis is not date or datetime. Assuming date scale.")
    }

    # Get range of x and y scale
    ranges <- coord$backtransform_range(panel_params)

    # Setup Lines
    if (!flipped_aes) {
      year <- .calc_visible_years(ranges$x, is_date, year_break, break_type)

      data <- data |>
        dplyr::distinct_all() |>
        dplyr::cross_join(data.frame(x = year + just, xend = year + just)) |>
        dplyr::mutate(y = ranges$y[1], yend = ranges$y[2])
    } else {
      year <- .calc_visible_years(ranges$y, is_date, year_break, break_type)

      data <- data |>
        dplyr::distinct_all() |>
        dplyr::cross_join(data.frame(y = year + just, yend = year + just)) |>
        dplyr::mutate(x = ranges$x[1], xend = ranges$x[2])
    }

    # For test_that
    if (debug) {
      return(list(data = data, just = just, year = year))
    }

    # Draw Lines
    GeomSegment$draw_panel(data, panel_params, coord, lineend = lineend)
  },
  default_aes = GeomSegment$default_aes,
  draw_key = draw_key_vline,
  rename_size = TRUE,
  check_constant_aes = FALSE
)

# Get the dates of the day (year_break) for each year covered by the axis range
.calc_visible_years <- function(range, is_date = TRUE, year_break = "01-01",
                                break_type = c("day", "week"), week_start = 1) {
  break_type <- match.arg(break_type)

  if (is_date) {
    func_convert <- lubridate::as_date
  } else {
    func_convert <- lubridate::as_datetime
  }

  # Gets years covered by the axis
  year_range <- func_convert(range) |>
    lubridate::year()

  if (break_type == "day") {
    # Convert week to MM-DD
    if (stringr::str_detect(year_break, "^W([0-4][0-9]|5[0-3])$")) {
      year_break <- .calc_mm_dd_from_week(year_break)
    }

    if (!stringr::str_detect(year_break, "^(0[1-9]|1[0-2])\\-(0[1-9]|[12][0-9]|3[01])$")) {
      warning(cli::format_warning("`year_break` must be a valid date in MM-DD format (e.g., '04-15')."))
    }

    # Convert years back to date or datetime and then numeric for ggplot
    years <- year_range[1]:year_range[2] |>
      paste0("-", year_break) |>
      func_convert() |>
      as.numeric()
  } else if (break_type == "week") {
    # Convert MM-DD to week
    if (stringr::str_detect(year_break, "^(0[1-9]|1[0-2])\\-(0[1-9]|[12][0-9]|3[01])$")) {
      year_break <- .calc_week_from_mm_dd(year_break) |>
        stringr::str_pad(2, "left", "0") |>
        paste0("W", ... = _)
    }

    if (!stringr::str_detect(year_break, "^W([0-4][0-9]|5[0-3])$")) {
      abort(cli::format_warning("`year_break` must be a valid date in week format (e.g., 'W07')."))
    }

    # Convert years back using isoweek specification
    years <- year_range[1]:year_range[2] |>
      paste0("-", year_break, "-1") |>
      ISOweek::ISOweek2date() |>
      func_convert() |>
      as.numeric()
  }

  # Filter visible year breaks
  years <- years[between(years, range[1], range[2])]

  years
}

.calc_week_from_mm_dd <- function(mm_dd) {
  # Use 2024 since it is a leap year starting on Monday (2024-01-01).
  yday <- paste0("2024-", mm_dd) |> # "02-29"
    lubridate::as_date() |>
    lubridate::yday()

  return(ceiling(yday / 7))
}

.calc_mm_dd_from_week <- function(week) { # week as W01
  # Use 2024 since it is a leap year starting on Monday (2024-01-01).
  paste0("2024-", week, "-1") |>
    ISOweek::ISOweek2date() |>
    format(format = "%m-%d")
}
