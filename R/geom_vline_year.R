#' Automatically create lines at the turn of every year
#'
#'  Determines turn of year dates based on the range of either the x or y axis of the ggplot.
#'  * `geom_vline_year()` draws vertical lines at the turn of each year
#'  * `geom_hline_year()` draws horizontal lines at the turn of each year
#'
#' @param mapping Mapping created using [ggplot2::aes()]. Can be used to add the lines to the legend.
#' E.g. `aes(linetype = 'End of Year')`. Cannot access data specified in [ggplot2::ggplot()].
#' Panels created by [ggplot2::facet_wrap()] [ggplot2::facet_grid()] are available with `aes(linetype = PANEL)`.
#' @param position Position adjustment, either as a string, or the result of a call to
#'   a position adjustment function.
#' @param year_break String specifying the month and day of the year break ("MM-DD").
#' Defaults to: `"01-01"` for January 1.
#' @param just Numeric offset in days (justification). Shifts the lines from the year break date.
#' Defaults to `-0.5`, which shifts the line by half a day so if falls in the middle between December 31 and January 1.
#' @param ... Other arguments passed to \code{\link[ggplot2]{layer}}. For example:
#'   * \code{colour} Colour of the line. Try: `colour = "grey50"`
#'   * \code{linetype} Linetype. Try: `linetype = "dashed"` or `linetype = "dotted"`
#'   * \code{linewidth} Width of the line.
#'   * \code{alpha}  Transparency of the line.
#'   used to set an aesthetic to a fixed value, like `colour = "grey25"` or `linetype = 2`.
#' @param show.legend logical. Should this layer be included in the legends? `NA`, the default,
#'   includes if any aesthetics are mapped. `FALSE` never includes, and `TRUE` always includes.
#'
#' @return A ggplot2 layer that can be added to a plot.
#' @examples
#' library(ggplot2)
#' set.seed(1)
#'
#' plot_data_epicurve_imp <- data.frame(
#'   date = rep(as.Date("2023-12-01") + ((0:300) * 1), times = rpois(301, 0.5))
#' )
#'
#' ggplot(plot_data_epicurve_imp, aes(x = date, weight = 2)) +
#'   geom_epicurve(date_resolution = "week") +
#'   geom_vline_year(year_break = "01-01", show.legend = TRUE) +
#'   labs(title = "Epicurve Example") +
#'   scale_y_cases_5er() +
#'   scale_x_date(date_breaks = "4 weeks", date_labels = "W%V'%g") + # Correct ISOWeek labels week'year
#'   theme_bw()
#'
#' @export
geom_vline_year <- function(mapping = NULL, # data = NULL,
                            position = "identity",
                            year_break = "01-01",
                            just = -0.5,
                            ...,
                            show.legend = NA) {
  layer(
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
      just = just,
      ...
    )
  )
}

#' @rdname geom_vline_year
#' @export
geom_hline_year <- function(mapping = NULL, # data = NULL,
                            position = "identity",
                            year_break = "01-01",
                            just = -0.5,
                            ...,
                            show.legend = NA) {
  layer(
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
      just = just,
      ...
    )
  )
}

GeomVlineYear <- ggproto("GeomVlineYear", Geom,
  extra_params = c(GeomSegment$extra_params, "flipped_aes", "year_break", "just"),
  draw_panel = function(data, panel_params, coord, lineend = "butt",
                        flipped_aes, year_break, just) {
    # Setup vline (x) oder hline (y)
    # Check for CoordFlip since it flips some thing and not others
    if (xor(!flipped_aes, (inherits(coord, "CoordFlip")))) {
      sel_scale <- panel_params$x$scale
      sel_axis <- "x-axis"
    } else {
      sel_scale <- panel_params$y$scale
      sel_axis <- "y-axis"
    }

    # Check scale class to detect date or datetime
    if (inherits(sel_scale, "ScaleContinuousDate")) {
      is_date <- TRUE
    } else if (inherits(sel_scale, "ScaleContinuousDatetime")) {
      is_date <- FALSE
      just <- just * (24 * 60 * 60) # Transform just from days to seconds
    } else {
      is_date <- TRUE
      cli::cli_warn("{sel_axis} is not date or datetime. Assuming date scale.")
    }

    # Get range of x and y scale
    ranges <- coord$backtransform_range(panel_params)

    # Setup Lines
    # TODO: Floor Weekstart
    if (!flipped_aes) {
      year <- calc_visible_years(ranges$x, is_date, year_break)

      data <- data |>
        dplyr::distinct_all() |>
        dplyr::cross_join(data.frame(x = year + just, xend = year + just)) |>
        dplyr::mutate(y = ranges$y[1], yend = ranges$y[2])
    } else {
      year <- calc_visible_years(ranges$y, is_date, year_break)

      data <- data |>
        dplyr::distinct_all() |>
        dplyr::cross_join(data.frame(y = year + just, yend = year + just)) |>
        dplyr::mutate(x = ranges$x[1], xend = ranges$x[2])
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
calc_visible_years <- function(range, is_date = TRUE, year_break = "01-01") {
  if (is_date) {
    func_convert <- lubridate::as_date
  } else {
    func_convert <- lubridate::as_datetime
  }

  # Gets years covered by the axis
  year_range <- func_convert(range) |>
    lubridate::year()

  # Convert years back to date or datetime and then numeric for ggplot
  years <- year_range[1]:year_range[2] |>
    paste0("-", year_break) |>
    func_convert() |>
    as.numeric()

  # Filter visible year breaks
  years <- years[between(years, range[1], range[2])]

  years
}
