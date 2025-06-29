#' Create an epidemic curve plot or bin/count observations by date periods
#'
#' Creates a epicurve plot for visualizing epidemic case counts in outbreaks (epidemiological curves).
#' An epicurve is a bar plot, where every case is outlined. \code{geom_epicurve} additionally provides
#' date-based aggregation of cases (e.g. per week or month and many more).
#' - For week aggregation both isoweek (World + ECDC) and epiweek (US CDC) are supported.
#' - `stat_bin_date` and its alias `stat_date_count` provide date based binning only. After binning the by date, these
#' stats behave like [ggplot2::stat_count].
#' - `geom_epicurve_text` adds text labels to cases on epicurve plots.
#' - `geom_epicurve_point` adds points/shapes to cases on epicurve plots.
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}. Commonly used mappings:
#'   * **x or y**: date or datetime. Numeric is technically supported.
#'   * **fill**: for colouring groups
#'   * **weight**: if data is already aggregated (e.g. case counts)
#' @param data The data frame containing the variables for the plot
#' @param stat either "`epicurve`" for outlines around cases or "`bin_date`" for outlines around (fill) groups.
#' For large numbers of cases please use "`bin_date`" to reduce the number of drawn rectangles.
#' @param position Position adjustment. Currently supports "`stack`" for `geom_epicurve()`.
#' @param date_resolution Character string specifying the time unit for date aggregation.
#' Set to \code{NULL} or `NA` for no date aggregation \cr
#' Possible values are: `"day"`, `"week"`, `"month"`, `"bimonth"`, `"season"`, `"quarter"`, `"halfyear"`, `"year"`.
#' To special values enforce ISO or US week standard:
#'  - `isoweek` will force `date_resolution = week` and `week_start = 1` (ISO and ECDC Standard)
#'  - `epiweek` will force `date_resolution = week` and `week_start = 7` (US CDC Standard)
#' @param week_start Integer specifying the start of the week (1 = Monday, 7 = Sunday). \cr
#'        Only used when date_resolution includes weeks. Defaults to 1 (Monday). \cr
#'        For isoweek use \code{week_start = 1} and for epiweek use \code{week_start = 7}.
#' @param fill_gaps Logical; If `TRUE`, gaps in the time series will be filled with a count of 0.
#' @param width Numeric value specifying the width of the bars. If \code{NULL}, calculated
#'        based on resolution and relative.width
#' @param relative.width Numeric value between 0 and 1 adjusting the relative width
#'        of bars. Defaults to 1
#' @param vjust Vertical justification of the text or shape. Value between 0 and 1.
#'        Used by \code{geom_epicurve_text} and \code{geom_epicurve_point} to control
#'        vertical positioning within the case rectangles. Defaults to 0.5 (center).
#' @param geom  The geometric object to use to display the data for this layer.
#'   When using a `stat_*()` function to construct a layer, the `geom` argument
#'   can be used to override the default coupling between stats and geoms.
#' @param ... Other arguments passed to \code{\link[ggplot2]{layer}}. For example:
#'   * **colour**: Colour of the outlines around cases. Disable with colour = NA. Defaults to "white".
#'   * **linewidth**:  Width of the case outlines.
#'
#' For `geom_epicurve_text()` additional \code{\link[ggplot2]{geom_text}} arguments are supported:
#'   * **fontface**: Font face for text labels: one of "plain", "bold", "italic", "bold.italic".
#'   * **family**: The font family.
#'   * **size**: The font size.
#'
#' @inheritParams ggplot2::geom_bar
#'
#' @details
#' Epi Curves are a public health tool for outbreak investigation. For more details see the references.
#'
#' @return A `ggplot2` geom layer that can be added to a plot
#' @seealso [scale_y_cases_5er()], [geom_vline_year()]
#' @export
#'
#' @references
#' -  Centers for Disease Control and Prevention. Quick-Learn Lesson:
#'  Using an Epi Curve to Determine Mode of Spread. USA. \url{https://www.cdc.gov/training/quicklearns/epimode/}
#' -  Dicker, Richard C., Fátima Coronado, Denise Koo, and R. Gibson Parrish. 2006.
#'  Principles of Epidemiology in Public Health Practice; an Introduction to Applied Epidemiology and Biostatistics.
#'  3rd ed. USA. \url{https://stacks.cdc.gov/view/cdc/6914}
#'
#' @examples
#' # Basic epicurve with dates
#' library(ggplot2)
#' set.seed(1)
#'
#' plot_data_epicurve_imp <- data.frame(
#'   date = rep(as.Date("2023-12-01") + ((0:300) * 1), times = rpois(301, 0.5))
#' )
#'
#' ggplot(plot_data_epicurve_imp, aes(x = date, weight = 2)) +
#'   geom_vline_year(break_type = "week") +
#'   geom_epicurve(date_resolution = "week") +
#'   labs(title = "Epicurve Example") +
#'   scale_y_cases_5er() +
#'   scale_x_date(date_breaks = "4 weeks", date_labels = "W%V'%g") + # Correct ISOWeek labels week'year
#'   coord_equal(ratio = 7) + # Use coord_equal for square boxes. 'ratio' are the days per week.
#'   theme_bw()
#'
#' # Categorical epicurve
#' library(tidyr)
#' library(outbreaks)
#'
#' sars_canada_2003 |> # SARS dataset from outbreaks
#'   pivot_longer(starts_with("cases"), names_prefix = "cases_", names_to = "origin") |>
#'   ggplot(aes(x = date, weight = value, fill = origin)) +
#'   geom_epicurve(date_resolution = "week") +
#'   scale_x_date(date_labels = "W%V'%g", date_breaks = "2 weeks") +
#'   scale_y_cases_5er() +
#'   theme_classic()
geom_epicurve <- function(mapping = NULL, data = NULL,
                          stat = "epicurve", position = "stack",
                          date_resolution = NULL,
                          week_start = getOption("lubridate.week.start", 1),
                          width = NULL, relative.width = 1,
                          ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomEpicurve,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      relative.width = relative.width,
      date_resolution = date_resolution,
      week_start = week_start,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_epicurve
#' @export
stat_bin_date <- function(mapping = NULL, data = NULL,
                          geom = "line", position = "identity",
                          date_resolution = NULL,
                          week_start = getOption("lubridate.week.start", 1),
                          fill_gaps = FALSE,
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatBinDate,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      date_resolution = date_resolution,
      week_start = week_start,
      fill_gaps = fill_gaps,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_epicurve
#' @export
stat_date_count <- stat_bin_date

#' @import ggplot2
#' @import dplyr
#' @import rlang
#' @importFrom cli cli_abort
#' @rdname geom_epicurve
#' @format NULL
#' @usage NULL
#' @export

StatEpicurve <- ggplot2::ggproto("StatEpicurve", Stat,
  required_aes = "x|y",
  default_aes = aes(x = after_stat(count), y = after_stat(count), group = row_number, weight = 1),
  extra_params = c("na.rm", "date_resolution", "week_start"),
  setup_params = function(self, data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(data, params, main_is_orthogonal = FALSE)

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      cli::cli_abort("stat_epicurve requires an x or y aesthetic.")
    }
    if (has_x && has_y) {
      cli::cli_abort("stat_epicurve must only have an x or y aesthetic.")
    }

    params
  },
  compute_panel = function(self, data, scales, flipped_aes = FALSE,
                          date_resolution = NA, week_start = 1, na.rm = FALSE) {
    date_resolution <- date_resolution %||% NA
    week_start <- week_start %||% 1
    flipped_aes <- flipped_aes %||% any(data$flipped_aes) %||% FALSE

    if (!is.na(date_resolution) & date_resolution == "isoweek") {
      date_resolution <- "week"
      week_start <- 1
    } # ISO
    if (!is.na(date_resolution) & date_resolution == "epiweek") {
      date_resolution <- "week"
      week_start <- 7
    } # US

    data <- ggplot2::flip_data(data, flipped_aes)
    # Check for CoordFlip since it flips some thing and not others
    if (!flipped_aes) {
      sel_scale <- scales$x
    } else {
      sel_scale <- scales$y
    }

    # Check scale class to detect date or datetime
    if (inherits(sel_scale, c("ScaleContinuousDate", "ScaleContinuousDatetime"))) {
      trans <- sel_scale$trans # Use Transformation of the scale
    } else {
      trans <- scales::transform_date()
      cli::cli_warn("{sel_scale$aesthetics[1]}-axis is not date or datetime. Assuming date scale.")
    }

    # Drop missing x
    complete <- stats::complete.cases(data$x)
    data <- data |> dplyr::filter(complete)
    if (!all(complete) && !na.rm) {
      cli::cli_warn(paste0(
        "Removed {sum(!complete)} row{?s} containing missing values (geom_epicurve)."
      ))
    }

    if (!is.na(date_resolution)) {
      data$x <- trans$inverse(data$x) # Transform to date or datetime

      data$x_ll <- as.numeric(lubridate::floor_date(data$x,
        unit = date_resolution,
        week_start = week_start
      ))
      # Use ceiling to be able to infer resolution in days
      data$x_ul <- as.numeric(lubridate::ceiling_date(data$x,
        unit = date_resolution,
        week_start = week_start,
        change_on_boundary = TRUE
      ))
    } else {
      data$x_ll <- data$x
      data$x_ul <- data$x
    }

    if (is.na(date_resolution) & inherits(sel_scale, "ScaleContinuousDatetime")) {
      cli::cli_warn("It seems you provided a datetime format. Column used as specified.
                          Please use date_resolution = 'day' to round to day (stat_epicurve).")
    }

    weight <- data$weight %||% rep(1, length(data$x))
    data <- data |> expand_counts(weight)
browser()
    bars <- data |>
      dplyr::mutate(
        x = x_ll,
        x_ul = x_ul,
        width = if (!is.na(date_resolution)) x_ul - x_ll else NULL,
        row_number = dplyr::row_number(data),
        count = 1,
        flipped_aes = flipped_aes
      )
    ggplot2::flip_data(bars, flipped_aes)
  },
  dropped_aes = "weight"
)

#' @import ggplot2
#' @import dplyr
#' @import rlang
#' @importFrom cli cli_abort cli_warn
#' @rdname geom_epicurve
#' @format NULL
#' @usage NULL
#' @export

StatBinDate <- ggplot2::ggproto("StatBinDate", Stat,
  required_aes = "x|y",
  default_aes = aes(!!!StatCount$default_aes),
  extra_params = c("na.rm", "date_resolution", "week_start", "fill_gaps"),
  setup_params = function(self, data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(data, params, main_is_orthogonal = FALSE)

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      cli::cli_abort("stat_epicurve requires an x or y aesthetic.")
    }
    if (has_x && has_y) {
      cli::cli_abort("stat_epicurve must only have an x or y aesthetic.")
    }

    params
  },
  compute_group = function(self, data, scales, flipped_aes = FALSE,
                           date_resolution = NA, week_start = 1, fill_gaps = FALSE) {
    date_resolution <- date_resolution %||% NA
    week_start <- week_start %||% 1
    flipped_aes <- flipped_aes %||% any(data$flipped_aes) %||% FALSE

    if (is.na(date_resolution)) {
      cli::cli_warn("It seems you provided no date_resolution. Column used as specified.
                          Please use date_resolution = 'week' to round to week (stat_bin_date/date_count).")
    }

    if (!is.na(date_resolution) & date_resolution == "isoweek") {
      date_resolution <- "week"
      week_start <- 1
    } # ISO
    if (!is.na(date_resolution) & date_resolution == "epiweek") {
      date_resolution <- "week"
      week_start <- 7
    } # US

    data <- ggplot2::flip_data(data, flipped_aes)
    # Check for CoordFlip since it flips some thing and not others
    if (!flipped_aes) {
      sel_scale <- scales$x
    } else {
      sel_scale <- scales$y
    }

    # Check scale class to detect date or datetime
    if (inherits(sel_scale, c("ScaleContinuousDate", "ScaleContinuousDatetime"))) {
      trans <- sel_scale$trans # Use Transformation of the scale
    } else {
      trans <- scales::transform_date()
      cli::cli_warn("{sel_scale$aesthetics[1]}-axis is not date or datetime. Assuming date scale.")
    }
    # Drop missing x
    complete <- stats::complete.cases(data$x)
    data <- data |> dplyr::filter(complete)
    if (!all(complete) && !params$na.rm) {
      cli::cli_warn(paste0(
        "Removed {sum(!complete)} row{?s} containing missing values (geom_epicurve)."
      ))
    }

    data$weight <- data$weight %||% rep(1, length(data$x))

    if (!is.na(date_resolution)) {
      data$x <- trans$inverse(data$x)
      org_tz <- lubridate::tz(data$x)

      data <- data |>
        bin_dates(
          dates_from = x, n = weight, fill_gaps = fill_gaps,
          week_start = week_start, date_resolution = date_resolution
        )

      if (is.numeric(data$x)) {
        # For isoyear and epiyear set date to 1st January, binning is still correct
        date_resolution <- "year"
        if (trans$name == "date") {
          data$x <- paste0(data$x, "-01-01") |> lubridate::ymd()
        } else {
          data$x <- paste0(data$x, "-01-01") |> lubridate::ymd(tz = org_tz)
        }
      }

      data$x_ll <- as.numeric(data$x)
      data$x_ul <- as.numeric(lubridate::ceiling_date(data$x,
        unit = date_resolution,
        week_start = week_start,
        change_on_boundary = TRUE
      ))
    } else {
      data <- data |>
        dplyr::arrange(x) |>
        dplyr::group_by(x) |>
        dplyr::tally(wt = weight) |>
        dplyr::ungroup()

      data$x_ll <- data$x
      data$x_ul <- data$x
    }

    bars <- data |>
      dplyr::transmute(
        count = n,
        prop = n / sum(abs(n)), # abs? negative weights?
        # TODO: incidence
        x = x_ll,
        x_ul = x_ul,
        width = if (!is.na(date_resolution)) x_ul - x_ll else NULL,
        .size = length(n),
        flipped_aes = flipped_aes
      )
    ggplot2::flip_data(bars, flipped_aes)
  },
  dropped_aes = "weight"
)

#' @rdname geom_epicurve
#' @format NULL
#' @usage NULL
#' @export
StatDateCount <- ggplot2::ggproto("StatDateCount", StatBinDate)

#' @import ggplot2
#' @import dplyr
#' @import rlang
#' @import lubridate
#' @importFrom cli cli_alert_info cli_alert_warning
GeomEpicurve <- ggplot2::ggproto("GeomEpicurve", GeomBar,
  default_aes = ggplot2:::defaults(
    # colour = from_theme(paper), linewidth = from_theme(borderwidth)
    aes(colour = "white", linewidth = 0.6, linetype = "solid"),
    GeomBar$default_aes
  ),
  extra_params = c(GeomBar$extra_params, "date_resolution", "relative.width"),
  setup_params = function(data, params) {
    params <- GeomBar$setup_params(data, params)
    # Disable date binning if not specified
    params$date_resolution <- params$date_resolution %||% NA
    # Full (100%) width bars
    params$relative.width <- params$relative.width %||% 1

    if (!is.null(params$colour)) {
      data$colour <- params$colour
    }

    if (!dplyr::between(params$relative.width, 0, 1)) {
      cli::cli_warn("relative.width is {params$relative.width}.
                             relative.width should be between 0 and 1 (geom_epicurve).")
    }
    params
  },
  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data$just <- params$just %||% 0.5

    if (!is.na(params$date_resolution)) {
      # Calculate width of bar in days based on specified rounding
      # data$width <- (data$date - data$x) #Here or in stat?

      data |>
        dplyr::distinct(x, width, just) |>
        dplyr::arrange(x) -> data_width

      # Adjust Bars to avoid jittering when using months
      if (nrow(data_width) > 1 & dplyr::n_distinct(data_width$width) != 1) {
        for (i in 2:nrow(data_width)) {
          # Check if there is a space between bars
          if ((data_width[i, ]$x - data_width[i - 1, ]$x) == data_width[i - 1, ]$width) {
            # If there is a previous bar, adjust justification to avoid gaps or overlap
            data_width[i, ]$just <- (data_width[i - 1, ]$width * data_width[i - 1, ]$just) / data_width[i, ]$width
          }
        }
      }
      # What about data width?
      data |>
        dplyr::select(-just, -width) |>
        dplyr::left_join(data_width, by = "x") |>
        dplyr::mutate(width = params$width %||% width * params$relative.width) -> data
    } else {
      data$width <- params$width %||% (resolution(data$x) * params$relative.width)
    }

    # Plotting Checks
    max_bar_height <- data |>
      dplyr::count(x) |>
      dplyr::slice_max(n, n = 1, with_ties = FALSE) |>
      dplyr::pull(n)

    if (max_bar_height[1] > 200) {
      cli::cli_alert_warning(
        "To many observations per date. If you experience problems, please use color = NA to disable outlines (geom_epicurve)."
      )
    }

    if ((max_bar_height[1] > 10000)) {
      cli::cli_alert_warning(
        "If you experience performance problems because of high case numbers, consider using stat = 'bin_date' (geom_epicurve)."
      )
    }

    x_width <- diff(range(data$x)) / data[1, ]$width
    if (x_width > 300) {
      cli::cli_alert_warning(
        "To many bars. If you experience problems, please change date_resolution to a lower resolution or use color = NA to disable outlines (geom_epicurve)."
      )
    }

    data <- transform(data,
      ymin = pmin(y, 0), ymax = pmax(y, 0),
      xmin = x - width * just, xmax = x + width * (1 - just),
      width = NULL, just = NULL
    )
    flip_data(data, params$flipped_aes)
  },
  rename_size = TRUE
)

#' @rdname geom_epicurve
#' @export
geom_epicurve_text <- function(mapping = NULL, data = NULL,
                               stat = "epicurve",
                               vjust = 0.5,
                               date_resolution = NULL,
                               week_start = getOption("lubridate.week.start", 1),
                               ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomText,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position_stack(vjust = vjust),
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      date_resolution = date_resolution,
      week_start = week_start,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_epicurve
#' @export
geom_epicurve_point <- function(mapping = NULL, data = NULL,
                                stat = "epicurve",
                                vjust = 0.5,
                                date_resolution = NULL,
                                week_start = getOption("lubridate.week.start", 1),
                                ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomPoint,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position_stack(vjust = vjust),
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      date_resolution = date_resolution,
      week_start = week_start,
      na.rm = na.rm,
      ...
    )
  )
}
