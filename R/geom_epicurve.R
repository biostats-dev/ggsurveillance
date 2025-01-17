#' @import ggplot2
#' @import dplyr
#' @import rlang
#' @importFrom cli cli_abort

# Rename to match ggplot2 pattern
StatEpicurve <- ggproto("StatEpicurve", Stat,
  required_aes = "x|y",
  default_aes = aes(x = after_stat(count), y = after_stat(count), group = row_number, weight = 1),
  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE)

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
  compute_layer = function(data, scales, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)

    weight <- data$weight %||% rep(1, length(data$x))
    data <- data |> expand_counts(weight)

    bars <- data |> dplyr::mutate(row_number = dplyr::row_number(data), count = 1)
    flip_data(bars, flipped_aes)
  },
  dropped_aes = "weight"
)

#' @import ggplot2
#' @import dplyr
#' @import rlang
#' @import lubridate
#' @importFrom cli cli_alert_info cli_alert_warning
GeomEpicurve <- ggproto("GeomEpicurve", GeomBar,
  default_aes = ggplot2:::defaults(
    # colour = from_theme(paper), linewidth = from_theme(borderwidth)
    aes(colour = "white", linewidth = 1, linetype = "solid"),
    GeomBar$default_aes
  ),
  extra_params = c(GeomBar$extra_params, "date_resolution", "relative.width", "datetime", "week_start", "stat"),
  setup_params = function(data, params) {
    params <- GeomBar$setup_params(data, params)
    # Disable date binning if not specified
    params$date_resolution <- params$date_resolution %||% NA
    # Full (100%) width bars
    params$relative.width <- params$relative.width %||% 1
    # Check values of x are so large to be datetime
    params$datetime <- params$datetime %||% (max(data$x, na.rm = TRUE) > 10^6)
    # Week_start defaults to Monday
    params$week_start <- params$week_start %||% 1

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

    # Drop missing x
    complete <- stats::complete.cases(data$x)
    data <- data |> dplyr::filter(complete)
    if (!all(complete) && !params$na.rm) {
      cli::cli_warn(paste0(
        "Removed {sum(!complete)} row{?s} containing missing values (geom_epicurve)."
      ))
    }

    if (!is.na(params$date_resolution)) {
      # Try to infer if x was date or datetime and convert to date.
      data$date <- if (params$datetime) lubridate::as_datetime(data$x) else lubridate::as_date(data$x)
      # Round to specified resolution
      data$x <- as.numeric(lubridate::floor_date(data$date,
        unit = params$date_resolution,
        week_start = params$week_start
      ))
      # Use ceiling to be able to infer resolution in days using ggplot2::resolution
      data$date <- as.numeric(lubridate::ceiling_date(data$date,
        unit = params$date_resolution,
        week_start = params$week_start,
        change_on_boundary = TRUE
      ))
      # Calculate width of bar in days based on specified rounding
      data$width <- (data$date - data$x)

      data |>
        dplyr::distinct(x, width, just) |>
        dplyr::arrange(x) -> data_width

      # Adjust Bars to avoid jittering when using months
      if (nrow(data_width) > 1) {
        for (i in 2:nrow(data_width)) {
          # Check if there is a space between bars
          if ((data_width[i, ]$x - data_width[i - 1, ]$x) == data_width[i - 1, ]$width) {
            # If there is a previous bar, adjust justification to avoid gaps or overlap
            data_width[i, ]$just <- (data_width[i - 1, ]$width * data_width[i - 1, ]$just) / data_width[i, ]$width
          }
        }
      }
      data |>
        dplyr::select(-just, -width) |>
        dplyr::left_join(data_width, by = "x") |>
        dplyr::mutate(width = params$width %||% width * params$relative.width) -> data

      # Recalc counts after binning if stat = "count"
      if (params$stat == "count") {
        data <- data |>
          select(-date) |>
          rename(weight = count) |>
          dplyr::group_by(dplyr::pick(-(y:flipped_aes))) |>
          dplyr::group_modify(~ StatCount$compute_group(data = .x)) |>
          dplyr::mutate(y = count) |>
          dplyr::ungroup()
      }
    } else if (params$datetime) {
      cli::cli_alert_info("It seems you provided a datetime format. Column used as specified.
                          Please use the resoultion = 'day' to round to date (geom_epicurve).")
      data$width <- params$width %||% (resolution(data$x) * params$relative.width)
    } else {
      data$width <- params$width %||% (resolution(data$x) * params$relative.width)
    }

    max_bar_height <- data |>
      dplyr::count(x) |>
      dplyr::slice_max(n, n = 1, with_ties = FALSE) |>
      dplyr::pull(n)
    if (max_bar_height[1] > 200) {
      cli::cli_alert_warning(
        "To many observations per date. If you experience problems, please use color = NA to disable outlines."
      )
    }
    x_width <- diff(range(data$x)) / data[1, ]$width
    if (x_width > 300) {
      cli::cli_alert_warning(
        "To many bars. If you experience problems, please change date_resolution to a lower resolution or use color = NA to disable outlines."
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

#' Create an epidemic curve plot
#'
#' Creates a bar plot specifically designed for visualizing epidemic curves (epicurves).
#' Supports both date/datetime and categorical data, with special handling for date-based
#' aggregation.
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}. Commonly used mappings:
#'   * x or y
#'   * fill for colouring groups
#'   * weight
#' @param data The data frame containing the variables for the plot
#' @param stat either "`epicurve`" for outlines around cases or "`count`" for outlines around (fill) groups.
#' For large numbers of cases please use "`count`".
#' @param position Position adjustment. Currently supports "`stack`".
#' @param date_resolution Character string specifying the time unit for date aggregation
#'        (e.g., "`day`", "`week`", "`month`", "`bimonth`", "`season`", "`quarter`", "`halfyear`", "`year`").
#'        Set to \code{NULL} for no date aggregation
#' @param width Numeric value specifying the width of the bars. If \code{NULL}, calculated
#'        based on resolution and relative.width
#' @param relative.width Numeric value between 0 and 1 adjusting the relative width
#'        of bars. Defaults to 1
#' @param week_start Integer specifying the start of the week (1 = Monday, 7 = Sunday).
#'        Only used when date_resolution includes weeks. Defaults to 1 (Monday)
#' @param ... Other arguments passed to \code{\link[ggplot2]{layer}}
#'   * \code{colour} Color of the observation borders. Disable with colour = NA. Defaults to "white".
#'   * \code{linewidth}  Width of the outlines.
#' @param na.rm If FALSE, missing values are removed with a warning.
#'        If TRUE, missing values are silently removed
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes If FALSE, overrides the default aesthetics from the plot
#'
#' @return A ggplot2 layer that can be added to a plot
#' @export
#'
#' @examples
#' # Basic epicurve with dates
#' library(ggplot2)
#' data <- data.frame(date = as.Date("2024-01-01") + 0:30)
#' ggplot(data, aes(x = date)) +
#'   geom_epicurve(date_resolution = "week")
#'
#' # Categorical epicurve
#' ggplot(mtcars, aes(x = factor(cyl))) +
#'   geom_epicurve()
geom_epicurve <- function(mapping = NULL, data = NULL,
                          stat = "epicurve", # or count for no outlines
                          position = "stack",
                          date_resolution = NULL,
                          width = NULL, relative.width = 1,
                          week_start = getOption("lubridate.week.start", 1),
                          ..., na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE) {
  stat_param <- stat

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
      stat = stat_param,
      relative.width = relative.width,
      date_resolution = date_resolution,
      week_start = week_start,
      na.rm = na.rm,
      ...
    )
  )
}
