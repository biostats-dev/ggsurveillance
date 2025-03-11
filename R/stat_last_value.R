#' Add labels or points to the last value of a line chart
#'
#' Creates a label, point or any geom at the last point of a line (highest x value). This is useful for
#' line charts where you want to identify each line at its endpoint, write the last value of a
#' time series at the endpoint or just add a point at the end of a \code{\link[ggplot2]{geom_line}}. This functions
#' also nudges the last value relative to the length of the x-axis.
#' The function automatically positions the label slightly to the right of the last point.
#' There are 5 functions:
#' * `stat_last_value()`: The core statistical transformation that identifies the last point of a line 
#' (e.g. last date of the time series). 
#' * `geom_last_value_label()`: Adds the last y value or a custom label 
#' after the last observation using \code{\link[ggplot2]{geom_label}}.
#' * `geom_last_value_text()`: Adds the last y value or a custom text 
#' after the last observation using \code{\link[ggplot2]{geom_text}}.
#' * `geom_last_value_label_repel()`: Adds non-overlapping labels with \code{\link[ggrepel]{geom_label_repel}}.
#' * `geom_last_value_text_repel()`: Adds non-overlapping text with \code{\link[ggrepel]{geom_label_repel}}.
#' 
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}. Commonly used mappings:
#'   * **x**: position on x-axis
#'   * **y**: position on y-axis
#'   * **label**: text to display (defaults to the last y value)
#' @param data The data frame containing the variables for the plot
#' @param stat The statistical transformation to use on the data. Defaults to "last_value"
#' @param position Position adjustment. Defaults to "identity"
#' @param rel_nudge Numeric value specifying how far to nudge the label to the right,
#'        relative to the width of the x-axis. Defaults to 0.05 (5% of axis width) for the label geoms.
#' @param ... Other arguments passed to \code{\link[ggplot2]{geom_label}}, \code{\link[ggplot2]{geom_text}}
#' @param geom  The geometric object to use to display the data for this layer.
#'   When using a `stat_*()` function to construct a layer, the `geom` argument
#'   can be used to override the default coupling between stats and geoms.
#' @inheritParams ggplot2::geom_label
#' @inheritParams ggrepel::geom_label_repel
#'
#' @return A `ggplot2` layer that can be added to a plot
#' @examples
#' # Basic example with last value labels
#' library(ggplot2)
#' 
#' ggplot(economics, aes(x = date, y = unemploy, group = 1)) +
#'   geom_line() +
#'   geom_last_value_label()
#'
#' # Multiple lines with custom labels
#' ggplot(economics_long, aes(x = date, y = value, color = variable)) +
#'   geom_line() +
#'   geom_last_value_label(aes(label = variable))
#' @name geom_laste_value_label/ stat_last_value
#' @rdname stat_last_value
NULL

#' @rdname stat_last_value
#' @export
stat_last_value <- function(mapping = NULL, data = NULL,
                            geom = "point", position = "identity",
                            rel_nudge = 0,
                            ...,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatLastValue,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      rel_nudge = rel_nudge,
      ...
    )
  )
}

#' @rdname stat_last_value
#' @export 
geom_last_value_label <- function(mapping = NULL, data = NULL,
                                  stat = "last_value", position = "identity",
                                  rel_nudge = 0.05,
                                  ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    geom = ggplot2::GeomLabel,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      rel_nudge = rel_nudge,
      ...
    )
  )
}

#' @rdname stat_last_value
#' @export
geom_last_value_text <- function(mapping = NULL, data = NULL,
                                  stat = "last_value", position = "identity",
                                  rel_nudge = 0.05,
                                  ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    geom = ggplot2::GeomText,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      rel_nudge = rel_nudge,
      ...
    )
  )
}

#' @rdname stat_last_value
#' @export
geom_last_value_label_repel <- function(mapping = NULL, data = NULL,
                                        stat = "last_value_repel", position = "identity",
                                        rel_nudge = 0.05, direction = "y", min.segment.length = 0.5,
                                        ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    geom = ggrepel::GeomLabelRepel,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      rel_nudge = rel_nudge,
      direction = direction,
      min.segment.length = min.segment.length,
      ...
    )
  )
}

#' @rdname stat_last_value
#' @export
geom_last_value_text_repel <- function(mapping = NULL, data = NULL,
                                        stat = "last_value_repel", position = "identity",
                                        rel_nudge = 0.05, direction = "y", min.segment.length = 0.5,
                                        ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    geom = ggrepel::GeomTextRepel,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      rel_nudge = rel_nudge,
      direction = direction,
      min.segment.length = min.segment.length,
      ...
    )
  )
}

#' @import ggplot2
#' @import dplyr
#' @rdname stat_last_value
#' @format NULL
#' @usage NULL
#' @export

#TODO: Custom Label function, Add x0 to docs
StatLastValue <- ggproto("StatLastValue", Stat,
  required_aes = c("x", "y"),
  extra_params = c("na.rm", "rel_nudge"),
  default_aes = aes(label = after_stat(y)),
  setup_params = function(self, data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(data, params, main_is_orthogonal = FALSE)
    params
  },
  compute_group = function(data, scales, flipped_aes = NA, rel_nudge = 0, ...) {
    flipped_aes <- flipped_aes %||% any(data$flipped_aes) %||% FALSE

    if (!flipped_aes) {
      sel_scale <- scales$x
      sel_axis <- "x-axis"
    } else {
      sel_scale <- scales$y
      sel_axis <- "y-axis"
    }

    scale_width <- diff(sel_scale$range$range)

    # data$label <- data$label %||% data$y
    # last y, last x? label as default after_stat?
    data |>
      dplyr::filter(x == max(x)) |> # warning? return only 1?
      dplyr::mutate(
        x0 = x,
        x = x + rel_nudge * scale_width # recalc x depending on length of x-axis (scale)
      ) |>
      ggplot2::flip_data(flipped_aes)
  },
  dropped_aes = "weight"
)

#' @import ggplot2
#' @import dplyr
#' @rdname stat_last_value
#' @format NULL
#' @usage NULL
#' @export
StatLastValueRepel <- ggproto("StatLastValueRepel", Stat,
  required_aes = c("x", "y"),
  extra_params = c("na.rm", "rel_nudge"),
  default_aes = aes(label = after_stat(y)),
  setup_params = function(self, data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(data, params, main_is_orthogonal = FALSE)
    params
  },
  compute_group = function(data, scales, flipped_aes = NA, rel_nudge = 0, ...) {
    flipped_aes <- flipped_aes %||% any(data$flipped_aes) %||% FALSE

    if (!flipped_aes) {
      sel_scale <- scales$x
      sel_axis <- "x-axis"
    } else {
      sel_scale <- scales$y
      sel_axis <- "y-axis"
    }

    scale_width <- diff(sel_scale$range$range)

    data |>
      dplyr::filter(x == max(x)) |> # warning? return only 1?
      dplyr::mutate(
        x0 = x, # x0 is a by ggplot recognized x value (for flipping etc.)
        x = x,
        nudge_x = x + rel_nudge * scale_width, # Nudging for ggrepel (new x coordinate)
        xmax = nudge_x # Force extension of the x-axis scale limits
      ) |>
      ggplot2::flip_data(flipped_aes)
  },
  dropped_aes = "weight"
)
