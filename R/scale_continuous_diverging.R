#' Diverging continuous scales for diverging bar charts with symmetrical limits
#'
#' These scales automatically create symmetrical limits around a centre point (zero by default).
#' They're useful for diverging continuous variables where the visual encoding should
#' be balanced around a center point, such as positive and negative values.
#' They are intended to be used with [geom_bar_diverging()], [geom_area_diverging()] and [stat_diverging()].
#'
#' @inheritParams ggplot2::scale_x_continuous
#' @inheritParams ggplot2::scale_y_continuous
#' @param limits Numeric vector of length two providing limits of the scale.
#'   If `NULL` (the default), limits are automatically computed to be
#'   symmetrical around zero.
#' @param labels Either `waiver()`, a character vector or a function that takes the breaks
#'   as input and returns labels as output. By default, absolute values are displayed or passed to the label function.
#' @param transform Defaults to "identity". Use "reverse" to invert the scale.
#' Especially useful to flip the direction of diverging bar charts.
#'
#' @return A `ggplot2` scale object that can be added to a plot.
#'
#' @seealso [geom_bar_diverging()], [geom_area_diverging()], [stat_diverging()]
#' @examples
#' library(ggplot2)
#'
#' # Create sample data with positive and negative values
#' df <- data.frame(
#'   x = c(-5, -2, 0, 3, 7),
#'   y = c(2, -1, 0, -3, 5)
#' )
#'
#' # Basic usage
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   scale_x_continuous_diverging() +
#'   scale_y_continuous_diverging()
#'
#' @name scale_continuous_diverging
NULL

#' @rdname scale_continuous_diverging
#' @export
scale_x_continuous_diverging <- function(name = waiver(), limits = NULL, labels = NULL,
                                         transform = "identity", ...,
                                         breaks = waiver(), n.breaks = NULL,
                                         expand = waiver(), position = "bottom") {
  if (!is.null(labels)) {
    # if labels are a list of functions compose, else treat them as labels
    if (
      any(sapply(c(labeller), is.function)) ||
        all(sapply(as.character(labeller), exists, mode = "function"))) {
      labeller <- do.call(scales::compose_label, c(abs, labels))
    } else {
      labeller <- labels
    }
  } else {
    labeller <- abs
  }

  limits <- limits %||% limit_symmetrical

  ggplot2::scale_x_continuous(
    name = name,
    limits = limits,
    labels = labeller,
    ...,
    breaks = breaks,
    n.breaks = n.breaks,
    transform = transform,
    expand = expand,
    position = position
  )
}

#' @rdname scale_continuous_diverging
#' @export
scale_y_continuous_diverging <- function(name = waiver(), limits = NULL, labels = NULL,
                                         transform = "identity", ...,
                                         breaks = waiver(), n.breaks = NULL,
                                         expand = waiver(), position = "left") {
  if (!is.null(labels)) {
    # if labels are a list of functions compose, else treat them as labels
    if (
      any(sapply(c(labels), is.function)) ||
        all(sapply(as.character(labels), exists, mode = "function"))) {
      labeller <- do.call(scales::compose_label, c(abs, labels))
    } else {
      labeller <- labels
    }
  } else {
    labeller <- abs
  }

  limits <- limits %||% limit_symmetrical

  ggplot2::scale_y_continuous(
    name = name,
    limits = limits,
    labels = labeller,
    ...,
    breaks = breaks,
    n.breaks = n.breaks,
    transform = transform,
    expand = expand,
    position = position
  )
}

# This function creates symmetrical limits around 0 (offset).
limit_symmetrical <- function(x, center = 0) (max(abs(x - center)) * sign(x)) + center
