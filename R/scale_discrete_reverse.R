#' Reversed discrete scale for 'ggplot2'
#'
#' `scale_y_discrete_reverse()` and `scale_x_discrete_reverse()` are standard discrete 'ggplot2'
#' scales with a reversed order of values. Since the ggplot2 coordinate system starts with 0 in
#' the lower left corner, factors on the y-axis are sorted is descending order by default
#' (i.e. alphabetically from Z to A). With this scale the the y-axis will start with the
#' first factor level at the top or with alphabetically correctly ordered values
#'
#' @param limits Can be either NULL which uses the default reversed scale values
#' or a character vector which will be reversed.
#' @inheritParams ggplot2::scale_y_discrete
#' @param ... Arguments passed on to [ggplot2::discrete_scale()]
#' @seealso [geom_epigantt()], [ggplot2::scale_y_discrete()]
#' @return A `ggplot2` scale object that can be added to a plot.
#' @examples
#' library(ggplot2)
#'
#' # Create sample data
#' df <- data.frame(
#'   category = factor(c("A", "B", "C", "D")),
#'   value = c(10, 5, 8, 3)
#' )
#'
#' # Basic plot with reversed y-axis
#' ggplot(df, aes(x = value, y = category)) +
#'   geom_col() +
#'   scale_y_discrete_reverse()
#'
#' @export
scale_y_discrete_reverse <- function(name = waiver(), limits = NULL, ...,
                                     expand = waiver(), position = "left") {
  # Pass the function rev to limits or reverse the provided limits
  if (rlang::is_empty(limits)) {
    limits <- rev
  } else if (is.vector(limits)) {
    limits <- rev(limits)
  }

  ggplot2::scale_y_discrete(
    name = name,
    limits = limits,
    ...,
    expand = expand,
    position = position
  )
}

#' @rdname scale_y_discrete_reverse
#' @export
scale_x_discrete_reverse <- function(name = waiver(), limits = NULL, ...,
                                     expand = waiver(), position = "left") {
  # Pass the function rev to limits or reverse the provided limits
  if (rlang::is_empty(limits)) {
    limits <- rev
  } else if (is.vector(limits)) {
    limits <- rev(limits)
  }

  ggplot2::scale_x_discrete(
    name = name,
    limits = limits,
    ...,
    expand = expand,
    position = position
  )
}
