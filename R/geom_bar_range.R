#' Create a ranged bar chart
#'
#' @description
#' Creates a bar chart with explicitly defined ranges.
#'
#' @inheritParams ggplot2::geom_bar
#' @param stat Defaults to "identity".
#'
#' @return A ggplot2 layer that can be added to a plot created with [ggplot()].
#'
#' @section Aesthetics:
#' Required aesthetics:
#' \itemize{
#'   \item Either `x` or `y`
#'   \item Either `xmin` and `xmax` or `ymin` and `ymax`
#' }
#'
#' @examples
#' # Basic example
#' library(ggplot2)
#' df <- data.frame(x = 1:3, ymin = -1:-3, ymax = 1:3)
#' ggplot(df, aes(x = x, ymin = ymin, ymax = ymax)) +
#'   geom_col_range()
#'
#' @export
geom_col_range <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomBarRange,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

GeomBarRange <- ggproto("GeomBarRange", GeomBar,
  required_aes = c("x|y"), # , "xmin|ymin", "xmax|ymax"

  # These aes columns are created by setup_data(). They need to be listed here so
  # that GeomRect$handle_na() properly removes any bars that fall outside the defined
  # limits, not just those for which x and y are outside the limits
  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
  default_aes = aes(!!!GeomRect$default_aes, width = 0.9),
  setup_params = function(data, params) {
    # Stat diverging returns x, xmax, xmin for flipped aes,
    # range_is_orthogonal determines flipped based on ranges (i.e. xmax, xmin and ymax, ymin)
    params$flipped_aes <- ggplot2::has_flipped_aes(data, params, range_is_orthogonal = TRUE)

    params
  },
  extra_params = c("just", "na.rm", "orientation"),
  setup_data = function(self, data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data$just <- params$just %||% 0.5
    data$width <- data$width %||% params$width %||% 0.9
    data <- transform(data,
      xmin = x - width * just, xmax = x + width * (1 - just),
      width = NULL, just = NULL
    )
    flip_data(data, params$flipped_aes)
  },
  rename_size = FALSE
)
