library(ggplot2)
#' @import ggplot2
#' @importFrom cli cli_abort
#' @importFrom dplyr mutate row_number

# Rename to match ggplot2 pattern
StatEpicurve <- ggproto("StatEpicurve", Stat,
  required_aes = "x|y",
  default_aes = aes(x = after_stat(count), y = after_stat(count), group = row_number),
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
    bars <- data |> dplyr::mutate(row_number = dplyr::row_number(data), count = 1)
    flip_data(bars, flipped_aes)
  }
)

#' Create an epidemic curve plot
#'
#' @param mapping Set of aesthetic mappings
#' @param data The data to be displayed
#' @param stat The statistical transformation to use
#' @param linewidth Line width for the bars outline
#' @param size Deprecated. Use linewidth instead
#' @param position Position adjustment # Stack, Fill?
#' @param ... Other arguments passed to layer()
#' @param na.rm If FALSE, remove missing values
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes If FALSE, overrides the default aesthetics
#' @export
geom_epicurve <- function(mapping = NULL, data = NULL,
                         linewidth = NULL, position = "stack", 
                         ..., na.rm = FALSE, color = "white",
                         show.legend = NA, inherit.aes = TRUE) {
  
  # GeomEpicurve params ist ein besserer Ort
  linewidth <- linewidth %||% .5 * .pt
  
  # ggplot2::resolution
  # Datetime
  # specify resolution 
  # https://github.com/tidyverse/ggplot2/blob/efc53cc000e7d86e3db22e1f43089d366fe24f2e/R/geom-bar.R

  list(
    layer(
      geom = "bar",
      mapping = mapping,
      data = data,
      stat = StatEpicurve,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        linewidth = linewidth,
        color = color,
        #fill = NA,
        na.rm = na.rm,
        ...
      )
    )
  )
}

# Test the custom geom
plot_data_epicurve_imp <- data.frame(
  date = rep(as.Date("2024-01-01") + (0:6) * 2, times = c(3, 6, 4, 1, 4, 5, 1))
  # category = rep(c("A", "B"), times = 7)
)

ggplot(plot_data_epicurve_imp, aes(x = date)) +
  geom_epicurve(width = 0.9) +
  labs(title = "Epicurve Example") +
  theme_bw()

# # Test the custom geom
ggplot(mtcars, aes(y = factor(cyl), fill = factor(gear))) +
   geom_epicurve(width = 0.9)
#ggsave("test.png")

ggplot(mtcars, aes(x = factor(cyl), fill = factor(gear))) +
  geom_epicurve(width = 0.9, size = 0.5) +
  labs(title = "Epicurve Example")

#resolution(as.numeric(plot_data_epicurve_imp$date), zero = F)

# 
# GeomEpicurve <- ggproto("GeomEpicurve", GeomBar,
#                    required_aes = c("x", "y"),
#                    
#                    # These aes columns are created by setup_data(). They need to be listed here so
#                    # that GeomRect$handle_na() properly removes any bars that fall outside the defined
#                    # limits, not just those for which x and y are outside the limits
#                    non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
#                    
#                    default_aes = aes(!!!GeomRect$default_aes, width = NULL),
#                    
#                    setup_params = function(data, params) {
#                      params$flipped_aes <- has_flipped_aes(data, params)
#                      params
#                    },
#                    
#                    extra_params = c("just", "na.rm", "orientation"),
#                    
#                    setup_data = function(data, params) {
#                      data$flipped_aes <- params$flipped_aes
#                      data <- flip_data(data, params$flipped_aes)
#                      data$width <- data$width %||%
#                        params$width %||% (min(vapply(
#                          split(data$x, data$PANEL, drop = TRUE),
#                          resolution, numeric(1), zero = FALSE
#                        )) * 0.9)
#                      data$just <- params$just %||% 0.5
#                      data <- transform(data,
#                                        ymin = pmin(y, 0), ymax = pmax(y, 0),
#                                        xmin = x - width * just, xmax = x + width * (1 - just),
#                                        width = NULL, just = NULL
#                      )
#                      flip_data(data, params$flipped_aes)
#                    },
#                    
#                    rename_size = TRUE
# )
