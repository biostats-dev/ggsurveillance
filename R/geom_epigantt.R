#' Epi Gantt Chart: Visualize Epidemilogical Time Intervalls
#'
#' Various ways of representing a vertical interval defined by `y`,
#' `xmin` and `xmax`. Each case draws a single graphical object.
#'
#' @inheritParams ggplot2::geom_linerange
#'
#' @export
#' @examples
#'
#'
geom_epigantt <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomEpigantt,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @import ggplot2
GeomEpigantt <- ggproto("GeomEpigantt", GeomLinerange,

                         default_aes = ggplot2:::defaults(
                           # linewidth = from_theme(borderwidth)
                           aes(colour = "dodgerblue4", linewidth = 8, linetype = "solid"),
                           GeomLinerange$default_aes),
# TODO: auto adjust linewidth and warn if to many cases.
)
