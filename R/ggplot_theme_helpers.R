#' Quickly remove the minor lines of the panel grid
#'
#' @description
#' `th_remove_panel_grid_minor()`, `th_remove_panel_grid_minor_x()`,
#' `th_remove_panel_grid_minor_y()` are convenience functions remove the minor lines
#' of the panel grid.
#' Has to be called after setting the theme.
#' @return Changes the `panel.grid.minor` of the [ggplot2::theme()].
#' @export
# TODO: https://pkgdown.r-lib.org/reference/build_reference.html
th_remove_panel_grid_minor <- function() {
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}
#' @rdname th_remove_panel_grid_minor
#' @export
th_remove_panel_grid_minor_y <- function() {
  ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
}
#' @rdname th_remove_panel_grid_minor
#' @export
th_remove_panel_grid_minor_x <- function() {
  ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
}
#' @rdname th_remove_panel_grid_minor
#' @export
th_remove_panel_grid <- function() {
  ggplot2::theme(panel.grid = ggplot2::element_blank())
}

#' Rotate axis labels
#' @description
#' Rotate axis labels by 90°, 45° or any angle.
#' Has to be called after setting the theme.
#' @param angle Angle of rotation. Should be between 10 and 90 degrees.
#' @param margin_top Used to move the tick labels downwards to prevent text intersecting the x-axis.
#' Increase for multiline text (e.g. 5 for two lines).
#' @param hjust,vjust Text justification within the rotated text element. Just ignore.
#' @param ... Arguments passed to `th_rotate_x_axis_labels` and [ggplot2::element_text()].
#' @return Changes the rotation of the axis labels by modifying the `axis.text` of the [ggplot2::theme()].
#' @export
#' @name th_rotate_axis_labels
#' @rdname th_rotate_axis_labels
th_rotate_x_axis_labels <- function(angle = 90, margin_top = 2, vjust = 0.4, hjust = 0, ...) {
  ggplot2::theme(axis.text.x = ggplot2::element_text(
    angle = -angle, vjust = vjust, hjust = hjust,
    margin = margin(t = margin_top)
  ), ...)
}
#' @rdname th_rotate_axis_labels
#' @export
th_rotate_x_axis_labels_90 <- function(angle = 90, ...) th_rotate_x_axis_labels(angle = angle)
#' @rdname th_rotate_axis_labels
#' @export
th_rotate_x_axis_labels_45 <- function(angle = 45, ...) th_rotate_x_axis_labels(angle = angle)
#' @rdname th_rotate_axis_labels
#' @export
th_rotate_x_axis_labels_30 <- function(angle = 30, ...) th_rotate_x_axis_labels(angle = angle)
#' @rdname th_rotate_axis_labels
#' @export
th_rotate_x_axis_labels_60 <- function(angle = 60, ...) th_rotate_x_axis_labels(angle = angle)
# testthat::expect_identical(theme_rotate_x_axis_labels(), theme(axis.text.x = element_text(angle = -90, vjust = 0.5)))
#' @rdname th_rotate_axis_labels
#' @export
th_rotate_y_axis_labels <- function(angle = 90, hjust = 0.5, vjust = 0, ...) {
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = angle, hjust = hjust, vjust = vjust, ...))
}


#' Quickly remove the minor lines of the panel grid
#'
#' @description
#' Convenience functions to control the legend position for `ggplot2`.
#' Has to be called after setting the theme.
#' @param position Position of the ggplot2 legend.
#' Options are `("top", "bottom", "left", "right", "none", "inside")`
#' @param position.inside Coordinates for the legend inside the plot.
#' If set overwrites `position` to `inside`.
#' @return Changes the `legend.position` of the [ggplot2::theme()].
#' @name th_disable_legend, th_legend_position
#' @rdname th_disable_legend
#' @export
th_disable_legend <- function() ggplot2::theme(legend.position = "none")
#' @rdname th_disable_legend
#' @export
th_legend_position <- function(
    position = c("top", "bottom", "left", "right", "none", "inside"), position.inside = NULL) {
  position <- match.arg(position)
  if (!is.null(position.inside)) position <- "inside"
  # TODO: Check if position.inside is vector
  ggplot2::theme(legend.position = position, legend.position.inside = position.inside)
}
#' @rdname th_disable_legend
#' @export
th_legend_top <- function() ggplot2::theme(legend.position = "top")
#' @rdname th_disable_legend
#' @export
th_legend_bottom <- function() ggplot2::theme(legend.position = "bottom")
#' @rdname th_disable_legend
#' @export
th_legend_left <- function() ggplot2::theme(legend.position = "left")
#' @rdname th_disable_legend
#' @export
th_legend_right <- function() ggplot2::theme(legend.position = "right")


#' Quick legend helpers
#'
#' @description
#'  - `th_guide_thick_lines()`: Increase the line thickness for `geom_line()` **colour** legends.
#'  - `th_guide_long_lines()`: Increase the line thickness for `geom_line()` **linetype** legends.
#'  - `th_remove_legend_title()`: Removes the legend title. Use after setting the theme.
#' @param linewidth Line width of the line shown in the colour legend.
#' @param keywidth Key width of the line shown in the legend.
#' @param ... further parameters passed to [ggplot2::guide_legend()].
#' @return Overrides the `aes` of the colour guide using [ggplot2::guides()].
#' @name Quick legend helpers
#' @rdname th_legend_helpers
NULL

#' @rdname th_legend_helpers
#' @export
th_guide_thick_lines <- function(linewidth = 3, ...) {
  ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(linewidth = linewidth), ...))
}

#' @rdname th_legend_helpers
#' @export
th_guide_long_lines <- function(keywidth = 3, ...) {
  # TODO: nur Lintype oder alle mit theme?
  ggplot2::guides(linetype = ggplot2::guide_legend(keywidth = keywidth, ...))
}

#' @rdname th_legend_helpers
#' @export
th_remove_legend_title <- function() {
  ggplot2::theme(legend.title = ggplot2::element_blank())
}
