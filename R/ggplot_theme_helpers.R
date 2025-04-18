#' Quickly remove the minor lines of the panel grid
#'
#' @description
#' `theme_mod_remove_minor_grid()`, `theme_mod_remove_minor_grid_x()`,
#' `theme_mod_remove_minor_grid_y()` are convenience functions remove the minor lines
#' of the panel grid.
#' Has to be called after setting the theme.
#' @return Changes the `panel.grid.minor` of the [ggplot2::theme()].
#' @export
# TODO: https://pkgdown.r-lib.org/reference/build_reference.html
theme_mod_remove_minor_grid <- function() {
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}
#' @rdname theme_mod_remove_minor_grid
#' @export
theme_mod_remove_minor_grid_y <- function() {
  ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
}
#' @rdname theme_mod_remove_minor_grid
#' @export
theme_mod_remove_minor_grid_x <- function() {
  ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
}
#' @rdname theme_mod_remove_minor_grid
#' @export
theme_mod_remove_panel_grid <- function() {
  ggplot2::theme(panel.grid = ggplot2::element_blank())
}

#' Rotate axis labels
#' @description
#' Rotate axis labels by 90°, 45° or any angle.
#' Has to be called after setting the theme.
#' @param angle Angle of rotation. Should be between 10 and 90 degrees.
#' @param margin_top Used to move the tick labels downwards to prevent text intersecting the x-axis.
#' Increase for angled multiline text (e.g. 5 for two lines at 45°).
#' @param hjust,vjust Text justification within the rotated text element. Just ignore.
#' @param ... Arguments passed to `theme_mod_rotate_x_axis_labels` and [ggplot2::element_text()].
#' @return Changes the rotation of the axis labels by modifying the `axis.text` of the [ggplot2::theme()].
#' @export
#' @name theme_mod_rotate_axis_labels
#' @rdname theme_mod_rotate_axis_labels
theme_mod_rotate_x_axis_labels <- function(angle = 90, margin_top = 2, vjust = 0.4, hjust = 0, ...) {
  ggplot2::theme(axis.text.x = ggplot2::element_text(
    angle = -angle, vjust = vjust, hjust = hjust,
    margin = margin(t = margin_top)
  ), ...)
}
#' @rdname theme_mod_rotate_axis_labels
#' @export
theme_mod_rotate_x_axis_labels_90 <- function(angle = 90, ...) theme_mod_rotate_x_axis_labels(angle = angle)
#' @rdname theme_mod_rotate_axis_labels
#' @export
theme_mod_rotate_x_axis_labels_45 <- function(angle = 45, ...) theme_mod_rotate_x_axis_labels(angle = angle)
#' @rdname theme_mod_rotate_axis_labels
#' @export
theme_mod_rotate_x_axis_labels_30 <- function(angle = 30, ...) theme_mod_rotate_x_axis_labels(angle = angle)
#' @rdname theme_mod_rotate_axis_labels
#' @export
theme_mod_rotate_x_axis_labels_60 <- function(angle = 60, ...) theme_mod_rotate_x_axis_labels(angle = angle)
# testthat::expect_identical(theme_rotate_x_axis_labels(), theme(axis.text.x = element_text(angle = -90, vjust = 0.5)))
#' @rdname theme_mod_rotate_axis_labels
#' @export
theme_mod_rotate_y_axis_labels <- function(angle = 90, hjust = 0.5, vjust = 0, ...) {
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = angle, hjust = hjust, vjust = vjust, ...))
}


#' Quickly adjust the legend position
#'
#' @description
#' Convenience functions to control the legend position for `ggplot2`.
#' Has to be called after setting the theme.
#' @param position Position of the ggplot2 legend.
#' Options are `("top", "bottom", "left", "right", "none", "inside")`
#' @param position.inside Coordinates for the legend inside the plot.
#' If set overwrites `position` to `inside`.
#' @return Changes the `legend.position` of the [ggplot2::theme()].
#' @name theme_mod_disable_legend, theme_mod_legend_position
#' @rdname theme_mod_disable_legend
#' @export
theme_mod_disable_legend <- function() ggplot2::theme(legend.position = "none")
#' @rdname theme_mod_disable_legend
#' @export
theme_mod_legend_position <- function(
    position = c("top", "bottom", "left", "right", "none", "inside"), position.inside = NULL) {
  position <- match.arg(position)
  if (!is.null(position.inside)) position <- "inside"
  # TODO: Check if position.inside is vector
  ggplot2::theme(legend.position = position, legend.position.inside = position.inside)
}
#' @rdname theme_mod_disable_legend
#' @export
theme_mod_legend_top <- function() ggplot2::theme(legend.position = "top")
#' @rdname theme_mod_disable_legend
#' @export
theme_mod_legend_bottom <- function() ggplot2::theme(legend.position = "bottom")
#' @rdname theme_mod_disable_legend
#' @export
theme_mod_legend_left <- function() ggplot2::theme(legend.position = "left")
#' @rdname theme_mod_disable_legend
#' @export
theme_mod_legend_right <- function() ggplot2::theme(legend.position = "right")
#' @rdname theme_mod_disable_legend
#' @export
theme_mod_remove_legend_title <- function() ggplot2::theme(legend.title = ggplot2::element_blank())
