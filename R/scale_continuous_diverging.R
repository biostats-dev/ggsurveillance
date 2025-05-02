#' @export
scale_y_continuous_diverging <- function(name = waiver(), limits = NULL, labels = NULL, ...,
                                         breaks = waiver(), n.breaks = NULL, transform = "identity",
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

#' @export
scale_x_continuous_diverging <- function(name = waiver(), limits = NULL, labels = NULL, ...,
                                         breaks = waiver(), n.breaks = NULL, transform = "identity",
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

# This function creates symmetrical limits around 0 (offset).
limit_symmetrical <- function(x, center = 0) (max(abs(x - center)) * sign(x)) + center
