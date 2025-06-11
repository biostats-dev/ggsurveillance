#' Continuous x-axis and y-axis scale for (case) counts
#'
#' A continuous ggplot scale for count data with sane defaults for breaks.
#' It uses [base::pretty()] to increase the default number of breaks and prefers 5er breaks.
#' Additionally, the first tick (i.e. zero) is aligned to the lower left corner.
#'
#' @inheritParams ggplot2::scale_y_continuous
#' @param n Target number of breaks passed to [base::pretty()]. Defaults to 8.
#' @param min.n Minimum number of breaks passed to [base::pretty()]. Defaults to 5.
#' @param u5.bias The "5-bias" parameter passed to [base::pretty()]; higher values
#'   push the breaks more strongly toward multiples of 5. Defaults to 4.
#' @param expand Uses own expansion logic. Use \code{expand = waiver()} to restore ggplot defaults
#' or [ggplot2::expansion()] to modify
#' @param limits The lower limit defaults to 0 and the upper limits is chosen based on the data.
#' This is the recommended approach for visualizing case numbers and incidences,
#' i.e. the scale starts at 0 and is only positive.
#' To use the default `ggplot2` limits use `limits = NULL`.
#' @param ... Additional arguments passed on to [base::pretty()].
#'
#' @return A `ggplot2` scale object that can be added to a plot.
#' @seealso [geom_epicurve()], [ggplot2::scale_y_continuous()], [base::pretty()],
#' [theme_mod_remove_minor_grid_y()]
#'
#' @examples
#' library(ggplot2)
#'
#' data <- data.frame(date = as.Date("2024-01-01") + 0:30)
#' ggplot(data, aes(x = date)) +
#'   geom_epicurve(date_resolution = "week") +
#'   scale_y_cases_5er() +
#'   theme_mod_remove_minor_grid_y()
#' @importFrom scales censor
#' @export

scale_y_cases_5er <- function(
    name = waiver(),
    n = 8, min.n = 5, u5.bias = 4,
    expand = NULL, limits = c(0, NA),
    labels = waiver(),
    oob = scales::censor, na.value = NA_real_,
    transform = "identity", position = "left",
    sec.axis = waiver(), guide = waiver(), ...) {
  # Scale Continuous
  scale_y_continuous(
    name = name,
    # Pass Pretty arguments
    breaks = .auto_pretty(n = n, min.n = min.n, u5.bias = u5.bias, ...),
    # minor_breaks = waiver(),
    labels = labels,
    limits = limits,
    expand = expand %||% expansion(mult = c(0, 0.1)),
    oob = oob,
    na.value = na.value,
    transform = transform,
    position = position,
    sec.axis = sec.axis,
    guide = guide
  )
}

#'
#' @rdname scale_y_cases_5er
#' @export

scale_x_cases_5er <- function(
    name = waiver(),
    n = 8, min.n = 5, u5.bias = 4,
    expand = NULL, limits = c(0, NA),
    labels = waiver(),
    oob = scales::censor, na.value = NA_real_,
    transform = "identity", position = "bottom",
    sec.axis = waiver(), guide = waiver(), ...) {
  # Scale Continuous
  scale_x_continuous(
    name = name,
    # Pass Pretty arguments
    breaks = .auto_pretty(n = n, min.n = min.n, u5.bias = u5.bias, ...),
    # minor_breaks = waiver(),
    labels = labels,
    limits = limits,
    expand = expand %||% expansion(mult = c(0, 0.1)),
    oob = oob,
    na.value = na.value,
    transform = transform,
    position = position,
    sec.axis = sec.axis,
    guide = guide
  )
}

.auto_pretty <- function(n = 8, min.n = 5, u5.bias = 4, ...) {
  force_all(n, min.n, u5.bias, ...)
  function(x) {
    pretty(
      x,
      n = if (max(x) < n) max(x) else n,
      min.n = if (max(x) < min.n) max(x) else if (min.n > n) n else min.n,
      u5.bias = u5.bias,
      ...
    )
  }
}
