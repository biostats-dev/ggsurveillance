#' Continuous x-axis and y-axis scale for (case) counts
#'
#' A continuous ggplot scale for count data with sane defaults for breaks.
#' It uses [base::pretty()] to increase the default number of breaks and prefers 5er breaks.
#' Additionally, the first tick (i.e. zero) is aligned to the lower left corner.
#'
#' @inheritParams ggplot2::scale_y_continuous
#' @param n Target number of breaks passed to [base::pretty()]. Defaults to 8.
#' @param n.min Minimum number of breaks passed to [base::pretty()]. Defaults to 5.
#' @param u5.bias The "5-bias" parameter passed to [base::pretty()]; higher values
#'   push the breaks more strongly toward multiples of 5. Defaults to 4.
#' @param expand Uses own expansion logic. Use \code{expand = waiver()} to restore ggplot defaults
#' or [ggplot2::expansion()] to modify
#' @param ... Additional arguments passed on to [base::pretty()].
#'
#' @return A **ggplot2** scale object that can be added to a plot.
#' @seealso [geom_epicurve()], [ggplot2::scale_y_continuous()], [base::pretty()]
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' data <- data.frame(date = as.Date("2024-01-01") + 0:30)
#' ggplot(data, aes(x = date)) +
#'   geom_epicurve(date_resolution = "week") +
#'   scale_y_cases_5er()
#' }
#' @importFrom scales censor
#' @export

scale_y_cases_5er <- function(
    name = waiver(),
    n = 8,
    n.min = 5,
    u5.bias = 4,
    expand = NULL,
    labels = waiver(),
    limits = NULL,
    oob = scales::censor,
    na.value = NA_real_,
    trans = "identity",
    position = "left",
    sec.axis = waiver(),
    guide = waiver(),
    ...
) {

  scale_y_continuous(
    name = name,
    # Pass Pretty arguments
    breaks = function(x) pretty(
      x,
      n = if (max(x) < n) ceiling(max(x)) else n,
      n.min = if (max(x) < n.min) ceiling(max(x)) else n.min,
      u5.bias = u5.bias,
      ...
    ),
    #minor_breaks = waiver(),
    labels = labels,
    limits = limits,
    expand = expand %||% expansion(mult = c(0, 0.1)),
    oob = oob,
    na.value = na.value,
    trans = trans,
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
    n = 8,
    n.min = 5,
    u5.bias = 4,
    expand = NULL,
    labels = waiver(),
    limits = NULL,
    oob = scales::censor,
    na.value = NA_real_,
    trans = "identity",
    position = "bottom",
    sec.axis = waiver(),
    guide = waiver(),
    ...
) {

  scale_x_continuous(
    name = name,
    # Pass Pretty arguments
    breaks = function(x) {pretty(
      x,
      n = n,
      n.min = if (max(x) < n.min) ceiling(max(x)) else n.min,
      u5.bias = u5.bias,
      ...
    )},
    #minor_breaks = waiver(),
    labels = labels,
    limits = limits,
    expand = expand %||% expansion(mult = c(0, 0.1)),
    oob = oob,
    na.value = na.value,
    trans = trans,
    position = position,
    sec.axis = sec.axis,
    guide = guide
  )
}
