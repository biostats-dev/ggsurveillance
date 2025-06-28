#' Nested axis guide for date scales
#'
#' @description
#' A specialized axis guide for date scales that creates nested axis labels
#' by automatically detecting hierarchical patterns in date labels (e.g.,
#' separating day-month from year components). This guide is particularly
#' useful for time series data, where the axis can get crowded when
#' showing the full dates. This is similar to the date scale from Excel.
#'
#' @param sep A regular expression pattern used to split axis labels into
#'   hierarchical components. Default is `"[^[:alnum:]]+"` which splits on
#'   non-alphanumeric characters.
#' @param regular_key Default is `"auto"`, which generates the nested axis based on the date labels and the separator above.
#' This option can be used to provide you own specification for the nested key. See [legendry::key_standard()]
#' @param type The visual type of nested axis guide to create. Options include:
#'   - `"bracket"` (default): Creates bracket-style nested labels
#'   - `"fence"`: Creates fence-style nested labels (like Excel)
#'   - `"box"`: Creates box-style nested labels
#' @param mode Processing mode for the guide. Default is `"simple"`.
#' @param pad_date Numeric value controlling the padding around date levels,
#' i.e. extending the length of the bracket or box or for correctly positioning the fences.
#'   If `NULL` (default), automatically sets to 0.5 for "fence" type and
#'   0.25 for other types.
#' @param oob How to handle out-of-bounds values of the scale labels. Default is `"none"`.
#' Another option is `"squish"`, but this can result in overlapping labels.
#' @param ... Additional arguments passed to [legendry::guide_axis_nested()].
#'
#' @return A nested axis guide object that can be used with [ggplot2::scale_x_date()] etc. or [ggplot2::guides()].
#'
#' @seealso [legendry::guide_axis_nested()]
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Create sample epidemic curve data
#' epi_data <- data.frame(
#'   date = rep(as.Date("2023-12-15") + 0:100, times = rpois(101, 2))
#' )
#'
#' ggplot(epi_data, aes(x = date)) +
#'   geom_epicurve(date_resolution = "week") +
#'   scale_x_date(
#'     date_breaks = "2 weeks", date_labels = "%d-%b-%Y",
#'     guide = guide_axis_nested_date()
#'   )
#'
#' # Using fence type with ISO week labels
#' ggplot(epi_data, aes(x = date)) +
#'   geom_epicurve(date_resolution = "week") +
#'   scale_x_date(
#'     date_breaks = "2 weeks", date_labels = "W%V.%G",
#'     guide = guide_axis_nested_date(type = "fence")
#'   )
#'
#' # Using box type with custom padding
#' ggplot(epi_data, aes(x = date)) +
#'   geom_epicurve(date_resolution = "month") +
#'   scale_x_date(
#'     date_breaks = "1 month", date_labels = "%b.%Y",
#'     guide = guide_axis_nested_date(type = "box", pad_date = 0.3)
#'   )
#'
#' # Custom separator for different label formats
#' ggplot(epi_data, aes(x = date)) +
#'   geom_epicurve(date_resolution = "week") +
#'   scale_x_date(
#'     date_breaks = "1 week", date_labels = "%d-%b-%Y",
#'     guide = guide_axis_nested_date(type = "bracket", sep = "-")
#'   )
#'
#' # Datetime example with fence type
#' datetime_data <- data.frame(
#'   datetime = rep(as.POSIXct("2024-02-05 01:00:00") + 0:50 * 3600,
#'     times = rpois(51, 3)
#'   )
#' )
#'
#' ggplot(datetime_data, aes(x = datetime)) +
#'   geom_epicurve(date_resolution = "2 hours") +
#'   scale_x_datetime(
#'     date_breaks = "6 hours", date_labels = "%Hh %e.%b",
#'     limits = c(as.POSIXct("2024-02-04 22:00:00"), NA),
#'     guide = guide_axis_nested_date()
#'   )
#'
guide_axis_nested_date <- function(
    sep = "[^[:alnum:]]+", # ?
    regular_key = "auto",
    type = "bracket",
    mode = "simple",
    pad_date = NULL,
    oob = "none",
    ...) {
  pad_date <- pad_date %||% switch(type,
    fence = 0.5,
    0.25
  )

  legendry::guide_axis_nested(
    key = key_range_date(sep = sep, mode = mode, pad_date = pad_date),
    regular_key = regular_key,
    type = type,
    oob = oob,
    # drop_zero = drop_zero, #drop zero is ignored for continuous scales
    ...
  )
}

key_range_date <- function(sep = "[^[:alnum:]]+", reverse = FALSE, mode = "simple", pad_date = 0, ...) {
  # check_string(sep)
  # check_bool(reverse)
  start <- end <- .level <- NULL
  force_all(sep, reverse, mode, pad_date)
  dots <- list(...)
  call <- current_call()
  key_simple <- legendry::key_range_auto(sep = sep)
  fun <- function(scale, aesthetic = NULL) {
    trans <- scale$get_transformation()
    # TODO: Mode exact
    # scale::get_labels(breaks = ...) um die Labels Funktion zu benutzen
    # Mode Simple
    key <- key_simple(scale, aesthetic)
    res <- ggplot2::resolution(key$start)
    key <- key |>
      dplyr::group_by(.level) |>
      dplyr::mutate(
        diff = ifelse(.level != 0, start - lag(end, default = start[1]), 0),
        diff = ifelse(n() > 1, c(diff[2], diff[2:n()]), res), # drop leading 0
        start = start - diff * pad_date, # group_by, arrange, lag, diff, *pad_date
        end = end + lead(diff, default = diff[n()]) * pad_date,
        diff = NULL
      ) |>
      dplyr::ungroup() |>
      # Date scales need transformation, also works for other continuous transformations
      dplyr::mutate(start = trans$inverse(start), end = trans$inverse(end))

    # tibble gives a missing column warning
    class(key) <- c("key_range", "key_guide", "data.frame")
    return(key)
  }

  class(fun) <- union("key_range_auto_function", class(fun))
  fun
}
