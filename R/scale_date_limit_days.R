#' Limit the x-axis date range to a specific number of days (Pre-Release)
#'
#' This function creates a date scale for ggplot2 that limits the x-axis to show
#' only a specific number of days, with options to control the reference date and
#' upper bound behaviour.
#' This is helpful to align the scale of multiple time series of different lengths and to limit the shown data
#' to the most recent observations.
#'
#' @param name The name of the scale - used as the axis label.
#' @param limit_scale_to_days The number of days to display on the x-axis. Default is 549 days (approximately 1.5 years).
#' @param reference_date A reference date to calculate the lower limit (i.e. reference_date - limit_days).
#' Useful to align multiple plots either at the current date or if the datasets have different ranges of data
#' (e.g. different dates for the last observation).
#' @param limit_right Logical. If TRUE, the upper limit will be exactly the reference date.
#'   If FALSE (default), the upper limit will be the maximum date in the data.
#' Warning: Can result in loss of labels or text on the right side of the last observation.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{scale_x_date}}
#' or \code{\link[ggplot2]{scale_x_datetime}}.
#' @param expand A numeric vector of length two giving multiplicative and additive expansion constants.
#' @param position The position of the axis. One of "top", "bottom" (default), "left", or "right".
#'
#' @return A ggplot2 scale object.
#' @noRd
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(lubridate)
#'
#' # Sample data
#' df <- data.frame(
#'   date = seq(as_date("2020-01-01"), as_date("2022-01-01"), by = "day"),
#'   value = runif(732)
#' )
#'
#' # Basic usage - shows last 549 days
#' ggplot(df, aes(x = date, y = value)) +
#'   geom_line() +
#'   scale_x_date_limit_days()
#'
#' # With reference date
#' ggplot(df, aes(x = date, y = value)) +
#'   geom_line() +
#'   scale_x_date_limit_days(reference_date = "2021-12-01")
#'
#' # With reference date as strict upper limit
#' ggplot(df, aes(x = date, y = value)) +
#'   geom_line() +
#'   scale_x_date_limit_days(reference_date = "2021-12-01", limit_right = TRUE)
#' }
scale_x_date_limit_days <- function(name = waiver(),
                                    limit_scale_to_days = 549, reference_date = NULL, limit_right = FALSE,
                                    ...,
                                    expand = waiver(), position = "bottom") {
  # If reference date is supplied, try to convert to date. Date should stay date
  if (!is.null(reference_date)) {
    reference_date <- tryCatch(
      lubridate::as_date(reference_date),
      error = function(e) NA
    )
    # give warning if after conversion if NA
    if (is.na(reference_date)) {
      warning("reference_date is not a known format. Will be ignored.")
      reference_date <- NULL
    }
  }

  # Partial Application of the limit function
  limit_days_by_range <- function(limit_scale_to_days, reference_date, limit_right) {
    function(range) {
      if (!is_empty(reference_date) & is.Date(reference_date)) {
        upper_limit <- reference_date
      } else {
        upper_limit <- range[2]
      }
      # TODO: floor_date?
      if (!limit_right) {
        return(c(upper_limit - lubridate::days(limit_scale_to_days), range[2]))
      } else {
        return(c(upper_limit - lubridate::days(limit_scale_to_days), upper_limit))
      }
    }
  }

  ggplot2::scale_x_date(
    name = name,
    limits = limit_days_by_range(limit_scale_to_days, reference_date, limit_right),
    ...,
    expand = expand,
    position = position
  )
}

# @rdname scale_x_date_limit_days
scale_x_date_limit_1.5years <- function(limit_scale_to_days = 549, ...) {
  scale_x_date_limit_days(limit_scale_to_days = limit_scale_to_days, ...)
}

# @rdname scale_x_date_limit_days
scale_x_datetime_limit_days <- function(name = waiver(),
                                        limit_scale_to_days = 549, reference_date = NULL, limit_right = FALSE,
                                        ...,
                                        expand = waiver(), position = "bottom") {
  # If reference date is supplied, try to convert to date. Date should stay date
  if (!is.null(reference_date)) {
    reference_date <- tryCatch(
      # as_datetime()?
      as.POSIXct(reference_date),
      error = function(e) NA
    )
    # give warning if after conversion if NA
    if (is.na(reference_date)) {
      warning("reference_date is not a known format. Will be ignored.")
      reference_date <- NULL
    }
  }

  # Partial Application of the limit function
  limit_days_by_range <- function(limit_scale_to_days, reference_date, limit_right) {
    function(range) {
      if (!is_empty(reference_date) & is.POSIXt(reference_date)) {
        upper_limit <- reference_date
      } else {
        upper_limit <- range[2]
        # lubridate/scales fix
        class(upper_limit) <- c("POSIXct", "POSIXt")
      }
      if (!limit_right) {
        return(c(upper_limit - lubridate::days(limit_scale_to_days), range[2]))
      } else {
        return(c(upper_limit - lubridate::days(limit_scale_to_days), upper_limit))
      }
    }
  }

  ggplot2::scale_x_datetime(
    name = name,
    limits = limit_days_by_range(limit_scale_to_days, reference_date, limit_right),
    ...,
    expand = expand,
    position = position
  )
}
