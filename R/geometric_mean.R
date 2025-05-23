#' Compute a Geometric Mean
#'
#' The geometric mean is typically defined for strictly positive values.
#' This function computes the geometric mean of a numeric vector,
#' with the option to replace certain values (e.g., zeros, non-positive values,
#' or values below a user-specified threshold) before computation.
#'
#' @param x A numeric or complex vector of values.
#' @param na.rm Logical. If \code{FALSE} (default), the presence of zero or
#'   negative values triggers a warning and returns \code{NA}. If \code{TRUE},
#'   such values (and any \code{NA}) are removed before computing the geometric mean.
#' @param replace_value Numeric or \code{NULL}. The value used for replacement,
#'   depending on \code{replace} (e.g., a detection limit (LOD) or quantification limit (LOQ)).
#'   If \code{NULL}, no replacement is performed. For recommendations how to use, see details.
#' @param replace Character string indicating which values to replace:
#'   \describe{
#'     \item{\code{"all"}}{Replaces all values less than \code{replace_value}
#'       with \code{replace_value}. This is useful if you have a global threshold
#'       (such as a limit of detection) below which any measurement is replaced.}
#'     \item{\code{"non-positive"}}{Replaces all non-positive values (\code{x <= 0})
#'       with \code{replace_value}. This is helpful if zeros or negative values
#'       are known to be invalid or below a certain limit.}
#'     \item{\code{"zero"}}{Replaces only exact zeros (\code{x == 0}) with
#'       \code{replace_value}. Useful if negative values should be treated as missing.}
#'   }
#' @param warning Disable warnings by setting it to `FALSE`. Defaults to `TRUE`.
#'
#' @details
#' \strong{Replacement Considerations}:
#' The geometric mean is only defined for strictly positive numbers (\eqn{x > 0}).
#' Despite this, the geometric mean can be useful for laboratory measurements which can contain 0 or negative values.
#' If these values are treated as NA and are removed, this results in an upward bias due to missingness.
#' To reduce this, values below the limit of detection (LOD) or limit of quantification (LOQ)
#' are often replaced with the chosen limit, making this limit the practical lower limit of the measurement scale.
#' This is therefore an often recommended approach.
#'
#' There are also alternatives approaches, where values are replaced by
#' either \eqn{\frac{LOD}{2}} or \eqn{\frac{LOD}{\sqrt{2}}} (or LOQ). These approaches create a gap in the distribution
#' of values (e.g. no values for \eqn{\frac{LOD}{2} < x < LOD}) and should therefore be used with caution.
#'
#' \strong{If the replacement approach for values below LOD or LOQ has a material effect on the
#' interpretation of the results, the values should be treated as statistically censored. In this case,
#' proper statistical methods to handle (left) censored data should be used.}
#'
#' When \code{replace_value} is provided, the function will \emph{first} perform
#' the specified replacements, then proceed with the geometric mean calculation.
#' If no replacements are requested but zero or negative values remain and
#' \code{na.rm = FALSE}, an \code{NA} will be returned with a warning.
#'
#' @return A single numeric value representing the geometric mean of the
#'   processed vector \code{x}, or \code{NA} if the resulting vector is empty
#'   (e.g., if \code{na.rm = TRUE} removes all positive values) or if non-positive
#'   values exist when \code{na.rm = FALSE}.
#'
#' @examples
#' # Basic usage with no replacements:
#' x <- c(1, 2, 3, 4, 5)
#' geometric_mean(x)
#'
#' # Replace all values < 0.5 with 0.5 (common in LOD scenarios):
#' x3 <- c(0.1, 0.2, 0.4, 1, 5)
#' geometric_mean(x3, replace_value = 0.5, replace = "all")
#'
#' # Remove zero or negative values, since log(0) = -Inf and log(-1) = NaN
#' x4 <- c(-1, 0, 1, 2, 3)
#' geometric_mean(x4, na.rm = TRUE)
#'
#' @export
geometric_mean <- function(x, na.rm = FALSE, replace_value = NULL, replace = c("all", "non-positive", "zero"),
                           warning = TRUE) {
  if (!is.numeric(x) && !is.complex(x) & warning) {
    cli::cli_warn("argument is not numeric: returning NA")
    return(NA_real_)
  }

  replace <- rlang::arg_match(replace)
  # Define the replacement function based on replace
  switch(replace,
    "all" = f_replace <- \(x) (x < replace_value),
    "non-positive" = f_replace <- \(x) (x <= 0),
    "zero" = f_replace <- \(x) (x == 0),
  )

  # If replace value, then print how many values were changed.
  if (!is.null(replace_value)) {
    n_replaced <- sum(f_replace(x), na.rm = TRUE)
    x[f_replace(x)] <- as.numeric(replace_value)
    if (n_replaced != 0 & warning) cli::cli_warn("{n_replaced} value{?s} were substituted with {as.numeric(replace_value)}.")
  }

  # Check if x is 0 or smaller
  if (any(x <= 0, na.rm = TRUE) & !na.rm & warning) {
    cli::cli_warn("Zero or negative values are treated as NA. Please use na.rm=TRUE to ignore them.")
    return(NA_real_)
  }

  # Remove NA und zero or negative values, if na.rm = TRUE
  if (na.rm) {
    x <- x[x > 0 & !is.na(x)]
  }

  if (length(x) == 0) {
    return(NA_real_)
  } # return NA instead of NaN for empty vectors

  # Finally: geometric mean
  gm <- exp(mean(log(x), na.rm = na.rm))
  return(gm)
}
