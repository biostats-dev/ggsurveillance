#' Format numbers as power-of-10 R expressions
#'
#' @description
#' Creates a labeller function that formats numbers in scientific notation using
#' power-of-10 R expressions (e.g., \eqn{2.5\times 10^3} or \eqn{5\times 10^6}). Useful for axis
#' labels in ggplot2 when dealing with large numbers or when you want to emphasize
#' the order of magnitude.
#'
#' @param decimal.mark Character used as decimal separator. If `NULL` (default),
#'   retrieves the setting from [[scales::number_options()]].
#' @param digits Number of significant digits to show in the mantissa.
#' @param scale Scaling factor multiplied to the input values. Default is `1`.
#' @param prefix Character string to prepend to each label. Default is `""`.
#' @param suffix Character string to append to each label. Default is `""`.
#' @param magnitude_only Logical. If `TRUE`, shows only the power-of-10 part
#'   (e.g., \eqn{10^5} instead of \eqn{1\times 10^5}). Default is `FALSE`.
#' @param ... Additional arguments passed to [`scales::scientific()`].
#'
#' @return A label function that takes a numeric vector and returns an expression
#'   vector suitable for use as axis labels in ggplot2.
#'
#' @details
#' The function converts numbers to scientific notation and then formats them
#' as mathematical expressions using the R expression syntax:
#'
#' - For exponent 0: returns the mantissa as-is (e.g., \eqn{5.5})
#' - For exponent 1: it omits the exponent (e.g., \eqn{1.5\times 10})
#' - For other exponents: everything is shown (e.g., \eqn{1.5\times 10^3})
#'
#' When `magnitude_only = TRUE`:
#' - For exponent 0: returns \eqn{1}
#' - For exponent 1: returns \eqn{10}
#' - For other exponents (positive or negative): returns \eqn{10^{exponent}}
#'
#' The function handles negative numbers by preserving the sign and supports
#' custom decimal marks, prefixes, and suffixes.
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic usage with default settings
#' label_power10()(c(1000, 10000, 100000, -1000))
#'
#' # Use in ggplot2
#' ggplot(
#'   data.frame(x = 1:5, y = c(1, 50000, 75000, 100000, 200000)),
#'   aes(x, y)
#' ) +
#'   geom_point() +
#'   scale_y_continuous(labels = label_power10())
#'
#' # Use in ggplot2 with options
#' ggplot(
#'   data.frame(x = 1:5, y = c(1, 50000, 75000, 100000, 200000)),
#'   aes(x, y)
#' ) +
#'   geom_point() +
#'   scale_y_continuous(labels = label_power10(decimal.mark = ",", digits = 2, suffix = " CFU"))
#'
#' # Magnitude only for cleaner labels with log scales
#' ggplot(
#'   data.frame(x = 1:5, y = c(1000, 10000, 100000, 1000000, 10000000)),
#'   aes(x, y)
#' ) +
#'   geom_point() +
#'   scale_y_log10(labels = label_power10(magnitude_only = TRUE))
#'
#' @export
label_power10 <- function(decimal.mark = NULL, digits = 3, scale = 1, prefix = "", suffix = "", magnitude_only = FALSE, ...) {
  force_all(decimal.mark, digits, scale, prefix, suffix, magnitude_only, ...)

  function(x) {
    decimal.mark <- decimal.mark %||% getOption("scales.decimal.mark", default = ".")

    # Convert into scientific notation, split by 'e'
    parts <- scales::scientific(x, decimal.mark = decimal.mark, digits = digits, scale = scale, ...) |>
      stringr::str_split_fixed("e", n = 2)

    mant <- parts[, 1]
    expn <- as.integer(parts[, 2])

    # Escape non . decimal marks like ,
    if (decimal.mark != ".") {
      mant <- paste0('paste("', mant, '")')
    }

    # 3) Check if prefix or suffix are empty strings, if not escape text
    pre_txt <- if (nzchar(prefix)) paste0('"', prefix, '"') else NULL
    suf_txt <- if (nzchar(suffix)) paste0('"', suffix, '"') else NULL

    # Wrap numbers in paste(prefix, core, suffix) if needed
    wrap <- function(core) {
      bits <- c(pre_txt, core, suf_txt)
      if (length(bits) > 1) {
        paste0("paste(", paste(bits, collapse = ", "), ")")
      } else {
        core
      }
    }

    sign <- ifelse(substr(mant, 1, 1) == "-", "-", "")

    # Formatting depending on exponent
    dplyr::case_when(
      magnitude_only & expn == 0 ~ paste0(sign, "1"),
      expn == 0 ~ mant,
      magnitude_only & expn == 1 ~ paste0(sign, "10"),
      expn == 1 ~ paste0(mant, " %*% 10"),
      magnitude_only & (expn > 1 | expn < 0) ~ paste0(sign, "10^", expn),
      TRUE ~ paste0(mant, " %*% 10^", expn)
    ) |>
      # Wrap it in pre and suffix
      vapply(FUN = wrap, FUN.VALUE = character(1)) |>
      # Convert to expression
      parse(text = _)
  }
}


# Example with magnitude_only = TRUE to show only order of magnitude
# label_power10(magnitude_only = TRUE)(100000)  # Will show 10^5
# label_power10(magnitude_only = FALSE)(100000) # Will show 1 × 10^5 (default)
# label_power10(magnitude_only = TRUE)(250000)  # Will show 10^5 (same magnitude)

# plot(c(1), xlab = label_power10(suffix = "kg", prefix = "test ", decimal.mark = ".", digits = 4)(0.5234))
# options(OutDec = ".")
