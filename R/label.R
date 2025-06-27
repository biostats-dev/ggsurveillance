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

    sign <- ifelse(substr(mant,1,1) == "-", "-", "")

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
