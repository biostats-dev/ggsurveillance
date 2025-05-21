label_power10 <- function(decimal.mark = NULL, digits = 3, scale = 1, prefix = "", suffix = "", ...) {
  force_all(decimal.mark, digits, scale, prefix, suffix, ...)

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

    # Formatting depending on exponent
    dplyr::case_when(
      expn == 0 ~ mant,
      expn == 1 ~ paste0(mant, " %*% 10"),
      TRUE ~ paste0(mant, " %*% 10^", expn)
    ) |>
      # Wrap it in pre and suffix
      vapply(FUN = wrap, FUN.VALUE = character(1)) |>
      # Convert to expression
      parse(text = _)
  }
}

# label_power10(suffix = "kg", decimal.mark = ".")(523)

# label_power10(decimal.mark = ".")(c(52, 10))


# plot(c(1), xlab = label_power10(suffix = "kg", prefix = "test ", decimal.mark = ".", digits = 4)(0.5234))
# options(OutDec = ".")
