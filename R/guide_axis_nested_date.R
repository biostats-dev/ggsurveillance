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


guide_axis_nested_date <- function(
    # key   = "range_auto",
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
