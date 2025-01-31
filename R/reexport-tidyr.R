#' Duplicate rows according to a weighting variable
#'
#' @description
#' `uncount()` is provided by the tidyr package, and re-exported
#' by ggsurveillance. See [tidyr::uncount()] for more details.
#'
#' `uncount()` and its alias `expand_counts()` are complements of [dplyr::count()]: they take
#' a data frame with a column of frequencies and duplicate each row according to
#' those frequencies.
#'
#' @usage uncount(data, weights, ..., .remove = TRUE, .id = NULL)
#'
#' @inheritParams tidyr::uncount
#'
#' @return A `data.frame` with rows duplicated according to weights.
#' @examples
#' df <- data.frame(x = c("a", "b"), n = c(2, 3))
#' df |> uncount(n)
#' # Or equivalently:
#' df |> expand_counts(n)
#'
#' @importFrom tidyr uncount
#' @export
#' @name uncount
#' @aliases expand_counts
uncount

#' @rdname uncount
#' @export
expand_counts <- uncount
