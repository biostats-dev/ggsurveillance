#' Skip labels on an axis
#'
#' @description
#' Creates a labeller function that removes every n-th label on an `ggplot2` axis.
#' Useful for reducing overlapping labels while keeping the major ticks.
#'
#' @param n Integer. Display every nth label. Default is `2`.
#' @param start Where to start the pattern. Either `"left"` for first tick (default),
#' `"right"` for last tick, or an integer position (i.e. `1` for first tick, `2` for second tick, etc.).
#' @param labeller Optional function to transform labels before applying skip pattern.
#' For example [label_date()]. For more complex labeller combinations use [scales::compose_label()].
#'
#' @return A function that takes a vector of labels and returns a vector with
#'   skipped labels replaced by empty strings.
#'
#' @examples
#' library(ggplot2)
#' # Default skip labels
#' ggplot(mtcars, aes(x = mpg, y = wt)) +
#'   geom_point() +
#'   scale_x_continuous(labels = label_skip())
#'
#' # Skip date labels, while keep ticks
#' ggplot(economics, aes(x = date, y = unemploy)) +
#'   geom_line() +
#'   scale_x_date(
#'     date_breaks = "2 years",
#'     labels = label_skip(start = "right", labeller = label_date(format = "%Y"))
#'   ) +
#'   theme_bw()
#'
#' @export

label_skip <- function(n = 2, start = "left", labeller = NULL) {
  force_all(n, start, labeller)

  # Validate inputs
  n <- as.integer(n) # Only integer
  if (n < 1) cli::cli_abort("'n' must be a positive integer")

  # Check type of start
  if (is.character(start)) {
    start <- match.arg(start, c("left", "right"))
  } else if (is.numeric(start)) {
    # convert start to be an integer between 0 and n-1
    start <- as.integer(start) %% n
  } else {
    cli::cli_abort("'start' must be either 'left', 'right', or an integer.")
  }

  if (!is.null(labeller)) {
    # if labels are a list of functions compose, else treat them as labels
    if (
      any(sapply(c(labeller), is.function)) ||
        all(sapply(as.character(labeller), exists, mode = "function"))) {
      # TODO: remove identity function
      labeller <- do.call(scales::compose_label, c(identity, labeller))
    } else {
      cli::cli_abort("label_skip(): {labeller} not a function.")
    }
  }

  function(x) {
    # Apply other labeller
    if (!is.null(labeller) && is.function(labeller)) {
      x <- labeller(x)
    }

    # Find non-NA positions
    # ggplot2 sometimes passes NA values in x, which are not shown on the scale
    non_na <- which(!is.na(x))

    # Convert left and right to integer start values
    if (start == "left") start <- 1
    # Right can only be caluclated if length of x is known
    if (start == "right") start <- (length(non_na)) %% n

    # Apply skip pattern only to non-NA positions
    show_positions <- seq_along(non_na) %% n == start
    # The documentation states every nth label is shown. Just to be technically correct.
    if (n == 1) show_positions <- !show_positions

    # Create boolean vector of the length of x
    show_label <- logical(length(x)) # FALSE
    show_label[non_na] <- show_positions

    # Replace skipped labels with empty string, preserving NA
    x[!show_label & !is.na(x)] <- ""

    return(x)
  }
}
