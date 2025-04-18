label_skip <- function(n = 2, start = "left", labeller = NULL) {
  force(n)
  force(start)

  # Validate inputs
  n <- as.integer(n) # Only integer
  if (n < 1) stop("'n' must be a positive integer")

  # Check type of start
  if (is.character(start)) {
    start <- match.arg(start, c("left", "right"))
  } else if (is.numeric(start)) {
    # convert start to be an integer between 0 and n-1
    start <- as.integer(start) %% n
  } else {
    cli::cli_abort("'start' must be either 'left', 'right', or an integer.")
  }

  function(x) {
    # Apply other labeller
    if (!is.null(labeller) && is.function(labeller)) {
      x <- labeller(x)
    }

    #   function(x) {
    # # Apply the labeller function first if provided
    # if (!is.null(labeller)) {
    #   if (is.function(labeller)) {
    #     x <- labeller(x)
    #   } else if (inherits(labeller, "formula")) {
    #     # Convert formula to function and apply
    #     fun <- scales::as_labeller(labeller)
    #     x <- fun(x)
    #   }
    # }

    # Find non-NA positions
    # ggplot2 sometimes passes NA values in x, which are not shown on the scale
    non_na <- which(!is.na(x))

    # Convert left and right to integer start values
    if (start == "left") start <- 1
    # Right can only be caluclated if length of x is known
    if (start == "right") start <- (length(non_na)) %% n

    # Apply skip pattern only to non-NA positions
    show_positions <- seq_along(non_na) %% n == start

    # Create boolean vector of the length of x
    show_label <- logical(length(x)) # FALSE
    show_label[non_na] <- show_positions

    # Replace skipped labels with empty string, preserving NA
    x[!show_label & !is.na(x)] <- ""

    return(x)
  }
}
