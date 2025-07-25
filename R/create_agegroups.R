#' Create Age Groups from Numeric Values
#'
#' @description
#' Creates age groups from numeric values using customizable break points and formatting options.
#' The function allows for flexible formatting and customization of age group labels.
#'
#' If a factor is returned, this factor includes factor levels of unobserved age groups.
#' This allows for reproducible age groups, which can be used for joining data
#' (e.g. adding age grouped population numbers for incidence calculation).
#'
#' @param values Numeric vector of ages to be grouped
#' @param age_breaks Numeric vector of break points for age groups. \cr
#' Default: \code{c(5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90)}
#' @param breaks_as_lower_bound Logical; if \code{TRUE} (default), breaks define the the lower bounds of the intervals (e.g., a break at 5 starts the '5-9' group).
#' If \code{FALSE}, breaks define the upper bound (e.g., a break at 5 ends the '0-5' group).
#' @param first_group_format Character string template for the first age group. Uses [glue::glue] syntax. \cr
#' The variable `x` represents the upper bound of the first interval.\cr
#' Default: \code{"0-{x}"}. Other common styles: \code{"<={x}", "<{x+1}"}
#' @param interval_format Character string template for intermediate age groups. Uses [glue::glue] syntax.\cr
#' The variables `x` and `y` represent the lower and upper bounds of the interval, respectively.\cr
#' Default: \code{"{x}-{y}"}. Other common styles: \code{"{x} to {y}"}
#' @param last_group_format Character string template for the last age group. Uses [glue::glue] syntax. \cr
#' The variable `x` represents the lower bound of the last interval.\cr
#' Default: \code{"{x}+"}. Other common styles: \code{">={x}",">{x-1}"}
#' @param pad_numbers Logical or numeric; if numeric, pad numbers up to the specified length (Tip: use \code{2}).
#' Not compatible with calculations within glue formats. Default: \code{FALSE}
#' @param pad_with Character to use for padding numbers. Default: \code{"0"}
#' @param collapse_single_year_groups Logical; if \code{TRUE}, groups spanning a single year (e.g., from `age_breaks = c(1, 2)`)
#' are formatted as a single number (e.g., "1") instead of a range (e.g., "1-1"). Default: \code{FALSE}
#' @param na_label Label for \code{NA} values. If \code{NA}, keeps default \code{NA} handling. Default: \code{NA}
#' @param return_factor Logical; if \code{TRUE}, returns a factor, if \code{FALSE} returns character vector. Default: \code{FALSE}
#'
#' @return Vector of age group labels (character or factor depending on return_factor)
#'
#' @examples
#' # Basic usage
#' create_agegroups(1:100)
#'
#' # Custom formatting with upper bounds
#' create_agegroups(1:100,
#'   breaks_as_lower_bound = FALSE,
#'   interval_format = "{x} to {y}",
#'   first_group_format = "0 to {x}"
#' )
#'
#' # Ages 1 to 5 are kept as numbers by collapsing single year groups
#' create_agegroups(1:10,
#'   age_breaks = c(1, 2, 3, 4, 5, 10),
#'   collapse_single_year_groups = TRUE
#' )
#'
#' @import glue
#' @import cli
#' @import dplyr
#' @import forcats
#' @importFrom stringr str_pad
#' @export
create_agegroups <- function(
    values,
    age_breaks = c(5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90),
    breaks_as_lower_bound = TRUE,
    first_group_format = "0-{x}", # "≤{x}", "<{x+1}"
    interval_format = "{x}-{y}", # "{x} to {y}"
    last_group_format = "{x}+", # "≥{x}",">{x-1}"
    pad_numbers = FALSE, pad_with = "0",
    collapse_single_year_groups = FALSE,
    na_label = NA, return_factor = FALSE) {
  if (!is.numeric(age_breaks)) {
    cli::cli_abort("{.arg age_breaks} must be numeric.")
  }

  values <- as.numeric(values)
  age_breaks <- sort(floor(age_breaks))
  breaks <- c(-Inf, age_breaks, Inf)

  write_labels <- function(lb, x, y = NA, pn = pad_numbers, pad = pad_with) {
    if (pn) {
      # Pad Numbers with 0. Breaks glue {y+1}
      x <- stringr::str_pad(x, as.numeric(pn), pad = pad)
      y <- stringr::str_pad(y, as.numeric(pn), pad = pad)
    }
    glue::glue(lb)
  }

  if (breaks_as_lower_bound) {
    corr_up <- -1
    corr_low <- 0
  } else {
    corr_up <- 0
    corr_low <- 1
  }

  labels <- c(
    # First Group
    write_labels(first_group_format, age_breaks[1] + corr_up),
    # Mid group when more than 1 breaks was supplied
    if (length(age_breaks) > 1) {
      dplyr::case_when(
        # Collapse groups if specified
        (collapse_single_year_groups &
          (age_breaks[1:(length(age_breaks) - 1)] + corr_low == age_breaks[2:length(age_breaks)] + corr_up)
        ) ~ as.character(age_breaks[1:(length(age_breaks) - 1)] + corr_low),
        # Default
        TRUE ~ write_labels(
          interval_format,
          age_breaks[1:(length(age_breaks) - 1)] + corr_low,
          age_breaks[2:length(age_breaks)] + corr_up
        ),
      )
    },
    # Last Group
    write_labels(last_group_format, age_breaks[length(age_breaks)] + corr_low)
  )

  agegroups <- cut(
    values,
    breaks = breaks,
    right = !breaks_as_lower_bound,
    labels = labels
  )

  # Explicit NA if specified
  if (!is.na(na_label)) {
    agegroups <- forcats::fct_na_value_to_level(agegroups, as.character(na_label))
  }

  if (!return_factor) {
    agegroups <- as.character(agegroups, na_label)
  }

  return(agegroups)
}
