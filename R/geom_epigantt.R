#' Epi Gantt Chart: Visualize Epidemiological Time Intervals
#'
#' @description
#' Creates Epi Gantt charts, which are specialized timeline visualizations used in
#' outbreak investigations to track potential exposure periods and identify transmission
#' patterns. They are particularly useful for:
#'
#' * Hospital outbreak investigations to visualize patient movements between wards
#' * Identifying potential transmission events by showing when cases were in the same location
#' * Visualizing common exposure times using overlapping exposure time intervals
#'
#' The chart displays time intervals as horizontal bars, typically with one row per case/patient.
#' Different colors can be used to represent different locations (e.g., hospital wards) or
#' exposure types. Additional points or markers can show important events like symptom onset
#' or test dates.
#'
#' `geom_epigantt()` will adjust the linewidth depending on the number of cases.
#'
#' @param stat A `ggplot2` stat. Defaults to `"identity"`.
#' @param position A `ggplot2` position. Defaults to `"identity"`.
#' @param mapping Set of aesthetic mappings. Must include:
#'   * `y`: Case/patient identifier
#'   * `xmin`: Start date/time of interval
#'   * `xmax`: End date/time of interval
#'   * Optional: `color` or `fill` for different locations/categories
#' @inheritParams ggplot2::geom_linerange
#' @param ... Additional parameters:
#'   * `linewidth`: Set width of bars directly, disables auto-scaling if set.
#'   * `lw_scaling_factor`: Scaling factor for auto-width calculation.
#'    Linewidth is calculated as lw_scaling_factor/number_of_rows (default: 90)
#'   * `lw_min`: Minimum auto-scaled line width cutoff (default: 1)
#'   * `lw_max`: Maximum auto-scaled line width cutoff (default: 8)
#' @return A `ggplot2` geom layer that can be added to a plot
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#'
#' # Transform hospital outbreak line list to long format
#' linelist_hospital_outbreak |>
#'   pivot_longer(
#'     cols = starts_with("ward"),
#'     names_to = c(".value", "num"),
#'     names_pattern = "ward_(name|start_of_stay|end_of_stay)_([0-9]+)",
#'     values_drop_na = TRUE
#'   ) -> df_stays_long
#'
#' linelist_hospital_outbreak |>
#'   pivot_longer(cols = starts_with("pathogen"), values_to = "date") -> df_detections_long
#'
#' # Create Epi Gantt chart showing ward stays and test dates
#' ggplot(df_stays_long) +
#'   geom_epigantt(aes(y = Patient, xmin = start_of_stay, xmax = end_of_stay, color = name)) +
#'   geom_point(aes(y = Patient, x = date, shape = "Date of pathogen detection"),
#'     data = df_detections_long
#'   ) +
#'   scale_y_discrete_reverse() +
#'   theme_bw() +
#'   theme(legend.position = "bottom")
#'
#' @export
geom_epigantt <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomEpigantt,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @import ggplot2
GeomEpigantt <- ggproto("GeomEpigantt", GeomLinerange,
  default_aes = ggplot2:::defaults(
    # linewidth = from_theme(borderwidth)
    aes(
      colour = "dodgerblue4",
      # linewidth = 8, # Will be auto-adjusted based on the number of cases
      linetype = "solid"
    ),
    GeomLinerange$default_aes
  ),
  extra_params = c(GeomLinerange$extra_params, "lw_min", "lw_max", "lw_scaling_factor"),
  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes

    # TODO: warn if to many cases.
    data$linewidth <- data$linewidth %||% params$linewidth %||% .calc_linewidth(
      data, params,
      min = params$lw_min %||% 1, max = params$lw_max %||% 8,
      scaling_factor = params$lw_scaling_factor %||% 90
    )
    data
  },
)

.calc_linewidth <- function(data, params, max = 8, min = 1, scaling_factor = 90) {
  if (params$flipped_aes) n_obs <- dplyr::n_distinct(data$y) else n_obs <- dplyr::n_distinct(data$x)

  # Should scaling_factor be adjustable by user?
  linewidth <- scaling_factor / n_obs

  # return linewidth if between min and max, else cutoff at min or max
  return(pmin(pmax(min, linewidth), max))
}
