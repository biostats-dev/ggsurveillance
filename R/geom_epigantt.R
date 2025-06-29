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
#' Different colours can be used to represent different locations (e.g., hospital wards) or
#' exposure types. Additional points or markers can show important events like symptom onset
#' or test dates.
#'
#' @param stat A `ggplot2` stat. Defaults to `"identity"`.
#' @param position A `ggplot2` position. Defaults to `"identity"`.
#' @param mapping Set of aesthetic mappings. Must include:
#'   * `y`: Case/patient identifier
#'   * `xmin`: Start date/time of interval
#'   * `xmax`: End date/time of interval
#'   * Optional: `colour` or `fill` for different locations/categories
#' @inheritParams geom_col_range
#' @param ... Additional parameters:
#'   * `width`: Set width of bars.
#' @return A `ggplot2` geom layer that can be added to a plot
#' @seealso [theme_mod_legend_bottom()]
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
#'   geom_epigantt(aes(y = Patient, xmin = start_of_stay, xmax = end_of_stay, fill = name)) +
#'   geom_point(aes(y = Patient, x = date, shape = "Date of pathogen detection"),
#'     data = df_detections_long
#'   ) +
#'   scale_y_discrete_reverse() +
#'   theme_bw() +
#'   theme_mod_legend_bottom()
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
GeomEpigantt <- ggplot2::ggproto("GeomEpigantt", GeomBarRange,
  default_aes = ggplot2:::defaults(
    aes(
      fill = "dodgerblue4",
      colour = NA,
      alpha = 1
    ),
    GeomBarRange$default_aes
  ),
  extra_params = c(GeomBarRange$extra_params),
  setup_data = function(data, params) {
    # Call parent setup_data first to handle flipped aesthetics and bar positioning
    data <- GeomBarRange$setup_data(data, params)
    
    data
  },
)

.calc_linewidth <- function(data, flipped_aes, max = 8, min = 1, scaling_factor = 90) {
  if (flipped_aes) n_obs <- dplyr::n_distinct(data$y) else n_obs <- dplyr::n_distinct(data$x)

  # scaling_factor is adjustable by the user
  linewidth <- scaling_factor / n_obs

  # return linewidth if between min and max, else cutoff at min or max
  return(pmin(pmax(min, linewidth), max))
}
