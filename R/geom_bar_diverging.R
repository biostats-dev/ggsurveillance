#' Create diverging bar charts, diverging area charts or other plots for opposing categorical data.
#'
#' @description
#' `geom_bar_diverging()` creates a diverging bar chart, i.e. stacked bars which are centered at 0.
#' This is useful for visualizing contrasting categories like:
#'  * case counts by contrasting categories like vaccination status or autochthonous vs imported infection
#'  * age pyramids,
#'  * likert scales for e.g. agreement (sentiment analysis)
#'  * or any data with natural opposing groups.
#'
#' `stat_diverging()` calculates the required statistics for diverging
#' charts and can be used with different geoms. Used for easy labeling of diverging charts.
#'
#' `geom_area_diverging()` creates a diverging area chart, for continuous data of opposing categories.
#' x (or y) has to be continuous for this geom.
#'
#' @section Diverging bar charts:
#' Diverging bar charts split categories into positive and negative directions based on
#' factor level order. Categories in the first half of factor levels go in the negative
#' direction, while categories in the second half go in the positive direction.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#' @param data The data to be displayed in this layer.
#' @param proportion Logical. If `TRUE`, each stacked bar are normalized to 100%.
#' Useful to plot or calculate the percentages of each category within each bar.
#' @param neutral_cat How to handle the middle category for a odd number of factor levels.
#'  * `"odd"`: If the number of factor levels is odd, the middle category is treated as neutral.
#'  * `"never"`: For odd factor levels, the middle categories is treated as positive.
#' @param geom `stat_diverging()`: The geometric object to use to display the data, e.g. `"text"` or `"label"`.
#' @param stacked Logical. If `TRUE`, categories are stacked.
#' @param totals_by_direction Logical. If `TRUE`, totals are calculated by direction.
#' I.e. the total for the positive, negative and, if existent, neutral category.
#' @param nudge_label_outward Numeric. Relative value to nudge labels outward from `0`. Try `0.05`.
#' Negative values nudge inward.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer}}.
#' @param position Position adjustment.
#' @inheritParams ggplot2::geom_bar
#'
#' @return A `ggplot2` geom layer that can be added to a plot.
#'
#' @section Aesthetics:
#' Required aesthetics:
#' * `x` or `y`
#' * `diverging_groups`: Will default to `fill` if missing. A factor should be used for this aesthetic for best results.
#' All factor levels defined will be used to determine positive, negative and neutral categories.
#' Behaviour of the diverging bar charts can therefore be controlled by creating empty dummy factor levels.
#'
#' Optional aesthetics:
#' * `weight`: Adjust the weight of observations. Can be used to pass case counts or incidences.
#'
#' @section Calculated stats:
#' * `count`
#' * `prop`: Proportion of the category within the stacked bar.
#' * `sign`: Direction of the category. Either `-1`, `0` or `+1`
#'
#' @examples
#' # Basic example with geom_bar_diverging
#' library(ggplot2)
#' library(dplyr)
#' library(tidyr)
#'
#' set.seed(123)
#' df_6cat <- data.frame(matrix(sample(1:6, 600, replace = TRUE), ncol = 6)) |>
#'   mutate_all(~ ordered(., labels = c("+++", "++", "+", "-", "--", "---"))) |>
#'   pivot_longer(cols = everything())
#'
#' ggplot(df_6cat, aes(y = name, fill = value)) +
#'   geom_bar_diverging() + # Bars
#'   stat_diverging() + # Labels
#'   scale_x_continuous_diverging() + # Scale
#'   theme_classic()
#'
#' ggplot(df_6cat, aes(y = name, fill = value)) +
#'   geom_bar_diverging() + # Bars
#'   stat_diverging(totals_by_direction = TRUE, nudge_label_outward = 0.05) + # Totals as Label
#'   scale_x_continuous_diverging() + # Scale
#'   theme_classic()
#'
#' # Age pyramid
#' population_german_states |>
#'   filter(state %in% c("Berlin", "Mecklenburg-Vorpommern"), age < 90) |>
#'   ggplot(aes(y = age, fill = sex, weight = n)) +
#'   geom_bar_diverging(width = 1) +
#'   geom_vline(xintercept = 0) +
#'   scale_x_continuous_diverging(n.breaks = 10) +
#'   facet_wrap(~state, scales = "free_x") +
#'   theme_bw()
#'
#' # Vaccination status for dummy factor level
#' set.seed(456)
#' cases_vacc <- data.frame(year = 2017:2025) |>
#'   rowwise() |>
#'   mutate(vacc = list(sample(1:4, 100, prob = (4:1)^(1 - 0.2 * (year - 2017)), replace = TRUE))) |>
#'   unnest(vacc) |>
#'   mutate(
#'     year = as.factor(year),
#'     "Vaccination Status" = ordered(vacc,
#'       levels = c(1:3, "dummy", 4),
#'       labels = c("Fully Vaccinated", "Partially Vaccinated", "Unknown", "dummy", "Unvaccinated")
#'     )
#'   )
#'
#' ggplot(cases_vacc, aes(y = year, fill = `Vaccination Status`)) +
#'   geom_vline(xintercept = 0) +
#'   geom_bar_diverging(proportion = TRUE) +
#'   stat_diverging(
#'     size = 3, proportion = TRUE,
#'     totals_by_direction = TRUE, nudge_label_outward = 0.05
#'   ) +
#'   scale_x_continuous_diverging(labels = scales::label_percent(), n.breaks = 10) +
#'   scale_y_discrete_reverse() +
#'   ggtitle("Proportion of vaccinated cases by year") +
#'   theme_classic() +
#'   theme_mod_legend_bottom()
#'
#' @name geom_bar_diverging
#' @export
geom_bar_diverging <- function(mapping = NULL, data = NULL, position = "identity",
                               proportion = FALSE, neutral_cat = c("odd", "never"),
                               # break_cat = NULL, # odd, never, always
                               ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  neutral_cat <- rlang::arg_match(neutral_cat)

  ggplot2::layer(
    geom = GeomBarRange,
    mapping = mapping,
    data = data,
    stat = "diverging",
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      neutral_cat = neutral_cat,
      stacked = TRUE,
      proportion = proportion,
      totals_by_direction = FALSE,
      nudge_label_outward = 0,
      ...
    )
  )
}

#' @rdname geom_bar_diverging
#' @export
geom_area_diverging <- function(mapping = NULL, data = NULL, position = "identity",
                                proportion = FALSE, neutral_cat = c("odd", "never"),
                                # break_cat = NULL, # odd, never, always
                                ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  neutral_cat <- rlang::arg_match(neutral_cat)

  ggplot2::layer(
    geom = GeomAreaDiverging,
    mapping = mapping,
    data = data,
    stat = "diverging",
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      neutral_cat = neutral_cat,
      stacked = TRUE,
      proportion = proportion,
      totals_by_direction = FALSE,
      nudge_label_outward = 0,
      ...
    )
  )
}

#' @rdname geom_bar_diverging
#' @export
stat_diverging <- function(mapping = NULL, data = NULL,
                           geom = "text", position = "identity",
                           stacked = TRUE, proportion = FALSE,
                           neutral_cat = c("odd", "never"),
                           totals_by_direction = FALSE, nudge_label_outward = 0,
                           ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  neutral_cat <- rlang::arg_match(neutral_cat)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatDiverging,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      stacked = stacked,
      neutral_cat = neutral_cat,
      proportion = proportion,
      totals_by_direction = totals_by_direction,
      nudge_label_outward = nudge_label_outward,
      ...
    )
  )
}

StatDiverging <- ggplot2::ggproto("StatDiverging", Stat,
  required_aes = c("x|y", "fill|diverging_groups"),
  default_aes = aes(weight = 1, label = after_stat(default_label)),
  extra_params = c("stacked", "proportion", "totals_by_direction", "neutral_cat", "nudge_label_outward", "na.rm"),
  setup_data = function(data, params) {
    data$diverging_groups <- data$diverging_groups %||% data$fill
    # Convert to factor, since factor levels have to be known
    if (!is.factor(data$diverging_groups)) {
      data$diverging_groups <- as.factor(data$diverging_groups)
    }

    if (nlevels(data$diverging_groups) < 2) {
      cli::cli_warn("stat_diverging(): diverging_groups should have at least 2 factor levels.")
    }

    data
  },
  # From ggplot2::StatCount
  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE)
    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      cli::cli_abort("{.fn {snake_class(self)}} requires an {.field x} or {.field y} aesthetic.")
    }
    if (has_x && has_y) {
      cli::cli_abort("{.fn {snake_class(self)}} must only have an {.field x} {.emph or} {.field y} aesthetic.")
    }
    if (is.null(params$width)) {
      x <- if (params$flipped_aes) {
        "y"
      } else {
        "x"
      }
      params$width <- resolution(data[[x]], discrete = TRUE) * 0.9
    }

    params$neutral_cat <- match.arg(params$neutral_cat, choices = c("odd", "never"))

    params
  },
  compute_group = function(data, scales, width = NULL, neutral_cat = "odd") {
    n_levels <- nlevels(data$diverging_groups)
    current_level <- unique(as.numeric(data$diverging_groups))

    direction <- case_when(
      # Split middle group if odd number of levels
      (neutral_cat == "odd") & current_level == ((n_levels / 2) + 0.5) ~ c(-0.5, 0.5),
      # Second half is positive,
      current_level > (n_levels / 2) ~ c(0, 1),
      # first half is negative (the rest)
      T ~ c(-1, 0)
    )

    data$weight <- data$weight %||% rep(1, length(data$x))

    data <- data |>
      dplyr::group_by(x, diverging_groups, group) |>
      dplyr::tally(wt = weight) |> # negative or fractional weights?
      ungroup()

    bars <- data |>
      dplyr::transmute(
        x = x,
        diverging_groups = diverging_groups,
        group = group,
        count = n,
        default_label = count,
        # prop = n / sum(n), # Can only be calculated later
        ymin = count * direction[1],
        ymax = count * direction[2],
        sign = sum(direction),
        y = sign * count, # y is count * direction
        width = width,
        .size = length(data$n),
      )
  },

  # All used/passed params have to be named. ... will result in deletions of panel params.
  # See ggplot2::Stat$parameters()
  compute_panel = function(self, data, scales, flipped_aes = FALSE,
                           stacked = TRUE, width = 0.9, neutral_cat = TRUE,
                           proportion = FALSE, totals_by_direction = FALSE, nudge_label_outward = 0) {
    # TODO: Dodge group
    data <- ggplot2::flip_data(data, flipped_aes)
    # data is empty?
    if (is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
      return(data.frame())
    }
    groups <- split(data, ~ group + diverging_groups) |>
      base::Filter(f = nrow) # Drop empty groups
    # Compute group stats
    stats <- lapply(groups, function(group) {
      self$compute_group(data = group, scales = scales, width = width, neutral_cat = neutral_cat)
    }) |> do.call(rbind, args = _) -> stats

    # Calc total numbers and proportions by bar
    stats |>
      dplyr::group_by(x) |>
      dplyr::mutate(
        total = sum(count),
        total_neg = sum(ymin[ymin < 0]),
        total_pos = sum(ymax[ymax > 0]),
        prop = count / total,
        prop_neg = total_neg / total,
        prop_pos = total_pos / total,
      ) -> stats

    # Stack results
    if (stacked) {
      stats |>
        dplyr::group_by(x) |>
        dplyr::mutate(
          # Stack bars from left (-) to right (+)
          ymin = total_neg[1] + cumsum(dplyr::lag(count, default = 0)),
          ymax = ymin + count,
          y = (ymin + ymax) / 2, # For stacked, y is the midpoint of the area
        ) -> stats
    }

    data |>
      # Distinct by the same variables as in split() above
      dplyr::distinct(group, diverging_groups, .keep_all = TRUE) |>
      arrange(x, diverging_groups, groups) |>
      select(-x) |>
      right_join(stats, by = c("group", "diverging_groups")) -> data

    if (totals_by_direction) {
      data |>
        dplyr::group_by(x, sign) |>
        dplyr::summarise(
          total = total[1],
          total_neg = total_neg[1],
          total_pos = total_pos[1],
          prop_neg = prop_neg[1],
          prop_pos = prop_pos[1],
          ymin = min(ymin),
          ymax = max(ymax),
          # Largest negative values for negative group, largest positive value for pos group, middle is 0.
          y = sign[1] * max(c(sign * ymin, sign * ymax)),
          count = sum(count),
          prop = count / total,
          default_label = count
        ) -> data
    }

    if (proportion) {
      data |>
        dplyr::mutate(
          ymin = ymin / total,
          ymax = ymax / total,
          y = y / total,
          default_label = scales::percent(prop, accuracy = 0.1),
        ) -> data
    }

    # Apply nudge_label_outward
    # y-coordinates (for labels) will be moved outwards relative to bar range
    data |>
      dplyr::ungroup() |>
      dplyr::mutate(
        range = (max(ymax) - min(ymin)),
        y = y + (max(ymax) - min(ymin)) * sign * nudge_label_outward
      ) -> data

    data <- ggplot2::flip_data(data, flipped_aes)

    # Handle Transformation of continous scale (e.g. reverse)
    sel_scale <- if (flipped_aes) scales$x else scales$y
    if (!is.null(sel_scale$transform_df)) {
      transformed <- sel_scale$transform_df(data)
      data[names(transformed)] <- transformed
    }

    data
  },
  dropped_aes = "weight"
)

GeomAreaDiverging <- ggplot2::ggproto("GeomAreaDiverging", GeomRibbon,
  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)

    if (inherits(data$x, "mapped_discrete")) {
      cli::cli_warn("{flipped_names(params$flipped_aes)$x} should be continuous and not discrete.")
    }

    if (is.null(data$ymin) && is.null(data$ymax)) {
      cli::cli_abort("Either {.field {flipped_names(params$flipped_aes)$ymin}} or {.field {flipped_names(params$flipped_aes)$ymax}} must be given as an aesthetic.")
    }
    data <- data[order(data$PANEL, data$group, data$x), , drop = FALSE]
    data$y <- data$ymin %||% data$ymax
    flip_data(data, params$flipped_aes)
  }
)
