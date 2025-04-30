geom_bar_diverging <- function(mapping = NULL, data = NULL,
                               stat = "diverging", position = "identity",
                               proportion = FALSE,
                               neutral_cat = TRUE, break_cat = NULL, # odd, never, always
                               ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomBarRange,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      neutral_cat = neutral_cat,
      stacked = TRUE,
      proportion = proportion,
      totals_only = FALSE,
      div_just = 0,
      ...
    )
  )
}

geom_bar_range <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomBarRange,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

stat_diverging <- function(mapping = NULL, data = NULL,
                           geom = "text", position = "identity",
                           stacked = TRUE, proportion = FALSE,
                           totals_only = FALSE, div_just = 0,
                           neutral_cat = TRUE,
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
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
      totals_only = totals_only,
      div_just = div_just,
      ...
    )
  )
}

StatDiverging <- ggproto("StatDiverging", Stat,
  required_aes = c("x|y", "fill|diverging_groups"),
  default_aes = aes(weight = 1, label = after_stat(default_label)),
  extra_params = c("stacked", "stacked", "proportion", "totals_only", "neutral_cat", "div_just", "na.rm"),
  setup_data = function(data, params) {
    # browser()
    data$diverging_groups <- data$diverging_groups %||% data$fill
    # Convert to factor, since factor levels have to be known
    if (!is.factor(data$diverging_groups)) {
      data$diverging_groups <- as.factor(data$diverging_groups)
    }
    data
  },
  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE)

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      cli::cli_abort("stat_diverging() requires an {.field x} or {.field y} aesthetic.")
    }
    if (has_x && has_y) {
      cli::cli_abort("stat_diverging() must only have an {.field x} {.emph or} {.field y} aesthetic.")
    }

    if (is.null(params$width)) {
      x <- if (params$flipped_aes) "y" else "x"
      params$width <- resolution(data[[x]], discrete = TRUE) * 0.9
    }

    params
  },
  compute_group = function(data, scales, width = NULL, neutral_cat = TRUE) {
    # data <- flip_data(data, flipped_aes)
    n_levels <- length(levels(data$diverging_groups))
    current_level <- unique(as.numeric(data$diverging_groups))

    direction <- case_when(
      # Split middle group if odd number of levels
      neutral_cat & current_level == ((n_levels / 2) + 0.5) ~ c(-0.5, 0.5),
      # Second half is positive,
      current_level > (n_levels / 2) ~ c(0, 1),
      # first half is negative (the rest)
      T ~ c(-1, 0)
    )

    data$weight <- data$weight %||% rep(1, length(data$x))

    data <- data |>
      dplyr::arrange(x, diverging_groups) |>
      dplyr::group_by(x, diverging_groups, group) |>
      dplyr::tally(wt = weight)


    bars <- data |>
      dplyr::transmute(
        x = x,
        diverging_groups = diverging_groups,
        group = group,
        count = n,
        default_label = count,
        prop = n / sum(abs(n)), # abs? negative weights?
        ymin = count * direction[1],
        ymax = count * direction[2],
        y = (ymin + ymax) / 2,
        sign = sign(y),
        width = width,
        .size = length(data$n),
        # flipped_aes = flipped_aes
      )
    # flip_data(bars, flipped_aes)
  },
  # All used/passed params have to be named. ... will result in deletions of panel params.
  # See ggplot2::Stat$parameters()
  compute_panel = function(self, data, scales, flipped_aes = FALSE,
                           stacked = TRUE, width = 0.9, neutral_cat = TRUE,
                           proportion = FALSE, totals_only = FALSE, div_just = 0) {
    # TODO: Dodge group
    # browser()
    data <- flip_data(data, flipped_aes)
    if (plyr::empty(data)) {
      return(data.frame())
    }
    groups <- split(data, ~ group + diverging_groups) |>
      base::Filter(f = nrow) # Drop empty groups
    # Compute group stats
    stats <- lapply(groups, function(group) {
      self$compute_group(data = group, scales = scales, width = width, neutral_cat = neutral_cat)
    }) |> dplyr::bind_rows()

    # Calc total numbers and proportions by row
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
    if (stacked == TRUE) {
      stats |>
        dplyr::group_by(x) |>
        dplyr::mutate(
          ymin = .calc_ymin_stacked(
            dplyr::lag(ymin, default = total_neg[1]), dplyr::lag(count, default = 0)
          ),
          ymax = ymin + count,
          y = (ymin + ymax) / 2, # Recalc midpoint
        ) -> stats
    }

    # Reconstruct data
    lapply(groups, slice_head, n = 1) |>
      dplyr::bind_rows() |>
      select(-x) |>
      right_join(stats, by = c("group", "diverging_groups")) -> data

    if (totals_only) {
      data |>
        dplyr::group_by(x, sign) |>
        summarize(
          total = total[1],
          total_neg = total_neg[1],
          total_pos = total_pos[1],
          prop_neg = prop_neg[1],
          prop_pos = prop_pos[1],
          ymin = min(ymin),
          ymax = max(ymax),
          # Largest negative values for negative group, largest positive value for pos group, middle is 0.
          y = sign * max(c(sign * ymin, sign * ymax)),
          count = sum(count),
          prop = count / total,
          default_label = count
        ) -> data
    }

    if (proportion == TRUE) {
      data |>
        dplyr::mutate(
          ymin = ymin / total,
          ymax = ymax / total,
          y = y / total,
          default_label = scales::percent(prop, accuracy = 0.1),
        ) -> data
    }

    # Apply div_just
    # y-coordinates (for labels) will be moved outwards relative to bar range
    data |>
      dplyr::ungroup() |>
      dplyr::mutate(
        range = (max(ymax) - min(ymin)),
        y = y + (max(ymax) - min(ymin)) * sign * div_just
      ) -> data

    data <- flip_data(data, flipped_aes)

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

.calc_ymin_stacked <- function(ymin, count) {
  for (i in seq_along(ymin)[-1]) {
    ymin[i] <- ymin[i - 1] + count[i]
  }
  return(ymin)
}

GeomBarRange <- ggproto("GeomBarRange", GeomBar,
  # TODO: Allow flipped aes
  required_aes = c("x|y", "xmin|ymin", "xmax|ymax"),

  # These aes columns are created by setup_data(). They need to be listed here so
  # that GeomRect$handle_na() properly removes any bars that fall outside the defined
  # limits, not just those for which x and y are outside the limits
  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
  default_aes = aes(!!!GeomRect$default_aes, width = 0.9),
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params)
    params
  },
  extra_params = c("just", "na.rm", "orientation"),
  setup_data = function(self, data, params) {
    # browser()
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data$just <- params$just %||% 0.5
    data <- transform(data,
      xmin = x - width * just, xmax = x + width * (1 - just),
      width = NULL, just = NULL
    )
    flip_data(data, params$flipped_aes)
  },
  rename_size = FALSE
)



scale_y_continuous_diverging <- function(name = waiver(), limits = NULL, labels = NULL, ...,
                                         breaks = waiver(), n.breaks = 10, transform = c("identity", "reverse"),
                                         expand = waiver(), position = "left") {
  if (!is.null(labels)) {
    # if more than 2 labels functions are passed, then ignore
    labeller <- if (length(labels) > 2) labels else do.call(scales::compose_label, c(abs, labels))
  } else {
    labeller <- abs
  }

  limits <- limits %||% limit_symmetrical

  transform <- match.arg(transform)

  ggplot2::scale_y_continuous(
    name = name,
    limits = limits,
    labels = labeller,
    ...,
    breaks = breaks,
    n.breaks = n.breaks,
    transform = transform,
    expand = expand,
    position = position
  )
}

scale_x_continuous_diverging <- function(name = waiver(), limits = NULL, labels = NULL, ...,
                                         breaks = waiver(), n.breaks = 10, transform = c("identity", "reverse"),
                                         expand = waiver(), position = "bottom") {
  if (!is.null(labels)) {
    # if more than 2 labels functions are passed, then ignore
    labeller <- if (length(labels) > 2) labels else do.call(scales::compose_label, c(abs, labels))
  } else {
    labeller <- abs
  }

  limits <- limits %||% limit_symmetrical

  transform <- match.arg(transform)

  ggplot2::scale_x_continuous(
    name = name,
    limits = limits,
    labels = labeller,
    ...,
    breaks = breaks,
    n.breaks = n.breaks,
    transform = transform,
    expand = expand,
    position = position
  )
}

# This function creates symmetrical limits around 0 (offset).
limit_symmetrical <- function(x, center = 0) (max(abs(x - center)) * sign(x)) + center


