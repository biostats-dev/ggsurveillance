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
                          stacked = TRUE,
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
      ...
    )
  )
}

StatDiverging <- ggproto("StatDiverging", Stat,
  required_aes = c("x|y", "fill|diverging_groups"),
  default_aes = aes(weight = 1, label = after_stat(count)),
  extra_params = c("stacked", "stacked", "neutral_cat", "na.rm"),
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
      cli::cli_abort("{.fn {snake_class(self)}} requires an {.field x} or {.field y} aesthetic.")
    }
    if (has_x && has_y) {
      cli::cli_abort("{.fn {snake_class(self)}} must only have an {.field x} {.emph or} {.field y} aesthetic.")
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
        prop = n / sum(abs(n)), # abs? negative weights?
        ymin = count * direction[1],
        ymax = count * direction[2],
        y = (ymin + ymax) / 2,
        width = width,
        .size = length(data$n),
        # flipped_aes = flipped_aes
      )
    # flip_data(bars, flipped_aes)
  },
  # All used/passed params have to be named. ... will result in deletions of panel params. 
  # See ggplot2::Stat$parameters()
  compute_panel = function(self, data, scales, flipped_aes, stacked, width, neutral_cat) {
    #TODO: Dodge group
    #browser()
    data <- flip_data(data, flipped_aes)
    if (plyr::empty(data)) {
      return(data.frame())
    }
    groups <- split(data, ~group + diverging_groups) |> 
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
        prop_pos = total_pos / total
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
    
    flip_data(data, flipped_aes)
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
  required_aes = c("x|y"),

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
    #browser()
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data$just <- params$just %||% 0.5
    data <- transform(data,
      # ymin = pmin(y, 0), ymax = pmax(y, 0),
      xmin = x - width * just, xmax = x + width * (1 - just),
      width = NULL, just = NULL
    )
    flip_data(data, params$flipped_aes)
  },
  rename_size = FALSE
)



scale_y_continuous_diverging <- function(name = waiver(), limits = NULL, labels = NULL, ...,
                                         expand = waiver(), position = "left") {
  ggplot2::scale_y_continuous(
    name = name,
    # TODO: add reverse
    limits = \(x) rep(max(abs(x)), 2) * c(-1, 1),
    # TODO: allow label functions
    labels = abs,
    ...,
    expand = expand,
    position = position
  )
}

scale_x_continuous_diverging <- function(name = waiver(), limits = NULL, labels = NULL, ...,
                                         expand = waiver(), position = "bottom") {
  ggplot2::scale_x_continuous(
    name = name,
    # TODO: add reverse
    limits = \(x) rep(max(abs(x)), 2) * c(-1, 1),
    # TODO: allow label functions
    labels = abs,
    ...,
    expand = expand,
    position = position
  )
}
