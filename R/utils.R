## plotly compatibility functions
.onLoad <- function(lib, pkg) {
  rlang::run_on_load()
}

rlang::on_load({
  if (isNamespaceLoaded("plotly")) {
    register_plotly_methods()
  } else {
    # Set up to register when plotly gets loaded later
    rlang::on_package_load("plotly", register_plotly_methods())
  }
})

# Function to register all plotly methods
register_plotly_methods <- function() {
  # Get the S3 methods from plotly
  geom_bar_method <- utils::getS3method("geom2trace", "GeomBar", envir = asNamespace("plotly"))
  linerange_method <- utils::getS3method("to_basic", "GeomLinerange", envir = asNamespace("plotly"))

  # Register our methods
  registerS3method("geom2trace", "GeomEpicurve", geom_bar_method, envir = asNamespace("plotly"))
  registerS3method("geom2trace", "GeomBarRange", geom_bar_method, envir = asNamespace("plotly"))
  registerS3method("to_basic", "GeomEpigantt", linerange_method, envir = asNamespace("plotly"))
}

# Evaluates all arguments (see https://github.com/r-lib/scales/pull/81)
force_all <- function(...) list(...)

# ggplot2 grid utils: https://github.com/tidyverse/ggplot2/blob/main/R/utilities-grid.R
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

# ggplot2 utils From https://github.com/tidyverse/ggplot2/blob/main/R/geom-.R
fix_linewidth <- function(data, name) {
  if (is.null(data$linewidth) && !is.null(data$size)) {
    deprecate_warn0("3.4.0", I(paste0("Using the `size` aesthetic with ", name)), I("the `linewidth` aesthetic"))
    data$linewidth <- data$size
  }
  data
}

borderedRectGrob <- function(
    xmin, xmax, ymin, ymax, default.units = "npc",
    linewidth, linewidth.unit = "points", fill, colour, ...) {
  # Linewidth relative to rect size in percent of smaller side
  # TODO: How to handle this better? This solution still results in uneven borders
  if (linewidth.unit %in% c("%", "percent")) {
    linewidth <- min(c(xmax - xmin, ymax - ymin)) * (linewidth / 100)
    linewidth.unit <- default.units
  }
  x_outer_min <- xmin |> grid::unit(default.units)
  x_outer_max <- xmax |> grid::unit(default.units)
  y_outer_min <- ymin |> grid::unit(default.units)
  y_outer_max <- ymax |> grid::unit(default.units)

  x_inner_min <- x_outer_min + grid::unit(linewidth, linewidth.unit)
  x_inner_max <- x_outer_max - grid::unit(linewidth, linewidth.unit)
  y_inner_min <- y_outer_min + grid::unit(linewidth, linewidth.unit)
  y_inner_max <- y_outer_max - grid::unit(linewidth, linewidth.unit)

  # Inner Rect
  fill_rect <- grid::polygonGrob(
    x = grid::unit.c(x_inner_min, x_inner_max, x_inner_max, x_inner_min),
    y = grid::unit.c(y_inner_min, y_inner_min, y_inner_max, y_inner_max),
    id = rep(1:length(x_inner_min), 4),
    default.units = default.units,
    name = "inner_rect",
    gp = grid::gpar(col = NA, fill = fill, lwd = 0)
  )

  # Border Polygon
  border_poly <- grid::polygonGrob(
    x = grid::unit.c(
      x_outer_min, x_outer_max, x_outer_max, x_outer_min, x_outer_min, # outer (CCW)
      x_inner_min, x_inner_min, x_inner_max, x_inner_max, x_inner_min # inner (CW)
    ),
    y = grid::unit.c(
      y_outer_min, y_outer_min, y_outer_max, y_outer_max, y_outer_min, # outer (CCW)
      y_inner_min, y_inner_max, y_inner_max, y_inner_min, y_inner_min # inner (CW)
    ),
    id = rep(1:length(x_inner_min), 10),
    default.units = "native",
    name = "border",
    gp = grid::gpar(
      col = NA,
      fill = colour,
      lwd = 0
    )
  )

  return(grid::gTree(children = grid::gList(fill_rect, border_poly)))
}

ggsave_multiple <- function(
    plot = last_plot(),
    filename = "test",
    date = Sys.Date(),
    path = NULL,
    device = c("png", "svg"),
    width = c(12, 14),
    height = c(9),
    units = "cm", # ?
    dpi = 600,
    limitsize = TRUE,
    bg = NULL,
    scale = 1,
    create.dir = FALSE,
    ...) {
  if (!rlang::is_null(date) && !is.na(date) && (as.character(date) != "")) {
    date_prefix <- paste0(as.character(date), "_")
  } else {
    date_prefix <- ""
  }
  # fmt: skip
  expand.grid(
    device = device, width = width, height = height, units = units, stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(
      filename = paste0(
        date_prefix, filename, "_", width, units, "x", height, units, ".", device
      )
    ) |>
    purrr::pmap(
      .f = ggplot2::ggsave,
      plot = plot, dpi = dpi, path = path,
      limitsize = limitsize, bg = bg,
      scale = scale, create.dir = create.dir,
      ...
    ) |>
    unlist() |>
    invisible()
}
