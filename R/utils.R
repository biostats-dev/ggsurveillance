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