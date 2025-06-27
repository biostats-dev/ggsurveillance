theme_classic_light <- function(base_size = 11, base_family = "", base_line_size = base_size / 22,
                                base_rect_size = base_size / 22) {
  theme_classic(
    base_size = base_size, base_family = base_family,
    base_line_size = base_line_size, base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.line = element_line(colour = "gray"),
      # legendry.bracket = element_line(colour = "gray"),
      panel.background = element_blank(),
      # legend.background = element_rect(fill = "transparent", colour = "NA") # More legend styling?
    )
}

theme_classic_light_x_axis <- function(base_size = 11, base_family = "", base_line_size = base_size / 22,
                                       base_rect_size = base_size / 22) {
  theme_classic(
    base_size = base_size, base_family = base_family,
    base_line_size = base_line_size, base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.line.x = element_line(colour = "gray"),
      axis.line.y = element_blank(),
      axis.ticks = element_line(color = "black"),
      # legendry.bracket = element_line(colour = "gray"),
      panel.background = element_blank(),
      # legend.background = element_rect(fill = "transparent", colour = "NA") # More legend styling?
    )
}

theme_ticks_only <- function(base_size = 11, base_family = "", base_line_size = base_size / 22,
                             base_rect_size = base_size / 22) {
  theme_classic(
    base_size = base_size, base_family = base_family,
    base_line_size = base_line_size, base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.line = element_blank(),
      axis.ticks = element_line(color = "black"),
      # legendry.bracket = element_line(colour = "gray"),
    )
}
