library(ggplot2)
library(tidyr)
library(outbreaks)

sars_canada_2003 |> # Sars dataset from outbreaks
  pivot_longer(starts_with("cases"), names_prefix = "cases_", names_to = "origin") |>
  ggplot(aes(x = date, weight = value, fill = origin)) +
  geom_epicurve(date_resolution = "week") +
  scale_x_date(date_labels = "W%V'%g", date_breaks = "2 weeks") +
  scale_y_cases_5er() +
  scale_fill_brewer(type = "qual", palette = 6) +
  theme_classic()

ggsave("man/figures/epicurve_readme.png", width = 7, height = 4, dpi = 600)
ggsave("man/figures/epicurve_readme.svg", width = 7, height = 4, dpi = 600)


## Influenza

influenza_germany |>
  filter(AgeGroup == "00+") |>
  align_dates_seasonal(
    dates_from = ReportingWeek,
    date_resolution = "isoweek",
    start = 28
  ) -> df_flu_aligned

ggplot(df_flu_aligned, aes(x = date_aligned, y = Incidence)) +
  stat_summary(
    aes(linetype = "Historical Median (Min-Max)"),
    data = . %>% filter(!current_season), fun.data = median_hilow,
    geom = "ribbon", alpha = 0.3,
  ) +
  stat_summary(
    aes(linetype = "Historical Median (Min-Max)"),
    data = . %>% filter(!current_season), fun = median,
    geom = "line"
  ) +
  geom_line(
    aes(linetype = "2024/25"),
    colour = "dodgerblue4",
    data = . %>% filter(current_season), linewidth = 2
  ) +
  labs(linetype = "") +
  scale_x_date(date_labels = "%b'%y") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.8))

ggsave("man/figures/seasonal_plot_readme.png", width = 6, height = 3.5, dpi = 600)

