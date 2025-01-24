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
  align_dates_seasonal(
    dates_from = ReportingWeek,
    date_resolution = "isoweek",
    start = 28
  ) -> df_flu_aligned

ggplot(df_flu_aligned, aes(x = date_aligned, y = Incidence)) +
  stat_summary(
    data = . %>% filter(!current_season), fun.data = median_hilow,
    geom = "ribbon", alpha = 0.3
  ) +
  stat_summary(
    data = . %>% filter(!current_season), fun = median,
    geom = "line"
  ) +
  geom_line(data = . %>% filter(current_season), color = "dodgerblue4", linewidth = 2) +
  scale_x_date(date_labels = "%b'%y") +
  facet_wrap(~AgeGroup, labeller = label_both) +
  theme_bw()

ggsave("man/figures/seasonal_plot_readme.png", width = 8, height = 4, dpi = 600)
