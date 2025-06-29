library(ggplot2)
library(tidyr)
library(outbreaks)

sars_canada_2003 |> # Sars dataset from outbreaks
  pivot_longer(starts_with("cases"), names_prefix = "cases_", names_to = "origin") |>
  ggplot(aes(x = date, weight = value, fill = origin)) +
  geom_epicurve(date_resolution = "week") +
  geom_epicurve_text(aes(label = ifelse(origin == "travel", "🛪", "")),
    date_resolution = "week",
    size = 1.5, color = "white"
  ) +
  scale_x_date(date_labels = "W%V'%g", date_breaks = "2 weeks") +
  scale_y_cases_5er() +
  scale_fill_brewer(type = "qual", palette = 6) +
  theme_classic()

ggsave("man/figures/epicurve_readme.png", width = 7, height = 4, dpi = 600)

ggsave("man/figures/epicurve_readme.svg", width = 7, height = 4, dpi = 600)

# ggplot2 extension gallery thumbnail https://exts.ggplot2.tidyverse.org/gallery/
sars_canada_2003 |> # Sars dataset from outbreaks
  pivot_longer(starts_with("cases"), names_prefix = "cases_", names_to = "origin") |>
  ggplot(aes(x = date, weight = value, fill = origin)) +
  geom_epicurve(date_resolution = "week") +
  scale_x_date(date_labels = "W%V'%g", date_breaks = "2 weeks") +
  scale_y_cases_5er() +
  scale_fill_brewer(type = "qual", palette = 6) +
  theme_classic() +
  theme_mod_legend_position(position.inside = c(0.5, 0.98)) +
  theme_mod_rotate_x_axis_labels_45() +
  theme_mod_remove_legend_title() +
  theme(legend.direction = "horizontal")
ggsave("man/figures/ggsurveillance.png", width = 350 / 80, height = 300 / 80, dpi = 300)

## Influenza
library(ggplot2)
library(dplyr)
Sys.setlocale("LC_ALL", "en_GB.UTF-8")

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
  labs(linetype = NULL) +
  scale_x_date(
    date_breaks = "month", date_labels = "%b'%Y",
    guide = guide_axis_nested_date()
  ) +
  theme_bw() +
  theme_mod_legend_position(position.inside = c(0.2, 0.8))

ggsave("man/figures/seasonal_plot_readme.png", width = 6, height = 3.5, dpi = 600)

## Diverging Bar Charts

# Age pyramid
library(dplyr)
library(ggplot2)
library(ggsurveillance)

population_german_states |>
  filter(state %in% c("Berlin", "Mecklenburg-Vorpommern"), age < 90) |>
  ggplot(aes(y = age, fill = sex, weight = n)) +
  geom_bar_diverging(width = 1) +
  geom_vline(xintercept = 0) +
  scale_x_continuous_diverging(n.breaks = 7) +
  facet_wrap(~state, scales = "free_x") +
  theme_bw() +
  theme_mod_legend_top()
ggsave("man/figures/diverging_bar_chart_age_pyramid_readme.png", width = 7, height = 4, dpi = 600)

## EpiGantt

library(dplyr)
library(tidyr)
library(ggplot2)

# Transform to long format
linelist_hospital_outbreak |>
  pivot_longer(
    cols = starts_with("ward"),
    names_to = c(".value", "num"),
    names_pattern = "ward_(name|start_of_stay|end_of_stay)_([0-9]+)",
    values_drop_na = TRUE
  ) -> df_stays_long

linelist_hospital_outbreak |>
  pivot_longer(cols = starts_with("pathogen"), values_to = "date") -> df_detections_long

# Plot
ggplot(df_stays_long) +
  geom_epigantt(aes(y = Patient, xmin = start_of_stay, xmax = end_of_stay, color = name)) +
  geom_point(aes(y = Patient, x = date, shape = "Date of pathogen detection"), data = df_detections_long) +
  scale_y_discrete_reverse() +
  theme_bw() +
  theme_mod_legend_bottom()

ggsave("man/figures/epigantt_plot_readme.png", width = 6, height = 3.5, dpi = 600)
