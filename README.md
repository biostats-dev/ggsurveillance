# ggsurveillance <img src="man/figures/logo.svg" alt="logo" align="right" width="120" height="139" style="border: none; float: right;"/>

<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![R-CMD-check](https://github.com/biostats-dev/ggsurveillance/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/biostats-dev/ggsurveillance/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

**ggsurveillance** is an R package with helpful tools and ggplot extensions for epidemiology, especially infectious disease surveillance and outbreak investigation. All functions provide tidy functional interfaces for easy integration with the tidyverse. For documentation and vignettes see: <a href="https://ggsurveillance.biostats.dev" target="_blank">ggsurveillance.biostats.dev</a>

This packages provides:

-   `geom_epicurve()` : A ggplot geom for plotting epicurves, includes date aggregation

    -   including `scale_y_cases_5er()` for better (case) count axis breaks and positioning.

-   `align_dates_seasonal()` : Align surveillance data for seasonal plots (e.g. flu season).

-   `create_agegroups()`: Create reproducible age groups with highly customizable labels.

-   `geom_epigantt()` : A geom for epigantt plots. Helpful to visualize overlapping time intervals for contact tracing. (Beta)

-   and more: `geometric_mean()` , `expand_counts()`,

## Creating Epicurves

``` r
library(ggplot2)
library(tidyr)
library(outbreaks)
library(ggsurveillance)

sars_canada_2003 |> #SARS dataset from outbreaks
  pivot_longer(starts_with("cases"), 
               names_prefix = "cases_", 
               names_to = "origin") |>
  ggplot(aes(x = date, weight = value, fill = origin)) +
  geom_epicurve(date_resolution = "week") +
  scale_x_date(date_labels = "W%V'%g", date_breaks = "2 weeks") +
  scale_y_cases_5er() +
  scale_fill_brewer(type = "qual", palette = 6) +
  theme_classic()
```

![Epicurve of 2003 SARS outbreak in Canada](man/figures/epicurve_readme.png)

## Align surveillance data for seasonal comparison

``` r
library(ggplot2)
library(ggsurveillance)

influenza_germany |>
  align_dates_seasonal(dates_from = ReportingWeek,
                       date_resolution = "isoweek",
                       start = 28) -> df_flu_aligned

ggplot(df_flu_aligned, aes(x = date_aligned, y = Incidence)) +
  stat_summary(data = . %>% filter(!current_season), fun.data = median_hilow,
               geom = "ribbon", alpha = 0.3) +
  stat_summary(data = . %>% filter(!current_season), fun = median,
               geom = "line") +
  geom_line(data = . %>% filter(current_season), color = "dodgerblue4", 
            linewidth = 2) +
  scale_x_date(date_labels = "%b'%y") +
  facet_wrap(~AgeGroup, labeller = label_both) +
  theme_bw()
```

![Seasonal influenza data from Germany by Age Group](man/figures/seasonal_plot_readme.png)

## Installation

Install the development version with:

``` r
devtools::install_github("biostats-dev/ggsurveillance")

# or directly with renv:
renv::install("biostats-dev/ggsurveillance")
```
