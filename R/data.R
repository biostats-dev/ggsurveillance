#' Germany Influenza (FLU) Surveillance data
#'
#' A subset of the weekly German influenza surveillance data from January 2020 to January 2025.
#'
#' @format A data frame with 1,037 rows and 4 columns:
#' \describe{
#'   \item{ReportingWeek}{Reporting Week in "2024-W03" format}
#'   \item{AgeGroup}{Age groups: `00+` for all and `00-14`, `15-59` and `60+` for age stratified cases.}
#'   \item{Cases}{Weekly case count}
#'   \item{Incidence}{Calculated weekly incidence}
#' }
#' @source License CC-BY 4.0: Robert Koch-Institut (2025): Laborbestätigte Influenzafälle in Deutschland. Dataset. Zenodo.
#' DOI:10.5281/zenodo.14619502. \url{https://github.com/robert-koch-institut/Influenzafaelle_in_Deutschland}
#' @examples
#' library(ggplot2)
#'
#' influenza_germany |>
#'   align_dates_seasonal(
#'     dates_from = ReportingWeek, date_resolution = "isoweek", start = 28
#'   ) -> df_flu_aligned
#'
#' ggplot(df_flu_aligned, aes(x = date_aligned, y = Incidence, color = season)) +
#'   geom_line() +
#'   facet_wrap(~AgeGroup) +
#'   theme_bw() +
#'   theme_mod_rotate_x_axis_labels_45()
#'
"influenza_germany"

#' Population of the German states (2023)
#'
#' German Population data by state in 2023
#'
#' @format A data frame with 2912 rows and 5 columns:
#' \describe{
#'   \item{reporting_date}{Date: Always "2023-12-31"}
#'   \item{state}{Character: Name of the German state}
#'   \item{age}{Numeric: Age from 0 to 89. Age 90 includes "90 and above"}
#'   \item{sex}{Factor: "female" or "male"}
#'   \item{n}{Numeric: Population size}
#' }
#' @source © Statistisches Bundesamt (Destatis), Genesis-Online, 2025:
#' Bevölkerung: Bundesländer, Stichtag, Geschlecht, Altersjahre (12411-0013).
#' Data licence Germany (\href{https://www.govdata.de/dl-de/by-2-0}{dl-de/by-2-0})
#' \url{https://www-genesis.destatis.de/datenbank/online/statistic/12411/table/12411-0013}
#' @examples
#' # Age pyramid
#' library(ggplot2)
#' population_german_states |>
#'   filter(age < 90) |>
#'   ggplot(aes(y = age, fill = sex, weight = n)) +
#'   geom_bar_diverging(width = 1) +
#'   geom_vline(xintercept = 0) +
#'   scale_x_continuous_diverging() +
#'   facet_wrap(~state, scales = "free_x") +
#'   theme_bw(base_size = 8) +
#'   theme_mod_legend_top()
"population_german_states"

#' Line list of a fictional hospital outbreak (Data)
#'
#' This hospital outbreak is inspired by typical hospital outbreaks with resistant 4MRGN bacterial pathogens.
#' These outbreaks start silent, since they are not initially apparent from the symptoms of the patient.
#'
#' @format A data frame with 8 rows and 9 columns:
#' * `Patient` - Patient ID (0-7)
#' * `ward_name_1` - Name of first ward where patient stayed
#' * `ward_start_of_stay_1` - Start date of stay in first ward
#' * `ward_end_of_stay_1` - End date of stay in first ward
#' * `ward_name_2` - Name of second ward where patient stayed (if applicable)
#' * `ward_start_of_stay_2` - Start date of stay in second ward (if applicable)
#' * `ward_end_of_stay_2` - End date of stay in second ward (if applicable)
#' * `pathogen_detection_1` - Date of first positive pathogen test
#' * `pathogen_detection_2` - Date of second positive pathogen test (if applicable)
#'
#' Patient details:
#' * Patient 0: Index case (ICU), infected early on but detected June 30, 2024
#' * Patient 1-2: ICU patients, found during initial screening
#' * Patient 3: Case who moved from ICU to general ward prior to the detection of patient 0,
#'    potentially linking both outbreak clusters. Detected during extended case search
#' * Patient 4-6: General ward cases, found after Patient 3's detection
#' * Patient 7: General ward case, detected post-discharge by GP, who notified the hospital
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#'
#' # Transform hospital outbreak line list to long format
#' linelist_hospital_outbreak |>
#'   pivot_longer(
#'     cols = starts_with("ward"),
#'     names_to = c(".value", "num"),
#'     names_pattern = "ward_(name|start_of_stay|end_of_stay)_([0-9]+)",
#'     values_drop_na = TRUE
#'   ) -> df_stays_long
#'
#' linelist_hospital_outbreak |>
#'   pivot_longer(cols = starts_with("pathogen"), values_to = "date") -> df_detections_long
#'
#' # Create Epi Gantt chart showing ward stays and test dates
#' ggplot(df_stays_long) +
#'   geom_epigantt(aes(y = Patient, xmin = start_of_stay, xmax = end_of_stay, color = name)) +
#'   geom_point(aes(y = Patient, x = date, shape = "Date of pathogen detection"),
#'     data = df_detections_long
#'   ) +
#'   scale_y_discrete_reverse() +
#'   theme_bw() +
#'   theme(legend.position = "bottom")
#'
"linelist_hospital_outbreak"
