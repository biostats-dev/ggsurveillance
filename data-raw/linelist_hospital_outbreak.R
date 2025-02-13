# Example Data for a hospital outbreak over multiple wards

library(dplyr)
library(tidyr)

linelist_hospital_outbreak <- tibble::tribble(
  ~Patient, ~ward_name_1, ~ward_start_of_stay_1, ~ward_end_of_stay_1, ~ward_name_2, ~ward_start_of_stay_2, ~ward_end_of_stay_2, ~pathogen_detection_1, ~pathogen_detection_2,
  # Patient 0 was infected early on, but only tested later
  "0", "intensive care unit", "2024-06-12", "2024-07-25", NA, NA, NA, "2024-06-30", "2024-07-09",
  # Cases on ICU screened after Patient 0 tested positive
  "1", "intensive care unit", "2024-06-26", "2024-07-03", NA, NA, NA, "2024-07-01", NA,
  "2", "intensive care unit", "2024-06-28", "2024-07-06", NA, NA, NA, "2024-07-01", NA,
  # Patient which already had left the ICU when Patient 0 tested positive. Was tested later when the case search was extended.
  # This Patient is a possible link between the outbreak in the ICU and the outbreak on the general ward.
  "3", "intensive care unit", "2024-06-19", "2024-06-26", "general ward", "2024-06-26", "2024-07-13", "2024-07-06", NA,
  # Outbreak cases on general ward. Cases were detected during screening after Patient 3 was tested positive
  "4", "general ward", "2024-07-03", "2024-07-12", NA, NA, NA, "2024-07-07", NA,
  "5", "general ward", "2024-07-04", "2024-07-15", NA, NA, NA, "2024-07-07", NA,
  "6", "general ward", "2024-07-06", "2024-07-11", NA, NA, NA, "2024-07-08", NA,
  # Patient 7 was tested positive by the general practitioner, which then contacted the hospital.
  "7", "general ward", "2024-06-30", "2024-07-05", NA, NA, NA, "2024-07-13", NA,
) |> mutate_at(vars(contains("stay"), contains("detection")), lubridate::as_date)

usethis::use_data(linelist_hospital_outbreak, overwrite = TRUE, compress = "xz")

linelist_hospital_outbreak |>
  pivot_longer(
    cols = starts_with("ward"),
    names_to = c(".value", "num"),
    names_pattern = "ward_(name|start_of_stay|end_of_stay)_([0-9]+)",
    values_drop_na = TRUE
  ) -> df_stays_long

linelist_hospital_outbreak |>
  pivot_longer(cols = starts_with("pathogen"), values_to = "date") -> df_detections_long

ggplot(df_stays_long) +
  geom_epigantt(aes(y = Patient, xmin = start_of_stay, xmax = end_of_stay, color = name)) +
  geom_point(aes(y = Patient, x = date), data = df_detections_long) +
  scale_y_discrete_reverse() +
  theme_bw() +
  theme(legend.position = "bottom")
