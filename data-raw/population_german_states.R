library(dplyr)
library(tidyr)

# Source: https://www-genesis.destatis.de/datenbank/online/statistic/12411/table/12411-0013
raw_pop_berlin <- readr::read_csv2("data-raw/12411-0013_de_population_age_sex.csv")

population_german_states <- raw_pop_berlin |>
  transmute(
    reporting_date = time,
    state = `1_variable_attribute_label`,
    sex = `2_variable_attribute_label`,
    age = sub("^ALT(\\d+).*", "\\1", `3_variable_attribute_code`) |> as.numeric(),
    n = value
  ) |>
  filter(
    sex != "Insgesamt",
    !is.na(age),
  ) |>
  mutate(
    sex = factor(sex, levels = c("weiblich", "männlich"), labels = c("female", "male"))
  )

usethis::use_data(population_german_states, overwrite = TRUE, compress = "xz")

population_german_states |>
  filter(state %in% c("Berlin", "Mecklenburg-Vorpommern"), age < 90) |>
  ggplot(aes(y = age, fill = sex, weight = n)) +
  geom_bar_diverging(width = 1) +
  geom_vline(xintercept = 0) +
  scale_x_continuous_diverging(n.breaks = 10) +
  facet_wrap(~state, scales = "free_x") +
  theme_bw()
