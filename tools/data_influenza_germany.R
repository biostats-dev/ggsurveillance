library(dplyr)

df_influenza <- readr::read_tsv("https://github.com/robert-koch-institut/Influenzafaelle_in_Deutschland/raw/refs/heads/main/IfSG_Influenzafaelle.tsv")

df_influenza_germany <- df_influenza |>
  filter(Region == "Deutschland", Altersgruppe != "Unbekannt")

df_influenza_german_states <- df_influenza |>
  filter(Region != "Deutschland", Altersgruppe == "00+")


