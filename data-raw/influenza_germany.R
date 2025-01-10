# Data available from: https://github.com/robert-koch-institut/Influenzafaelle_in_Deutschland
# License: Robert Koch-Institut (2025): Laborbestätigte Influenzafälle in Deutschland. [Dataset] Zenodo. DOI:10.5281/zenodo.14619502.

df_influenza <- readr::read_tsv("https://github.com/robert-koch-institut/Influenzafaelle_in_Deutschland/raw/refs/heads/main/IfSG_Influenzafaelle.tsv")

influenza_germany <- df_influenza |>
  filter(Region == "Deutschland", Altersgruppe != "Unbekannt") |>
  transmute(
    ReportingWeek = Meldewoche,
    AgeGroup = Altersgruppe,
    Cases = Fallzahl,
    Incidence = Inzidenz
  )

usethis::use_data(influenza_germany, overwrite = TRUE, compress = "xz")
