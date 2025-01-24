#' Germany Influenza (FLU) Surveillance data
#'
#' A subset of the weekly German influenza surveillance data from 2020 to 2024.
#'
#' A data frame with 1,029 rows and 4 columns:
#' \describe{
#'   \item{ReportingWeek}{Reporting Week in "2024-W03" format}
#'   \item{AgeGroup}{Age groups: `00+` for all and `00-14`, `15-59` and `60+` for age stratified cases.}
#'   \item{Cases}{Year}
#'   \item{Incidence}{Calculated weekly incidence}
#' }
#' @source License CC-BY 4.0: Robert Koch-Institut (2025): Laborbestätigte Influenzafälle in Deutschland. Dataset. Zenodo.
#' DOI:10.5281/zenodo.14619502. \url{https://github.com/robert-koch-institut/Influenzafaelle_in_Deutschland}
"influenza_germany"
