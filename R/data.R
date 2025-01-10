#' Germany Influenza (FLU) Surveillance data
#'
#' A subset of the weekly german influenza surveillance data.
#'
#' @format ## `influenza_germany`
#' A data frame with 1,029 rows and 4 columns:
#' \describe{
#'   \item{ReportingWeek}{Reporting Week in "2024-W03" format}
#'   \item{AgeGroup}{Age groups: `00+` for all and `00-14`, `15-59` and `60+` for age stratified cases.}
#'   \item{Cases}{Year}
#'   \item{Incidence}{Calculated weekly incidence}
#' }
#' @source https://github.com/robert-koch-institut/Influenzafaelle_in_Deutschland
#' License: Robert Koch-Institut (2025): Laborbestätigte Influenzafälle in Deutschland. Dataset. Zenodo. DOI:10.5281/zenodo.14619502.
"influenza_germany"
