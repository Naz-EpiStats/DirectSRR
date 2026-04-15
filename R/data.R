#' Example dataset
#'
#' Simulated aggregate event data for three study groups across three age
#' strata, intended for use in package examples and tests.
#'
#' @format A data frame with 9 rows and 5 columns:
#' \describe{
#'   \item{`age_group`}{Age stratum: `"18-39"`, `"40-64"`, or `"65+"`.}
#'   \item{`group`}{Study group: `"A"`, `"B"`, or `"C"`.}
#'   \item{`events`}{Integer event count for this stratum and group.}
#'   \item{`population`}{Person-time or population size.}
#'   \item{`stdpop`}{Standard population weight for the age stratum
#'     (proportions: 0.4, 0.4, 0.2).}
#' }
#'
#' @source Simulated; no real patients.
#'
#' @examples
#' data(directsrr_ExampleData)
#' head(directsrr_ExampleData)
"directsrr_ExampleData"
