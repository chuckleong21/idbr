#' @keywords internal
"_PACKAGE"

#' Access the Census International Data Base (IDB) from R
#'
#'
#'
#' @description This R package grants users access to the US Census Bureau's International Data Base (IDB) API, and returns queries as R data frames.  The IDB includes historical demographic data, current population estimates, and demographic projections to 2100 for countries of population 5,000 or greater that are recognized by the US Department of State.  Demographic indicators in the IDB include mid-year population; population counts by sex and age; and fertility, mortality, and migration variables such as net migration, infant mortality rates, and total fertility rates.  Future projections of these indicators are estimated using the cohort-component method.  For details on the US Census Bureau's methodology for producing population estimates, please visit \url{https://www2.census.gov/programs-surveys/international-programs/technical-documentation/methodology/idb-methodology.pdf}.
#'
#' @note This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau.
#' @seealso Census API terms of service: \url{https://www.census.gov/data/developers/about/terms-of-service.html}
#' @seealso US Census Bureau IDB API home page: \url{https://www.census.gov/data/developers/data-sets/international-database.html}
#'
#' @author Kyle Walker
#' @name idbr
## usethis namespace: start
#' @importFrom countrycode countrycode
#' @importFrom lifecycle is_present deprecated deprecate_stop
#' @import purrr
#' @importFrom jsonlite read_json
#' @importFrom dplyr as_tibble mutate select
#' @importFrom tidyr unnest_wider
#' @importFrom sf st_as_sf
#' @import dplyr
#' @import httr2
## usethis namespace: end
NULL
