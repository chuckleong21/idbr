# idb_concepts <- function() {
#
#   print(unique(idbr::variables5$Concept))
#
# }

#' Access IDB variables through api
#'
#' @return A tibble of 134 rows and 4 columns
#' @export
#'
#' @examples
#' idb_variables()
idb_variables <- function() {
  query_params <- c("1year", "5year")
  variables_json <- sprintf("https://api.census.gov/data/timeseries/idb/%s/variables.json", query_params)

  purrr::map2(query_params, variables_json, \(q, v) {
    r <- tryCatch(read_json(v))
    while(inherits(r, "try-error")) {
      r <- tryCatch(read_json(v))
    }

    r %>%
      as_tibble() %>%
      unnest_wider(col = variables) %>%
      mutate(name = map(r, names)[[1]],
                    api_level = q) %>%
      select(.data$name, .data$label, .data$concept, .data$api_level)
  }) %>%
    list_rbind()
}


