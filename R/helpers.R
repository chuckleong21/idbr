# Grab data from the API, once the URL is formatted correctly

# load_data <- function(api_call) {
#
#   request <- httr::GET(api_call)
#
#   cont <- httr::content(request, as = 'text')
#
#   if (jsonlite::validate(cont) == FALSE) {
#
#     stop("You have supplied an invalid query.  Consider revising your year selection (IDB data begin at 1950, and are not available for many countries until 1990), or use `idb_variables()` or `idb_concepts()` to view valid choices of variables and concepts.", call. = FALSE)
#
#   } else {
#
#     df <- data.frame(jsonlite::fromJSON(cont), stringsAsFactors = FALSE)
#
#     colnames(df) <- df[1, ]
#
#     df <- df[-1, ]
#
#     rownames(df) <- NULL
#
#     string_cols <- names(df) %in% c("NAME", "FIPS")
#
#     df[!string_cols] <- apply(df[!string_cols], 2, function(x) as.numeric(x))
#
#     return(dplyr::as_tibble(df))
#
#   }
# }


idb_api_key_available <- function() {
  api_key <- NULL

  in_env <- map_lgl(c('IDB_API' = 'IDB_API', 'CENSUS_API_KEY' = 'CENSUS_API_KEY'),
                           \(x) Sys.getenv(x) != '')

  if (any(in_env)) {

    if(length(which(in_env)) > 1) {
      api_key <- Sys.getenv(names(in_env)[which(in_env)[1]])
    } else {
      api_key <- Sys.getenv(names(in_env)[which(in_env)])
    }

  TRUE

  } else FALSE
}

api_conflict <- function(vars) {
  lvls <- variables %>%
    filter(.data$name %in% vars) %>%
    pull(.data$api_level) %>%
    unique()
  if(length(lvls) > 1) {
    stop('API variables conflict. Supply the variables under `api_levels, Run `idb_variables()` to see more.')
  }
}
