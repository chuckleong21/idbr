#' Get Data from the US Census Bureau's International Data Base API
#'
#' @param country A country name or vector of country names. Can be specified as ISO-2 codes
#'                as well. Use \code{country = "all"} to request all countries available in
#'                the IDB.
#' @param year A single year or vector of years for which you'd like to request data.
#' @param variables A character string or vector of variables representing data you would like
#'                  to request.  If you are specifying an age or sex subset, this should be kept as                     \code{NULL} as the function will return data from the 1-year-of-age IDB API.
#'                  If filtering by age or sex, should be NULL.
#' @param concept Variables in the IDB are organized by concepts; if specified, request all
#'                variables for a given concept.  Use \code{idb_concepts()} to view
#'                available concepts.
#' @param age A vector of ages for which you would like to request population data. If specified,                 will return data from the 1-year-age-band IDB API.  Should not be used when
#'            \code{variables} is not \code{NULL}.
#' @param sex One or more of "both", "male", or "female". If specified, will return data
#'            from the 1-year-age-band IDB API.  Should not be used when \code{variables}
#'            is not \code{NULL}.
#' @param geometry If \code{TRUE}, returns country simple feature geometry along with your data
#'                 which can be used for mapping. Geometry is obtained using the rnaturalearthdata
#'                 R package.
#' @param resolution one of \code{"low"} for lower-resolution (less-detailed) geometry, or          #'                   \code{"high"} for more detailed geometry.  It is recommended to use the low-
#'                   resolution geometries for smaller-scale (e.g. world) mapping, and the
#'                   higher-resolution geometries for medium-scale (e.g. regional) mapping.
#' @param api_key Your Census API key.  Can be supplied as part of the function call or
#'                set globally with the \code{idb_api_key()} function. If you are a tidycensus
#'                user with your API key already stored, \code{get_idb()} will pick up the
#'                API key from there, and no further action from you is required.
#'
#' @return A tibble or sf tibble of data from the International Data Base API.
#' @export
#'
#' @examples \dontrun{
#' # Get data from the 1-year-age-band dataset by sex for China from
#' # 1990 through 2021
#'
#' library(idbr)
#'
#' china_data <- get_idb(
#'   country = "China",
#'   year = 1990:2021,
#'   age = 1:100,
#'   sex = c("male", "female")
#'  )
#'
#' # Get data on life expectancy at birth for all countries in 2021 and
#' # make a map with ggplot2
#'
#' library(idbr)
#' library(tidyverse)
#'
#' lex <- get_idb(
#'   country = "all",
#'   year = 2021,
#'   variables = c("name", "e0"),
#'   geometry = TRUE
#' )
#
#' ggplot(lex, aes(fill = e0)) +
#'   theme_bw() +
#'   geom_sf() +
#'   coord_sf(crs = 'ESRI:54030') +
#'   scale_fill_viridis_c() +
#'   labs(fill = "Life expectancy at birth (2021)")
#' }
get_idb <- function(country,
                    year,
                    variables = NULL,
                    concept = deprecated(),
                    age = NULL,
                    sex = NULL,
                    geometry = FALSE,
                    resolution = c("low", "high"),
                    api_key = NULL) {
  if(is_present(concept)) {
    deprecate_stop("1.1.0", 'idbr::get_idb(concept = )', 'idbr::get_idb(variables = )')
  }

  if(is.null(api_key) & !idb_api_key_available()) {
    stop('A Census API key is required.  Obtain one at https://api.census.gov/data/key_signup.html,
         and then supply the key to the `idb_api_key` function to use it throughout your idbr session.', call. = FALSE)
  }

  base_url <- "https://api.census.gov/data/timeseries/idb/"
  api_level <- NULL; api_level_default <- "1year"
  sex_value <- 0:2; names(sex_value) <- c("both", "male", "female")


  if (!is.null(age) || !is.null(sex)) {
    api_level <- api_level_default
  } else {
    api_level <- "5year"
  }

  # country api param is now used by for=xxx
  country <- map_chr(country, \(x) {
    if(nchar(x) > 2) {
      countrycode(x, "country.name", "iso2c")
    } else x
  })

  country <- paste(country, collapse = ",")
  country <- sprintf("genc+standard+countries+and+areas:%s", country)

  vars_default <- toupper(c("name", "genc", "pop"))

  if(is.null(variables)) {
    variables <- vars_default
  } else {
    api_conflict(variables)

    variables <- map(vars_default, \(x) {
      if(!x %in% variables) {
        append(variables, x)
      } else variables
    }) %>%
      reduce(c) %>%
      unique()
  }

  if(is.null(age)) {
    age <- 0:100
  }

  if(is.null(sex)) {
    sex <- sex_value["both"]
  } else {
    sex <- sex_value[sex]
  }

  if(api_level == "5year") {
    header <- c("NAME", "GENC", "POP", "YR", "AREAS")
    q <- list(
      "get" = paste(variables, sep = ","),
      "YR" = paste(year, sep = ","),
      "for" = I(country),
      "key" = api_key
    )
  } else {
    header <- c("NAME", "GENC", "POP", "YR", "AGE", "SEX", "AREAS")
    q <- list(
      "get" = paste(variables, sep = ","),
      "YR" = paste(year, sep = ","),
      "AGE" = age,
      "SEX" = sex,
      "for" = I(country),
      "key" = api_key
    )
  }

  req <- request(base_url) %>%
    req_url_path_append(api_level) %>%
    req_url_query(!!!q, .multi = "comma") %>%
    req_perform()

  resp_df <- resp_body_json(req, simplifyVector = TRUE) %>%
    as.data.frame() %>%
    slice(-1) %>%
    set_names(header) %>%
    as_tibble()

  if(api_level == "1year") {
    resp_df$SEX <- names(sex_value)[match(resp_df$SEX, sex_value)]
    resp_df <- mutate(resp_df, across(c(.data$POP, .data$YR, .data$AGE), as.numeric))
  } else {
    resp_df <- mutate(resp_df, across(c(.data$POP, .data$YR), as.numeric))
  }

  # if (api_request$status_code != "200") {
  #   stop(sprintf("Your data request has errored.  The error message returned is %s",
  #                req_content))
  # }
  #
  # req_frame <- data.frame(jsonlite::fromJSON(req_content), stringsAsFactors = FALSE)
  #
  # colnames(req_frame) <- req_frame[1, ]
  #
  # req_frame <- req_frame[-1, ]
  #
  # rownames(req_frame) <- NULL
  #
  # string_cols <- names(req_frame) %in% c("NAME", "GEO_ID")
  #
  # req_frame[!string_cols] <- apply(req_frame[!string_cols], 2, function(x) as.numeric(x))
  #
  # req_tibble <- as_tibble(req_frame)
  #
  # names(req_tibble) <- tolower(names(req_tibble))
  #
  # req_tibble$geo_id <- stringr::str_sub(req_tibble$geo_id, start = -2)
  #
  # out_tibble <- select(req_tibble, code = geo_id, year = yr, everything())
  #
  # if ("sex" %in% names(out_tibble)) {
  #   out_tibble$sex <- recode(out_tibble$sex,
  #     `0` = "Both",
  #     `1` = "Male",
  #     `2` = "Female"
  #   )
  # }
  #
  # if (!is.null(country_vector)) {
  #   out_tibble <- out_tibble %>%
  #     filter(code %in% country_vector)
  # }
  #
  if (geometry) {
    resolution <- match.arg(resolution)

    sf_df <- map2(c("low", "high"), list(countries50, countries50), \(x, y) {
      select(st_as_sf(y), code = iso_a2)
    }) %>%
        set_names(c("low", "high"))

    left_join(resp_df, sf_df[[resolution]], join_by(.data$AREAS == code))
  } else {
    resp_df
  }


    # if (resolution == "low") {
    #   geom <- countries110 %>%
    #     st_as_sf() %>%
    #     select(code = iso_a2)
    # } else {
    #   geom <- idbr::countries50 %>%
    #     st_as_sf() %>%
    #     select(code = iso_a2)
    # }
  #
  #   # Should be left join if country is all, to make missing countries NULL
  #   # Not perfect yet, e.g. for regional mapping with missing countries
  #   if (all(country == "all")) {
  #
  #     joined_tbl <- geom %>%
  #       left_join(out_tibble, by = "code")
  #
  #   } else {
  #
  #     joined_tbl <- geom %>%
  #       inner_join(out_tibble, by = "code")
  #
  #   }
  #
  #   return(joined_tbl)
  # } else {
  #   return(out_tibble)
  # }

}
