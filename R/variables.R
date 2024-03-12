## code to prepare `variables` dataset goes here
#' Dataset with variable and concept descriptions for the IDB
#'
#' @title Dataset with variable and concept descriptions for the IDB
#' @description  Built-in dataset for use with the \code{idb_variables} and \code{idb_concepts} functions.
#' To access the data directly, issue the command \code{data(variables5)}.
#'
#' \itemize{
#'   \item \code{name}: The variable name; can be passed to the \code{idb5} function
#'   \item \code{label}: Description of the variable
#'   \item \code{concept}: The concept that a given variable belongs to
#'   \item \code{api_level}: The scope in which variables are needed for an API call, either `1year` or `5year`
#' }
#'
#' @docType data
#' @name variables
#' @usage data(variables)
#' @format A data frame with 134 rows and 4 columns
"variables"
