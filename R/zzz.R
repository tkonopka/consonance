# helper functions for use within the package only


#' capture allowed values for various arguments
#'
#' @noRd
#' @keywords internal
consonance_defaults <- list(
  logging.level = as.character(formals(consonance_suite)$logging.level)[-1],
  level = as.character(formals(consonance_suite)$level)[-1],
  type = as.character(formals(consonance_one)$.type)[-1]
)


#' variation on match.arg
#'
#' this emits a more informative error message
#' it also checks against consonance_defaults
#'
#' @param arg character, value supplied by user
#' @param label character, identifier to a list that holds allowed choices
#'
#' @return value converted to the allowed values
match.consonance.arg <- function(arg, label) {
  arg.name <- substitute(arg)
  tryCatch({
             result <- match.arg(arg[1], consonance_defaults[[label]])
           },
           error = function(e) {
             stop(paste0("invalid value for argument ",  arg.name, "\n"),
                  call.=FALSE)
           })
  result
}

