# print method for package objects


#' Display a summary for a consonance test suite
#'
#' @keywords internal
#' @param x object of class consonance
#' @param ... other arguments ignored
#'
#' @method print consonance_suite
#'
#' @export
print.consonance_suite <- function(x, ...) {
  n.tests <- length(x$tests)
  n.label <- ifelse(n.tests==1, "test", "tests")
  msg <- paste0("Consonance test suite with ", n.tests, " ", n.label)
  #paste0("  level:\t", x$level))
  cat(paste0(paste(msg, collapse="\n"), "\n"))
}


#' Display a summary for a consonance test
#'
#' @keywords internal
#' @param x object of class consonance_test
#' @param ... other arguments ignored
#'
#' @method print consonance_test
#'
#' @export
print.consonance_test <- function(x, ...) {
  args <- ""
  if (length(x$args)>0) {
    args <- paste(names(x$args), collapse=", ")
  }
  msg <- c("Consonance test",
           paste0("  description:\t", x$desc),
           paste0("  type:\t\t'", x$type, "'"),
           paste0("  level:\t", x$level),
           paste0("  arguments:\t", args),
           paste0("  variable:\t", paste(x$var, collapse=", ")))
  cat(paste0(paste(msg, collapse="\n"), "\n"))
}

