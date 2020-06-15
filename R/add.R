# build consonance objects


#' add a new test into a consonance test suite
#'
#' @param suite object of class consonance_suite
#' @param .desc character
#' @param .fun function, must accept at least one argument
#' @param ... other parameters, passed to constructor consonance_test
#' @export
#'
#' @return consonance suite
#'
#' @examples
#'
#'
add_test <- function(suite, .desc, .fun, ...) {

  # Implementation note - most arguments here start with .
  # This is deliberate so that they are not likely to interfere with
  # any parameters within "..." that are meant to be passed to .fun

  if (!is(suite, "consonance_suite")) {
    if ("consonance" %in% names(suite)) {
      suite$consonance <- add_test(suite$consonance, .desc, .fun, ...)
      return(suite)
    }
    .object <- substitute(suite)
    stop(paste0("suite '", .object, "' is not of class 'consonance_suite'"))
  }

  n <- length(suite$tests)
  new.test <- consonance_test(.desc=.desc, .fun=.fun, ...)
  suite$tests[[n+1]] <- new.test
  invisible(suite)
}


#' augment a consonance suite with a new consonance test
#'
#' @param suite object of class consonance_suite
#' @param test object of class consonance_test
#'
#' @export
#'
#' @return consonance suite
#'
#' @examples
#'
#'
add_consonance_test <- function(suite, test) {
  n <- length(suite$tests)
  suite$tests[[n+1]] <- test
  invisible(suite)
}


#' concatenate consonance tests into a consonance suite
#'
#' @param e1 object of class consonance or consonance_test
#' @param e2 object of class consonance_test
#' @export
#' @method + consonance_test
#'
#' @return a suite of consonance tests
"+.consonance_test" <- function(e1, e2) {
  is_e1 <- is(e1, "consonance_test")
  is_e2 <- is(e2, "consonance_test")
  add_test <- add_consonance_test
  if (is_e1 & is_e2) {
    suite <- consonance_suite()
    result <- add_test(add_test(suite, e1), e2)
  } else if (is_e1 & !is_e2){
    result <- add_test(e2, e1)
  } else {
    result <- add_test(e1, e2)
  }
  result
}

