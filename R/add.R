# build consonance objects


#' add a new test into a consonance test suite
#'
#' @param suite object of class consonance_suite
#' @param .desc character
#' @param .fun function, must accept at least one argument
#' @param ... other parameters, passed to constructor consonance_test
#'
#' @return consonance suite
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
#' @return consonance suite
add_consonance_test <- function(suite, test) {
  n <- length(suite$tests)
  suite$tests[[n+1]] <- test
  invisible(suite)
}


#' concatenate consonance tests into a consonance suite
#'
#' @param e1 consonance_suite or consonance_test
#' @param e2 consonance_suite or consonance_test
#' @export
#' @method + consonance
#'
#' @return a suite of consonance tests
#'
#' @examples
#'
#' # declare two consonance tests for a hypothetical
#' # data frame with columns "x" and "y"
#' test_x <- consonance_test("x numeric", is.numeric, .var="x")
#' test_y <- consonance_test("y numeric", is.numeric, .var="y")
#'
#' # create a suite by adding two tests to an empty suite
#' suite_2 <- consonance_suite() +  test_x + test_y
#'
#' # create a suite by adding two tests together
#' # (tests automatically grouped into a suite)
#' suite_2 <- test_x + test_y
#'
#' # create a large suite by concatenating two suite together
#' suite_4 <- suite_2 + suite_2
#' length(suite_4$tests)
#' # (note that + really means concatentation and tests can be redundant)
#'
"+.consonance" <- function(e1, e2) {
  is_e1 <- is(e1, "consonance_test")
  is_e2 <- is(e2, "consonance_test")
  add_test <- add_consonance_test
  if (is_e1 & is_e2) {
    suite <- consonance_suite()
    result <- add_test(add_test(suite, e1), e2)
  } else if (is_e1 & !is_e2){
    result <- add_test(e2, e1)
  } else if (!is_e1 & is_e2) {
    result <- add_test(e1, e2)
  } else {
    # two suites
    result <- e1
    result$tests <- c(e1$tests, e2$tests)
  }
  result
}

