# using a consonance suite as a sub-object in models, lists, etc.


#' attach a consonance object to an existing model
#'
#' @param object list-like object, for example a lm or glm model
#' @param suite object of class consonance_suite
#'
#' @export
#'
#' @return list-like object with a consonance component
#'
#' @examples
#'
#' # create a regression model and a suite of tests
#' xy <- data.frame(x=1:10, y=1:10)
#' xy.lm <- lm(y~x, data=xy)
#' suite <- consonance_suite() +
#'    consonance_assert("x numeric", assert_numeric)
#' xy.model = attach_consonance(xy.lm, suite)
#'
#' # preview that the test suite is attached to the model
#' xy.model$consonance
#'
#' # use the model as-if it were a test suite
#' test_consonance(xy, xy.model)
#'
attach_consonance <- function(object, suite) {
  if (is.null(object)) {
    stop("object cannot be NULL\n")
  }
  if (!is(suite, "consonance_suite")) {
    stop("suite is not a suite of consonance tests\n")
  }
  if ("consonance" %in% names(object)) {
    warning(paste0("object already includes consonance checks\n",
                   "existing check will be replaced by new test suite"))
  }
  object$consonance <- suite
  object
}



#' extract a consonance object from a composite object
#'
#' @noRd
#' @keywords internal
#' @param obj list-like object
#'
#' @return consonance suite, or character to signal bad input
get_consonance_suite <- function(obj) {
  if (is(obj, "consonance_suite"))
    return(obj)
  if (is(obj, "consonance_test"))
    return(consonance_suite() + obj)
  if ("consonance" %in% names(obj)) {
    result <- obj$consonance
    if (is(result, "consonance_suite"))
      return(result)
  }
  substitute(obj)
}

