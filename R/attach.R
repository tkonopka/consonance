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
#'    consonance_test("x numeric", is.numeric, .var="x")
#' xy.model <- attach_consonance(xy.lm, suite)
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
  result <- object
  result$consonance <- suite
  result
}


#' extract a consonance object from a composite object
#'
#' @noRd
#' @keywords internal
#' @param obj list-like object
#' @param parent environemt
#'
#' @return list with consonance suite and an environment
get_consonance_suite_env <- function(obj, parent=emptyenv()) {
  if (is(obj, "consonance_suite"))
    return(list(suite = obj, env = NULL))
  if (is(obj, "consonance_test"))
    return(list(suite= consonance_suite() + obj, env = NULL))
  if ("consonance" %in% names(obj)) {
    result <- obj$consonance
    if (is(result, "consonance_suite"))
      return(list(suite = result,
                  env = list2env(obj, parent=parent)))
  }
  NULL
}

