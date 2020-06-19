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
#'    consonance_test("x numeric", is.numeric)
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
  # re-assign the function environemnts so that the suite functions
  # can make use of variables defined within the object
  result.env <- list2env(result)
  for (i in seq_along(suite$tests)) {
    environment(result$consonance$tests[[i]]$fun) <- result.env
  }
  result
}


#' detach a suite from an object
#'
#' @export
#' @param object an object that includes a consonance suite
#'
#' @return consonance suite that is not attached to an object
#'
#' @examples
#'
#' # first attach a suite to a model
#' xy <- data.frame(x=1:10, y=1:10)
#' xy.lm <- lm(y~x, data=xy)
#' suite <- consonance_suite() +
#'    consonance_test("x numeric", is.numeric)
#' xy.model <- attach_consonance(xy.lm, suite)
#' xy.model$consonance
#'
#' # detach the suite from the model
#' suite_2 <- detach_consonance(xy.model)
#'
#' # note: to obtain the model without the suite, set the component to NULL
#' xy.model$consonance <- NULL
#'
detach_consonance <- function(object) {
  if (!"consonance" %in% names(object)) {
    stop("object does not contain a consonance suite\n")
  }
  result <- object$consonance
  for (i in seq_along(object$consonance$tests)) {
    environment(result$tests[[i]]$fun) <- globalenv()
  }
  result
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

