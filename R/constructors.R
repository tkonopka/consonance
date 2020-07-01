# constructors for consonance_suite and consonance_test


#' create a new suite of consonance tests
#'
#' @param level character, determines when to halt execution
#' @param logging.level character, determines level of verbosity in logs.
#' This argument is over-ridden by setting a non-default logger.
#' @param log.file character, path for a log file, or leave NULL to log
#' to the console
#' The default "log4r" sets up a simple console logger using package log4r.
#' Setting a non-default logger over-rides values set in logging.level.
#' @export
#'
#' @return object of class consonance
#' @examples
#'
#' suite <- consonance_suite()
#' suite
#'
consonance_suite <- function(level=c("error", "warning"),
                             logging.level=c("WARN", "INFO", "ERROR"),
                             log.file=NULL) {
  level <- match.consonance.arg(level, "level")
  logging.level <- match.consonance.arg(logging.level, "logging.level")
  logger <- consonance_logger(logging.level, log.file)
  result <- list(
    tests = list(),
    level = level,
    logger = logger
  )
  class(result) <- c("consonance", "consonance_suite")
  result
}


#' constructor for a logger for a consonance suite
#'
#' This is primarily for use by the consonance_suite constructor. It
#' can also be used to replace an existing logger.
#'
#' @keywords internal
#' @noRd
#' @param logging.level character, level of loggin
#' @param log.file character or NULL, path to log file
#'
#' @return object with functions log_error, log_info, log_warn
#'
#' @examples
#'
#' # a consonance suite automatically create a logger with default settings
#' suite <- consonance_suite()
#' suite$logger$threshold
#'
#' # replace the existing logger by a manually constructed logger
#' suite$logger <- consonance_logger("INFO", log.file="my-log.txt")
#' suite$logger$threshold
#'
consonance_logger <- function(logging.level=c("WARN", "INFO", "ERROR"),
                              log.file=NULL) {
  logging.level <- as.character(logging.level)
  logging.level <- match.consonance.arg(logging.level, "logging.level")
  result <- log4r::logger(logging.level)
  if (!is.null(log.file)) {
    result <- log4r::logger(logging.level,
                            appenders=log4r::file_appender(log.file))
  }
  result$log_error <- log4r::error
  result$log_info <- log4r::info
  result$log_warn <- log4r::warn
  result
}


#' constructor for a logger object that does not output anything
#'
#' @keywords internal
#' @noRd
#'
#' @return object with functions log_error, log_info, log_warn
silent_logger <- function() {
  result <- list()
  result$log_error <- result$log_info <- result$log_warn <- function(...) {}
  result
}


#' create a new consonance assertion, check, or test
#'
#' This is a constructor which generates an object. The object can be
#' inspected using print or str.
#'
#' @keywords internal
#' @param .desc character, test description
#' @param .fun function
#' @param ... arguments to pass into .fun
#' @param .value.var character, component of a data object to test
#' @param .level character, action triggered by a failing test
#' @param .type character, type of function, refer to 'checkmate' documentation
#' for the distinction between 'assert', 'check', and 'test' functions.
#' @export
#'
#' @return object x, unchanged
#'
#' @examples
#'
#' # define custom a assertion or test
#' assert_positive <- function(x) {
#'   stopifnot(all(x>0))
#'   x
#' }
#' test_positive <- function(x) {
#'   all(x>0)
#' }
#'
#' # construct an assertion / test for vectors
#' vector_assertion <- consonance_assert("vec positive", assert_positive)
#' vector_assertion
#' vector_test <- consonance_test("vec_positive", test_positive)
#' vector_test
#'
#' # construct an assertion / test for a column ('val') in a data frame
#' df_assertion <- consonance_assert("df column positive", assert_positive,
#'                                   .var="val")
#' df_assertion
#' df_test <- consonance_test("def column positive", test_positive,
#'                            .var="val")
#' df_test
#'
consonance_one <- function(.desc, .fun, ...,
                           .var=NULL,
                           .type=c("test", "assert", "check"),
                           .level=c("error", "warning", "info")) {
  .level <- match.consonance.arg(.level, "level")
  .type <- match.consonance.arg(.type, "type")
  if (!is(.desc, "character") | length(.desc) != 1) {
    stop("'desc' must be a single character string")
  }
  .args <- force(list(...))
  result <- list(desc=.desc,
                 fun=.fun,
                 args=.args,
                 var=.var,
                 type=.type,
                 level=.level)
  class(result) <- c("consonance", "consonance_test")
  invisible(result)
}


#' @rdname consonance_one
#' @export
consonance_assert <- function(.desc, .fun, ..., .type="assert") {
  consonance_one(.desc, .fun, ..., .type=.type)
}


#' @rdname consonance_one
#' @export
consonance_check <- function(.desc, .fun, ..., .type="check") {
  consonance_one(.desc, .fun, ..., .type=.type)
}


#' @rdname consonance_one
#' @export
consonance_test <- function(.desc, .fun, ..., .type="test") {
  consonance_one(.desc, .fun, ..., .type=.type)
}

