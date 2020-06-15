# consonance testing


#' two functions used in tryCatch blocks
#'
#' @noRd
#' @keywords internal
#' @param x character
#'
#' @return list with message and integer level value
warning_fun <- function(x) {
  list(msg=x, success=FALSE, result="warning")
}
error_fun <- function(x) {
  list(msg=x, success=FALSE, result="error")
}


#' helper object of the same format as produced by warning_fun and
#' error_fun, but designating a test that passes
#' @noRd
#' @keywords internal
pass_output <- list(msg="", success=TRUE, result="pass")


#' evaluate a test configuration on data
#'
#' ("evaluate" is intended as "test/check", not as "eval")
#'
#' @noRd
#' @keywords internal
#' @param conf a consonance_test
#' @param x data object
#' @param logger object for logging
#'
#' @return list with components msg, success, level
evaluate_test <- function(conf, x, logger) {
  var <- conf$var
  type <- conf$type
  if (is.null(var)) {
    args <- c(list(x), conf$args)
  } else {
    args <- c(list(x[[var]]), conf$args)
  }
  log_prefix <- "consonance test "

  # perform the test
  result <- tryCatch(
    {
    output <- do.call(conf$fun, args)
    if (type=="check" & is.character(output)) {
      output <- error_fun(output)
    } else if (type=="test" & !identical(output, TRUE)) {
      output <- error_fun(output)
    } else {
      output <- pass_output
    }
    output
  }, warning = warning_fun, error = error_fun )

  # log the output
  if (result$success) {
    logger$log_info(paste0(log_prefix, "pass:\t", conf$desc),
                    logger=logger)
  } else {
    log_fun <- logger$log_error
    if (result$result=="warning" | conf$level=="warning") {
      log_fun <- logger$log_warn
      result$result <- "warning"
    }
    log_fun(paste0(log_prefix, result$result, ":\t", conf$desc), logger=logger)
  }
  result
}


#' determine if a suite as a whole succeeded
#'
#' ("evaluate" is intended as "test/check", not as "eval")
#'
#' @noRd
#' @keywords internal
#' @param results list with individual test results
#' @param logger logger object
#'
#' @return number of tests pass, number of warnings or worse, number of errors
evaluate_suite <- function(results, logger) {
  labels <- vapply(results, function(x) { x$result }, character(1))
  n.pass <- sum(labels=="pass")
  n.warn <- sum(labels=="warning")
  n.error <- sum(labels=="error")
  log_fun <- logger$log_info
  if (n.warn > 0)
    log_fun <- logger$log_warn
  if (n.error > 0)
    log_fun <- logger$log_error
  msg <- paste0(n.pass, " OK, ",
               n.warn, " warning", ifelse(n.warn==1, "", "s"), ", ",
               n.error, " error", ifelse(n.error==1, "", "s"))
  log_fun(logger=logger, paste0("consonance suite:\t\t", msg))
  # note output for warnings is actually - warnings or worse
  c(pass = n.pass,
    warning = n.warn + n.error,
    error = n.error)
}


#' check consonance of an object with a suite of tests
#'
#' The function always returns its primary input and should be invoked
#' for its side effects. It stops when a data 
#' 
#' @param x data object
#' @param suite object of class consonance
#' @param skip logical, set to TRUE to skip all tests
#' @param logging.level character, code to indicate logging level,
#' leave NULL to use the logging level specified in suite constructor
#' @param log.file character, path to log file, or leave NA to follow
#' the logger set up within suite
#' @export
#'
#' @return object x, unchanged
#'
#' @examples
#'
#'
#'
test_consonance <- function(x, suite,
                            skip=FALSE,
                            logging.level=NULL,
                            log.file=NA) {

  # get a suite object - either from argument or an attachment
  suite = get_attached_consonance(suite)
  if (!is(suite, "consonance_suite"))
    stop(paste0("object '", suite, "' is not a consonance suite\n"))
  # establish a logger for this group of tests
  logger <- suite$logger
  if (!is.na(log.file)) {
    logger <- consonance_logger(suite$logger$threshold, log.file)
  }
  if (!is.null(logging.level)) {
    logger$threshold <- loglevel(logging.level)
  }
  if (skip) {
    logger$log_info("consonance suite: skipping", logger=logger)
    return(invisible(x))
  }

  # perform & log individual tests, then evaluate suite as a whole
  result <- lapply(suite$tests, evaluate_test, x=x, logger=logger)
  final <- evaluate_suite(result, logger)

  if (final[suite$level]>0) {
    stop("consonance suite\n", call.=FALSE)
  }

  invisible(x)
}

