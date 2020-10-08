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
#' @param model list or environment object, this can be passed
#' to consonance_test function as argument .model
#'
#' @return list with components msg, success, level
evaluate_test <- function(conf, x, logger, model=NULL) {
  var <- conf$var
  type <- conf$type
  f <- conf$fun
  if (is.null(var)) {
    args <- c(list(x), conf$args)
  } else {
    args <- c(list(x[[var]]), conf$args)
  }
  if (".model" %in% names(formals(f))) {
    args <- c(args, list(.model=model))
  }
  log_prefix <- "consonance "

  # perform the test
  result <- tryCatch(
    {
    output <- do.call(f, args)
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
    if (result$result == "warning" | conf$level == "warning") {
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
#' @param x.name character, name of data object
#' @param suite.name character, name of suite object
#'
#' @return number of tests pass, number of warnings or worse, number of errors
evaluate_suite <- function(results, logger, x.name, suite.name) {
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
  log_fun(logger=logger, paste0("consonance object:\t", x.name))
  log_fun(logger=logger, paste0("consonance suite:\t", suite.name))
  log_fun(logger=logger, paste0("consonance result:\t", msg))
  # note output for warnings is actually - warnings or worse
  c(pass = n.pass,
    warning = n.warn + n.error,
    error = n.error)
}


#' prepare a logger based on suite settings or run-time arguments
#'
#' @keywords internal
#' @noRd
#' @param suite object of class consonance
#' @param logging.level character, NULL, or NA
#' @param logging.file character, NULL, or NA
#'
#' @return list with functions log_info, log_warn, log_error
get_consonance_logger <- function(suite, logging.level=NA, log.file=NA) {
  logger <- suite$logger
  if (!identical(log.file, NA)) {
    logger <- consonance_logger(suite$logger$threshold, log.file)
  }
  if (!is.na(logging.level)) {
    if (logging.level=="NONE") {
      logger <- silent_logger()
    } else {
      logger$threshold <- loglevel(logging.level)
    }
  }
  logger
}

#' check consonance of an object with a suite of tests
#'
#' The function always returns its primary input and should be invoked
#' for its side effects. It stops when a data 
#' 
#' @param x data object
#' @param suite object of class consonance
#' @param level character, strictness level, use "warning" or "error" to
#' determine the minimal criteria to halt execution
#' @param skip logical, set to TRUE to skip all tests
#' @param skip.action character, determines what happens when skip=TRUE.
#' Action 'log' sends an INFO message to the logger, 'none' is silent.
#' @param logging.level character, code to indicate logging level;
#' leave NA to use the logging level specified in suite constructor;
#' set NULL to avoid logging altogether
#' @param log.file character, path to log file, or leave NA to follow
#' the logger set up within suite
#' @param x.name character, (used by validate_batch), leave NA during
#' standard usage
#' @param suite.name character, (used by validate_batch), leave NA during
#' standard usage
#' @export
#'
#' @return object x, unchanged
#'
#' @examples
#'
#' # create a test or a suite with a test
#' test_1 <- consonance_test("a numeric", is.numeric)
#' suite_1 <- consonance_suite() + test_1
#'
#' # applying the test or suite on numeric data
#' # should execute without visible output
#' validate(c(1, 2, 3), test_1)
#' validate(c(1, 2, 3), suite_1)
#'
#' # applying on non-numeric data should produce messages and stop execution
#' # validate(letters, test_1)
#' # validate(letters, suite_1)
#'
validate <- function(x, suite, level=NA,
                     skip=FALSE, skip.action=c("log", "none"),
                     logging.level=c(NA, "INFO", "WARN", "ERROR", "NONE"),
                     log.file=NA,
                     x.name=NA, suite.name=NA) {

  if (skip & match.arg(skip.action) == "none")
    return(invisible(x))
  logging.level <- match.arg(logging.level)

  # support batch testing
  if (missing(suite) & all(c("object", "suite") %in% names(x))) {
    result <- validate_batch(as.character(x$object),
                             as.character(x$suite),
                             envir=parent.frame(),
                             level=level, skip=skip,
                             skip.action=skip.action,
                             logging.level=logging.level,
                             log.file=log.file)
  } else {
    # get a suite object - either from argument or an attachment
    if (is.na(x.name)) x.name <- substitute(x)
    if (is.na(suite.name)) suite.name <- substitute(suite)
    result <- validate_inner(x, x.name=x.name,
                             suite, suite.name=suite.name,
                             level=level, skip=skip,
                             logging.level=logging.level,
                             log.file=log.file)
  }

  if (result["stop"]) stop("consonance\n", call.=FALSE)
  invisible(x)
}


#' the part of validate that performes consonance tests with one
#' object and one suite
#'
#' @rdname validate
#' @keywords internal
#' @param ... other arguments, not used
#'
#' @return vector with pass, warning, error, and stop counts
validate_inner <- function(x, suite, level=NA, skip=FALSE,
                           logging.level=NA, log.file=NA,
                           x.name=NA, suite.name=NA, ...) {

  suite_env <- get_consonance_suite_env(suite)
  if (is.null(suite_env))
    stop(paste0("object '", substitute(suite.name),
                "' is not a consonance suite\n"))
  suite <- suite_env$suite
  # get a logger - either the one from the suite or adjust based on args
  logger <- get_consonance_logger(suite, logging.level, log.file)

  if (skip) {
    logger$log_info("consonance suite: skipping", logger=logger)
    return(c(stop=FALSE))
  }

  logger$log_info(paste0("validate(", x.name, ", ", suite.name, ")"),
                  logger=logger)

  # perform & log individual tests, then evaluate suite as a whole
  result <- lapply(suite$tests, evaluate_test,
                   x=x, logger=logger, model=suite_env$env)
  final <- evaluate_suite(result, logger, x.name, suite.name)

  if (is.na(level))
    level <- suite$level
  final["stop"] <- as.integer(final[level]>0)
  final
}

#' applies validate() to a series of objects and consonance suites
#'
#' @keywords internal
#' @param objects, character vector with data object names
#' @param suies, character vector with suite names
#' @param envir, environment from which to fetch data and suites
#' @param ... other arguments passed to validate()
#'
#' @return vectoring with pass, warning, error, stop counts.
#' The most reliable value is "stop" that signals whether,
#' overall, the batch generated a stop condition
validate_batch <- function(objects, suites, envir, ...) {
  result <- c(pass=0, warning=0, error=0, stop=0)
  for (i in seq_along(objects)) {
    tryCatch({
      oi <- get(objects[i], envir=envir)
      si <- get(suites[i], envir=envir)
      result <- result + validate_inner(oi, si, ...,
                                         x.name=objects[i],
                                         suite.name=suites[i])
    },
      error=function(x) {
        result <<- result + c(pass=0, warning=0, error=1, stop=1)
      }
    )
  }
  result
}

