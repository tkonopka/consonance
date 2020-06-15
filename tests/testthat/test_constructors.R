# test for object constructors

source("simple_assertions.R")
library(log4r)


##############################################################################
# building a consonance suite

test_that("suite constructor create object structure", {
  suite <- consonance_suite()
  expect_is(suite, "consonance_suite")
  expect_true("tests" %in% names(suite))
})

test_that("suite constructor makes a logger", {
  suite <- consonance_suite()
  expect_true("logger" %in% names(suite))
  expect_true("log_info" %in% names(suite$logger))
  expect_true("log_warn" %in% names(suite$logger))
  expect_true("log_error" %in% names(suite$logger))
})

test_that("suite constructor checks logging level", {
  expect_error(consonance_suite(logging.level="AAA"))
  suite <- consonance_suite(logging.level="INFO")
  expect_equal(suite$logger$threshold, loglevel("INFO"))
})




##############################################################################
# building consonance tests

test_that("consonance_one checks test description", {
  # desc should be a string, not a number of vector
  expect_error(consonance_test(4, simple_pass), "desc")
  expect_error(consonance_test(c("a", "b"), simple_pass), "desc")
})

test_that("consonance_one matches .level", {
  test.error <- consonance_test("a", simple_pass, .level="err")
  expect_equal(test.error$level, "error")
  test.warning <- consonance_test("a", simple_pass, .level="warn")
  expect_equal(test.warning$level, "warning")
  expect_error(consonance_test("a", simple_pass, .level="abc"), "level")
})

