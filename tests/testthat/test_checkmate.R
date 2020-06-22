# test compatibility with checkmate

library("checkmate")

##############################################################################
# test suites with simple tests

# using checkmate assert
suite_assert <- consonance_suite() +
  consonance_assert("numeric", assert_numeric)

# using checkmate 'check' style functions
suite_check <- consonance_suite() +
  consonance_check("numeric", check_numeric)

# a poor configuration that uses check-style function but declares as 'assert'
suite_check_bad <- consonance_suite() +
  consonance_assert("numeric", check_numeric)

# using checkmate function with additional arguments
suite_percentage <- consonance_suite() +
  consonance_assert("numeric", assert_numeric, lower=0, upper=100)

suite_wrapped <- consonance_suite() +
  consonance_assert("numeric",
                    function(x) { assert_numeric(x, lower=0, upper=100) })


##############################################################################
# checkmate tests on simple data objects

test_that("add checkmate test without arguments", {
  expect_silent(test_consonance(4, suite_assert))
  expect_silent(test_consonance(-4, suite_assert))
  result <- test_consonance(4, suite_assert)
  expect_equal(result, 4)
})

test_that("potential pitfall mis-specifying check and asser", {
  expect_silent(test_consonance(4, suite_check_bad))
})

test_that("checkmate suite with arguments", {
  expect_silent(test_consonance(4, suite_percentage))
  expect_error(capture_output(test_consonance(120, suite_percentage)))
  expect_error(capture_output(test_consonance(-4, suite_percentage)))
})

test_that("checkmate suite with wrapped function", {
  expect_silent(test_consonance(4, suite_wrapped))
  expect_error(capture_output(test_consonance(120, suite_wrapped)))
})



# checkmate tests on object components (e.g. column in data frames)

test_that("add checkmate tests for columns in a data frame (passing)", {
  cons <- consonance_suite() +
    consonance_assert("a", assert_numeric, .var="a") +
    consonance_assert("b", assert_numeric, .var="b")
  d <- data.frame(a=1:4, b=11)
  expect_silent(test_consonance(d, cons))
})

test_that("add checkmate tests for columns in a data frame (fail)", {
  cons <- consonance_suite(logging.level="INFO") +
    consonance_test("a", assert_numeric, .var="a") +
    consonance_test("b", assert_numeric, .var="b")
  d <- data.frame(a=1:4, b=letters[1:4], stringsAsFactors=FALSE)
  expect_error(capture_output(test_consonance(d, cons)))
})

test_that("checkmate tests in range", {
  suite <-
    consonance_test("a", test_numeric, lower=0, upper=1, .var="a") +
      consonance_test("b", test_numeric, lower=0, upper=1, .var="b")
  df <- data.frame(a=seq(0.1, 0.9, by=0.2), b=0.5)
  expect_silent(test_consonance(df, suite))
})

