# tests for 'consonance', i.e. running a consonance test suite on data

source("simple_assertions.R")

# test suites with simple tests (for this file only)
suite_default <- consonance_suite() +
  consonance_assert("chars", simple_is_character)
suite_verbose <- consonance_suite(logging.level="INFO") +
  consonance_assert("chars", simple_is_character)

# test suites using assert / check / test
suite_test <- consonance_suite() +
  consonance_test("chars", is.character)
suite_check <- consonance_suite() +
  consonance_check("chars", simple_character_check)


# simple data objects (for this file only)
abc <- letters[1:3]


##############################################################################
# handling of inputs

test_that("consonance checks suite class", {
  expect_error(test_consonance(1:10, 10), "not a consonance suite")
})


##############################################################################
# Core functionality

test_that("test_consonance with assert / check / test syntax", {
  expect_silent(test_consonance(abc, suite_default))
  expect_silent(test_consonance(abc, suite_check))
  expect_silent(test_consonance(abc, suite_test))
  expect_error(capture_output(test_consonance(1:3, suite_verbose)))
  expect_error(capture_output(test_consonance(1:3, suite_check)))
  expect_error(capture_output(test_consonance(1:3, suite_test)))
})

test_that("test_consonance with verbose output", {
  expect_silent(test_consonance(abc, suite_default))
  # message should include INFO field and name of test
  expect_output(test_consonance(abc, suite_verbose), "INFO")
  expect_output(test_consonance(abc, suite_verbose), "chars")
})

test_that("test_consonance with verbose output", {
  suite <- consonance_suite() +
    consonance_test("all even", function(x) {all(x%%2==0)}) +
    consonance_test("above 0", function(x) {all(x > 0)})
  expect_silent(test_consonance(c(2,4,6), suite))
  expect_error(capture_output(test_consonance(c(2,4,5), suite)))
  expect_error(capture_output(test_consonance(c(2,-4,6), suite)))
})

test_that("test_consonance of data frames", {
  test_square <- function(x) {
    xdim <- dim(x)
    xdim[1] == xdim[2]
  }
  suite <- consonance_suite() + consonance_test("square", test_square)
  m_square <- matrix(1:9, ncol=3)
  m_long <- matrix(1:10, ncol=2)
  expect_silent(test_consonance(m_square, suite))
  expect_error(capture_output(test_consonance(m_long, suite)))
})

test_that("test_consonance of data frame columns", {
  suite_a <- consonance_suite() +
    consonance_assert("above 0", simple_above, .var="a")
  suite_b <- consonance_suite() +
    consonance_assert("above 0", simple_above, .var="b")
  df <- data.frame(a=5:10, b=-seq(5:10))
  expect_silent(test_consonance(df, suite_a))
  expect_error(capture_output(test_consonance(df, suite_b)))
})

test_that("signals level=warning and level=errors differently", {
  suite_e <- consonance_suite() +
    consonance_assert("in [0, 100]", simple_percentage, .level="error")
  suite_w <- consonance_suite() +
    consonance_assert("in [0, 100]", simple_percentage, .level="warning")
  # suite e should have errors
  expect_error(capture_output(test_consonance(-4, suite_e)))
  expect_error(capture_output(test_consonance(-4, suite_e)))
  # suite w should have warnings
  expect_output(test_consonance(-4, suite_w), "WARN")
  expect_output(test_consonance(-4, suite_w), "warning")
  # but suite w should not generate errors
  expect_failure(
    expect_output(test_consonance(-4, suite_w), "ERROR")
  )
})

test_that("signals R warning() as WARN", {
  suite_spearman <- consonance_suite() +
    consonance_assert("spearman >0", simple_spearman, col1="x", col2="y")
  good_df <- data.frame(x=seq(1, 20)+rnorm(10), y=seq(1, 20)+rnorm(20))
  bad_df <- data.frame(x=rep(1:5, each=2),
                       y=rep(1:5, each=2))
  # columns in good_df are well correlated
  expect_silent(test_consonance(good_df, suite_spearman))
  # column in bad_df have ties, which should trigger an R warning
  expect_output(test_consonance(bad_df, suite_spearman), "WARN")
})


##############################################################################
# logging into a file

test_that("suite constructor logs into a file", {
  file_path <- tempfile()
  suite_file <- consonance_suite(log.file=file_path) +
    consonance_assert("numeric", assert_numeric)
  expect_false(file.exists(file_path))
  expect_error(test_consonance("abc", suite_file))
  expect_true(file.exists(file_path))
  file_content <- readLines(file_path)
  expect_true(grepl("ERROR", file_content)[1])
  unlink(file_path)
  expect_false(file.exists(file_path))
})

test_that("test_consonance can log into a file on request", {
  file_path <- tempfile()
  expect_false(file.exists(file_path))
  # one run of test_consonance should generate 2 lines in log
  expect_error(test_consonance(1:3, suite_default, log.file=file_path))
  expect_true(file.exists(file_path))
  expect_equal(length(readLines(file_path)), 2)
  # a second run without log.file should not generate additional lines
  expect_error(capture_output(test_consonance(1:3, suite_default)))
  expect_equal(length(readLines(file_path)), 2)
  # a third run with log.file should add two more lines
  expect_error(test_consonance(1:3, suite_default, log.file=file_path))
  expect_equal(length(readLines(file_path)), 4)
  unlink(file_path)
  expect_false(file.exists(file_path))
})


##############################################################################
# run-time tuning

test_that("test_consonance can skip all tests", {
  # tests stop only on non-character inputs
  expect_silent(test_consonance(abc, suite_default))
  expect_error(capture_output(test_consonance(1:3, suite_default)))
  # but tests succeed when skip is TRUE
  expect_silent(test_consonance(1:3, suite_default, skip=TRUE))
  # when skip is TRUE, the output should still be equal to the input
  result <- test_consonance(1:3, suite_default, skip=TRUE)
  expect_equal(result, 1:3)
})

test_that("test_consonance can use different logging level", {
  # suite_default should be silent on normal inputs
  expect_silent(test_consonance(abc, suite_default))
  # but can produce output by setting logging level manually
  expect_output(test_consonance(abc, suite_default, logging.level="INFO"))
})

