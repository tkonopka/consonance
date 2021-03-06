# tests for 'consonance', i.e. running a consonance test suite on data

source("simple_assertions.R")

# single assertion / check / test
single_test <- consonance_test("chars", is.character)
single_assert <- consonance_assert("chars", simple_is_character)
single_check <- consonance_check("chars", simple_character_check)

# test suites with simple tests (for this file only)
suite_test <- consonance_suite() + single_test
suite_assert <- consonance_suite() + single_assert
suite_check <- consonance_suite() + single_check

# more logging
suite_verbose <- consonance_suite(logging.level="INFO") + single_assert

# test that only generate warnings
suite_warning <- consonance_suite() +
  consonance_test("chars", is.character, .level="warning")


# simple data objects (for this file only)
abc <- letters[1:3]


##############################################################################
# handling of inputs

test_that("consonance checks suite class", {
  expect_error(validate(1:10, 10), "not a consonance suite")
})


##############################################################################
# Core functionality

test_that("test_consonance with assert / check / test syntax", {
  expect_silent(validate(abc, suite_assert))
  expect_silent(validate(abc, suite_check))
  expect_silent(validate(abc, suite_test))
  expect_error(capture_output(validate(1:3, suite_verbose)))
  expect_error(capture_output(validate(1:3, suite_check)))
  expect_error(capture_output(validate(1:3, suite_test)))
})

test_that("test_consonance with a single test", {
  expect_silent(validate(abc, single_test))
  expect_silent(validate(abc, single_assert))
  expect_silent(validate(abc, single_check))
  expect_error(capture_output(validate(1:3, single_test)))
  expect_error(capture_output(validate(1:3, single_assert)))
  expect_error(capture_output(validate(1:3, single_check)))
})

test_that("test_consonance with verbose output", {
  expect_silent(validate(abc, suite_assert))
  # message should include INFO field and name of test
  expect_output(validate(abc, suite_verbose), "INFO")
  expect_output(validate(abc, suite_verbose), "chars")
})

test_that("test_consonance with verbose output", {
  suite <-
    consonance_test("all even", function(x) {all(x%%2==0)}) +
    consonance_test("above 0", function(x) {all(x > 0)})
  expect_silent(validate(c(2, 4, 6), suite))
  expect_error(capture_output(validate(c(2, 4, 5), suite)))
  expect_error(capture_output(validate(c(2, -4, 6), suite)))
})

test_that("test_consonance of data frames", {
  test_square <- function(x) {
    xdim <- dim(x)
    xdim[1] == xdim[2]
  }
  suite <- consonance_test("square", test_square)
  m_square <- matrix(1:9, ncol=3)
  m_long <- matrix(1:10, ncol=2)
  expect_silent(validate(m_square, suite))
  expect_error(capture_output(validate(m_long, suite)))
})

test_that("test_consonance of data frame columns", {
  suite_a <- consonance_assert("above 0", simple_above, .var="a")
  suite_b <- consonance_assert("above 0", simple_above, .var="b")
  df <- data.frame(a=5:10, b=-seq(5:10))
  expect_silent(validate(df, suite_a))
  expect_error(capture_output(validate(df, suite_b)))
})

test_that("signals level=warning and level=errors differently", {
  suite_e <-
    consonance_assert("in [0, 100]", simple_percentage, .level="error")
  suite_w <-
    consonance_assert("in [0, 100]", simple_percentage, .level="warning")
  # suite e should have errors
  expect_error(capture_output(validate(-4, suite_e)))
  expect_error(capture_output(validate(-4, suite_e)))
  # suite w should have warnings
  expect_output(validate(-4, suite_w), "WARN")
  expect_output(validate(-4, suite_w), "warning")
  # but suite w should not generate errors
  expect_failure(
    expect_output(validate(-4, suite_w), "ERROR")
  )
})

test_that("signals R warning() as WARN", {
  suite_spearman <-
    consonance_assert("spearman >0", simple_spearman, col1="x", col2="y")
  good_df <- data.frame(x=seq(1, 20)+rnorm(10), y=seq(1, 20)+rnorm(20))
  bad_df <- data.frame(x=rep(1:5, each=2),
                       y=rep(1:5, each=2))
  # columns in good_df are well correlated
  expect_silent(validate(good_df, suite_spearman))
  # column in bad_df have ties, which should trigger an R warning
  expect_output(validate(bad_df, suite_spearman), "WARN")
})


##############################################################################
# logging into a file

test_that("suite constructor logs into a file", {
  file_path <- tempfile()
  suite_file <- consonance_suite(log.file=file_path) +
    consonance_assert("numeric", assert_numeric)
  expect_false(file.exists(file_path))
  expect_error(validate("abc", suite_file))
  expect_true(file.exists(file_path))
  file_content <- readLines(file_path)
  expect_true(grepl("ERROR", file_content)[1])
  unlink(file_path)
  expect_false(file.exists(file_path))
})

test_that("validate can log into a file on request", {
  file_path <- tempfile()
  expect_false(file.exists(file_path))
  # one run of test_consonance should generate 4 lines in log
  # INFO ... validate() (not recorded)
  # consonance error: (name of test)
  # consonance data: ...
  # consonance suite: ...
  # consonance result: ...
  expect_error(validate(1:3, suite_assert, log.file=file_path))
  expect_true(file.exists(file_path))
  expect_equal(length(readLines(file_path)), 4)
  # a second run without log.file should not generate additional lines
  expect_error(capture_output(validate(1:3, suite_assert)))
  expect_equal(length(readLines(file_path)), 4)
  # a third run with log.file should add 4 more lines
  expect_error(validate(1:3, suite_assert, log.file=file_path))
  expect_equal(length(readLines(file_path)), 8)
  unlink(file_path)
  expect_false(file.exists(file_path))
})


##############################################################################
# run-time tuning

test_that("test_consonance can skip all tests", {
  # tests stop only on non-character inputs
  expect_silent(validate(abc, suite_assert))
  expect_error(capture_output(validate(1:3, suite_assert)))
  # but tests succeed when skip is TRUE
  expect_silent(validate(1:3, suite_assert, skip=TRUE))
  # when skip is TRUE, the output should still be equal to the input
  result <- validate(1:3, suite_assert, skip=TRUE)
  expect_equal(result, 1:3)
})

test_that("skipping tests logs the skip", {
  # normal execution should report test results
  expect_output(validate(abc, suite_verbose), "pass")
  # skip should record skip action
  expect_output(validate(abc, suite_verbose,
                                    skip=TRUE, skip.action = "log"),
                "skip")
  # skipping should be be able to be entirely quiet
  expect_silent(validate(abc, suite_verbose,
                                    skip=TRUE, skip.action="none"))
})

test_that("validate can use different logging level", {
  # suite_assert should be silent on normal inputs
  expect_silent(validate(abc, suite_assert))
  # but can produce output by setting logging level manually
  expect_output(validate(abc, suite_assert, logging.level="INFO"))
})

test_that("validate can avoid logging altogether at run-time", {
  s <- suite_assert
  # no output on good data
  expect_silent(validate(abc, s, logging.level="NONE"))
  # error and output on bad data
  expect_output(expect_error(validate(1:3, s)))
  # error only but not output
  expect_silent(expect_error(validate(1:3, s, logging.level="NONE")))
})

test_that("validate can toggle strictness at runtime", {
  # suite_warning should only generate warnings and keep going (no error)
  expect_output(validate(1:3, suite_warning), "WARN")
  # but can halt if runtime level is more strict
  expect_error(capture_output(
    validate(1:3, suite_assert, level="warning")))
})


##############################################################################
# logging of validations using partial objects, e.g. abc[, 1:2]

test_that("validate reports partial object names", {
  s <- consonance_suite() +
    consonance_test("x numeric", is.numeric, .var="x") +
    consonance_test("x numeric", is.numeric, .var="y")
  df <- data.frame(x=1:4, y=0, z=1:2)
  expect_silent(capture_output(validate(df, s)))
  expect_silent(capture_output(validate(df[, 1:2], s)))
  expect_error(capture_output(validate(df[, c(1,3)], s)))
  # now evaluate to capture the output
  out <- capture_output(validate(df[, c(1,3)], s, level="stop"))
  # the output should report the data object is based on df
  expect_true(grepl("df", out))
  # there should not be strange nested lists
  # (this is a test based on a historical bug)
  expect_false(grepl("\\[conson", out))
})

