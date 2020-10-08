# tests for 'consonance', i.e. running a consonance test suite on data

# single assertion / check / test
single_test <- consonance_test("chars", is.character)

# some common data objects
a1 <- letters[1:3]
a2 <- letters[4:6]



##############################################################################
# batch validation

test_that("validate batch with passing objects", {
  s1 <- consonance_suite() + consonance_test("chars", is.character)
  group <- data.frame(object=c("a1", "a2"), suite="s1",
                      stringsAsFactors=FALSE)
  expect_silent(validate(group))
  result <- validate(group)
  expect_identical(result, group)
})

test_that("validate batch with errors", {
  s1 <- consonance_suite() + consonance_test("chars", is.character)
  n1 <- 1:3
  group <- data.frame(object=c("a1", "n1"), suite="s1",
                      stringsAsFactors=FALSE)
  expect_error(capture_output(validate(group)))
})

test_that("validate_batch records names of data and suite objects", {
  file_path <- tempfile()
  expect_false(file.exists(file_path))
  mysuite <- consonance_suite() + consonance_test("chars", is.character)
  abc <- c("a", "b", "c")
  xyz <- c("x", "y", "z")
  group <- data.frame(object=c("abc", "xyz"), suite="mysuite",
                    stringsAsFactors=FALSE)
  # one run of test_consonance should generate 4 lines in log per row
  expect_silent(validate(group, logging.level="INFO", log.file=file_path))
  expect_true(file.exists(file_path))
  log_output <- readLines(file_path)
  expect_equal(length(log_output), 10)
  expect_true(any(grepl("mysuite", log_output)))
  expect_true(any(grepl("abc", log_output)))
  expect_true(any(grepl("xyz", log_output)))
  unlink(file_path)
  expect_false(file.exists(file_path))
})

test_that("validate_batch handles mistakes in batch definition", {
  s1 <- consonance_suite() + consonance_test("chars", is.character)
  group <- data.frame(object=c("a1", NA), suite="s1",
                      stringsAsFactors=FALSE)
  expect_error(capture_output(validate(group)))
})
