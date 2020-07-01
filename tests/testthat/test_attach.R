# test attaching a consonance suite to other objects (e.g. linear models)

source("simple_assertions.R")


# a simple data frame and regression model
xy <- data.frame(x=1:10, y=c(1:9, 8))
xy.lm <- lm(y~x, data=xy)


# attaching - argument tests

test_that("attach_consonance argument checks", {
  suite <- consonance_suite()
  expect_error(attach_consonance(NULL, suite), "NULL")
  expect_error(attach_consonance(list(), NULL), "not a suite")
  expect_error(attach_consonance(list(), list()), "not a suite")
})


# attaching a consonance suite to a list-like object

test_that("attach_consonance modifies an object", {
  model <- list(a=10)
  result <- attach_consonance(model, consonance_suite())
  expect_true("consonance" %in% names(result))
  expect_true("a" %in% names(result))
  expect_is(result$consonance, "consonance_suite")
})

test_that("add_test can add tests to a composite object", {
  model <- list(a=10)
  result <- attach_consonance(model, consonance_suite())
  result <- add_test(result, "a", simple_pass)
  expect_true("consonance" %in% names(result))
  expect_is(result$consonance, "consonance_suite")
  expect_equal(length(result$consonance$tests), 1)
})

test_that("attach_consonance twice generates a warning", {
  model <- list(a=10)
  suite_1 <- consonance_suite()
  suite_2 <- consonance_suite() +
    consonance_assert("numeric", simple_pass)
  result <- attach_consonance(model, suite_1)
  expect_warning(attach_consonance(result, suite_2), "replaced")
})



# attaching a consonance suite to a regression model

test_that("test_consonance using a model with consonance", {
  suite <- consonance_assert("numeric", simple_pass, .var="x") +
    consonance_assert("positive", simple_range, lower=0, .var="x")
  model <- attach_consonance(xy.lm, suite)
  # make sure the suite was attached
  expect_true("consonance" %in% names(model))
  # data conforms to the suite, so they should conform to the model
  expect_silent(test_consonance(xy, suite))
  expect_silent(test_consonance(xy, model))
})

test_that("test_consonance using a model with consonance suite", {
  suite <- consonance_assert("numeric", simple_pass, .var="x") +
    consonance_assert("positive", simple_range, lower=0, .var="x")
  model <- attach_consonance(xy.lm, suite)
  # make sure the suite was attached
  expect_true("consonance" %in% names(model))
  xy.bad <- data.frame(x=c(1,2,-4), y=c(2,3,4))
  # the suite should raise an error, and so should the model
  expect_error(capture_output(test_consonance(xy.bad, suite)))
  expect_error(capture_output(test_consonance(xy.bad, model)))
})



# attaching sets function environment

test_that("attaching sets function environment", {
  mylist <- list(a=10, b=20)
  # function uses "a" and "b" - these should be taken from the list environment
  # at run-time
  suite <- consonance_suite() +
    consonance_assert("[a, b]", function(x, .model) {
      simple_range(x, lower=.model$a, upper=.model$b)
    })
  model <- attach_consonance(mylist, suite)
  expect_silent(test_consonance(c(12, 14), model))
  expect_error(capture_output(test_consonance(c(5, 15), model)))
})

