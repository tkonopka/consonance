# test adding new test conditions

source("simple_assertions.R")


# add_test arguments

test_that("add_test checks arguments", {
  abc = c("a", "b", "c")
  t1 <- consonance_test("a", simple_is_character)
  expect_error(add_test(abc, t1), "abc")
  expect_error(add_test(10, t1), "10")
})


# shorthand with plus operator

test_that("composition of suite + test gives a larger suite", {
  result_1 <- consonance_suite() + consonance_test("a", simple_is_character)
  expect_equal(length(result_1$tests), 1)
  # addition (suite + test)
  result_2 <- result_1 + consonance_test("b", simple_is_character)
  expect_equal(length(result_2$tests), 2)
})


test_that("composition of test + suite gives a larger suite", {
  result_1 <- consonance_suite() + consonance_test("a", simple_is_character)
  expect_equal(length(result_1$tests), 1)
  # addition (test + suite)
  result_2 <- consonance_test("b", simple_is_character) + result_1
  expect_equal(length(result_2$tests), 2)
})

test_that("composition of two tests via plus operator gives a suite", {
  result <- consonance_test("a", simple_is_character) +
    consonance_test("b", simple_is_character)
  expect_equal(length(result$tests), 2)
})

