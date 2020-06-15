# test display of consonance objects in the terminal

source("simple_assertions.R")


# print for suite objects

test_that("print of empty suite gives a count summary", {
  cons <- consonance_suite()
  expect_output(print(cons), "suite" )
  expect_output(print(cons), "0 tests")
})

test_that("print of non-empty suite gives a count summary", {
  cons <- consonance_suite() + consonance_test("a", simple_is_character)
  expect_output(print(cons), "1 test" )
})


# print for individual tests

test_that("print of an individual test shows description and arguments", {
  result <- consonance_test("abc", simple_is_character)
  expect_output(print(result), "abc")
})

test_that("print of an individual test shows all arguments", {
  result <- consonance_test("abc", simple_range, lower=0, upper=100)
  expect_output(print(result), "lower, upper")
})

