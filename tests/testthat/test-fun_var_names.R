library(rsamplestudy)
context("fun_var_names")

test_that("fun_var_names works", {
  expect_equal(fun_var_names(5), paste0('x[', 1:5, ']'))
})
