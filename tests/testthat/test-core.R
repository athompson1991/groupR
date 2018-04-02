context("Core")

function_list <- list(avg_salary = "mean(salary)", max_salary = "max(salary)")

test_groupr <- groupr(
  df = main_df,
  groups = c("company", "party", "color"),
  functions = function_list
)

two_levels <- groupr(df = main_df, groups = c("company", "party", "color"), functions = function_list, depth = 2)

simple_function_1 <- function(df) df$avg_salary / df$max_salary
simple_function_2 <- function(df) df$avg_salary + 1
new_functions <- list(percent_calc = simple_function_1, stupid_calc = simple_function_2)
applied_obj <- gapply(test_groupr, new_functions = new_functions, is_cbind = T)

test_that("groupr returns proper list", {
  expect_equal(names(test_groupr), c("n_0_group", "n_1_group", "n_2_group", "n_3_group", "meta"))
  expect_equal(names(test_groupr$n_1_group), c("company", "party", "color"))
  expect_equal(names(test_groupr$n_2_group), c("company...party", "company...color", "party...color"))
  expect_equal(names(test_groupr$n_3_group), c("company...party...color"))
  expect_equal(length(two_levels), 4)
  expect_equal(names(test_groupr$n_1_group$company), c("company", "avg_salary", "max_salary"))
  expect_equal(names(test_groupr$n_2_group$company...party), c("company", "party", "avg_salary", "max_salary"))
})

test_that("group object has right classes", {
  expect_true(is.groupr(test_groupr))
  expect_true(is.list(test_groupr$n_1_group))
  expect_true(is.list(test_groupr$n_2_group))
  expect_true(is.list(test_groupr$n_3_group))
  expect_true(dplyr::is.grouped_df(test_groupr$n_1_group$company))
  expect_true(dplyr::is.grouped_df(test_groupr$n_2_group$company...party))
  expect_true(dplyr::is.grouped_df(test_groupr$n_3_group$company...party...color))
})

test_that("group object does calculation correctly", {
  expect_equal(test_groupr$n_1_group$company$avg_salary, c(77.5, 95.0))
  expect_equal(test_groupr$n_1_group$company$max_salary, c(100, 120))
})

test_that("group apply produces expected names", {
  expect_equal(names(applied_obj$n_1_group$company), c("company", "avg_salary", "max_salary", "percent_calc", "stupid_calc"))
  expect_equal(names(applied_obj$n_2_group$company...party), c("company", "party", "avg_salary", "max_salary", "percent_calc", "stupid_calc"))
})

test_that("group apply errors on bad function list", {
  expect_error(gapply(test_groupr, new_functions = list(function(df) df$avg_salary * 2)), "Functions must be named")
  expect_error(gapply(test_groupr, new_functions = list(double = function(foo) df$avg_salary * 2)), "Functions must take only one argument, named df")
  expect_error(gapply(test_groupr, new_functions = list(double = function(df) df$avg_salary * 2, sqed = function(foo) df$avg_salary ** 2)),
               "Functions must take only one argument, named df")
})

test_that("group apply produces expected classes", {
  expect_true(is.groupr(gapply(test_groupr, new_functions = new_functions, is_cbind = T)))
})

test_that("get_groups.groupr works", {
  expect_equal(get_groups(test_groupr), c("company", "party", "color"))
  expect_equal(get_groups(applied_obj), c("company", "party", "color"))
  expect_equal(get_groups(two_levels),  c("company", "party", "color"))
})

test_that("get_functions.groupr works", {
  expect_identical(get_functions(test_groupr), function_list)
  expect_identical(get_functions(applied_obj), function_list)
  expect_identical(get_functions(two_levels),  function_list)
})
