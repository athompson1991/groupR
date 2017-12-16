context("Core")

function_list <- list(avg_salary = "mean(salary)", max_salary = "max(salary)")

test_groupr <- get_groups(
  df = main_df,
  groups = c("company", "party", "color"),
  functions = function_list
)

two_levels <- get_groups(df = main_df, groups = c("company", "party", "color"), functions = list(avg_salary = "mean(salary)"), depth = 2)

simple_function <- function(df) df$avg_salary / df$max_salary
new_functions <- list(percent_calc = simple_function)
applied_obj <- group_obj_apply(test_groupr, new_functions = new_functions, is_cbind = T)

test_that("get_groups returns proper list", {
  expect_equal(names(test_groupr), c("n_0_group", "n_1_group", "n_2_group", "n_3_group"))
  expect_equal(names(test_groupr$n_1_group), c("company", "party", "color"))
  expect_equal(names(test_groupr$n_2_group), c("company...party", "company...color", "party...color"))
  expect_equal(names(test_groupr$n_3_group), c("company...party...color"))
  expect_equal(length(two_levels), 3)
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
  expect_equal(names(applied_obj$n_1_group$company), c("company", "avg_salary", "max_salary", "percent_calc"))
  expect_equal(names(applied_obj$n_2_group$company...party), c("company", "party", "avg_salary", "max_salary", "percent_calc"))
})

test_that("group apply produces expected classes", {
  expect_true(is.groupr(applied_obj))
})
