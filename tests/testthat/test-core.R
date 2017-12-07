context("Core")

function_list <- list(avg_salary = "mean(salary)", max_salary = "max(salary)")
new_functions <- list()

grouping_obj <- get_groups(
  df = main_df,
  groups = c("company", "party", "color"),
  functions = function_list
)

test_that("get_groups returns proper list", {
  expect_equal(names(grouping_obj), c("n_1_group", "n_2_group", "n_3_group"))
  expect_equal(names(grouping_obj$n_1_group), c("company", "party", "color"))
  expect_equal(names(grouping_obj$n_2_group), c("company...party", "company...color", "party...color"))
  expect_equal(names(grouping_obj$n_3_group), c("company...party...color"))
  expect_equal(
    length(get_groups(df = main_df, groups = c("company", "party", "color"), functions = list(avg_salary = "mean(salary)"), depth = 2)),
    2
  )
  expect_equal(names(grouping_obj$n_1_group$company), c("company", "avg_salary", "max_salary"))
  expect_equal(names(grouping_obj$n_2_group$company...party), c("company", "party", "avg_salary", "max_salary"))
})

test_that("group object has right class of dataframes", {
  expect_true(dplyr::is.grouped_df(grouping_obj$n_1_group$company))
  expect_true(dplyr::is.grouped_df(grouping_obj$n_2_group$company...party))
  expect_true(dplyr::is.grouped_df(grouping_obj$n_3_group$company...party...color))
})

test_that("group object does calculation correctly", {
  expect_equal(grouping_obj$n_1_group$company$avg_salary, c(77.5, 95.0))
  expect_equal(grouping_obj$n_1_group$company$max_salary, c(100, 120))
})
