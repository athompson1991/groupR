context("Utils")

function_list <- list(avg_salary = "mean(salary)", max_salary = "max(salary)")

test_groupr <- get_groups(
  df = main_df,
  groups = c("company", "party", "color"),
  functions = function_list
)

test_that("get_combinations returns as expected", {
  x <- c("foo", "bar", "baz", "potato")
  y <- get_combinations(3, x)
  expect_equal(length(y), 3)
  expect_equal(dim(y[[1]]), c(1, 4))
  expect_equal(dim(y[[2]]), c(2, 6))
  expect_equal(dim(y[[3]]), c(3, 4))
})

test_that("drop overall df works", {
  expect_equal(length(drop_overall_df(test_groupr)), 3)
  expect_true(is.list(drop_overall_df(test_groupr)))
  expect_identical(names(drop_overall_df(test_groupr)), c("n_1_group", "n_2_group", "n_3_group"))
})

test_that("extract dataframe returns correctly", {
  expect_identical(test_groupr$n_1_group$company, extract_df(groupr = test_groupr, groups = "company"))
  expect_identical(test_groupr$n_2_group$company...party, extract_df(groupr = test_groupr, groups = c("company", "party")))
  expect_identical(test_groupr$n_2_group$company...party, extract_df(groupr = test_groupr, groups = c("party", "company")))
  expect_warning(warn_extract <- extract_df(groupr = test_groupr, groups = c("company", "potato")), "Not in data: potato")
  expect_identical(test_groupr$n_1_group$company, warn_extract)
  expect_identical(test_groupr$n_3_group$company...party...color, extract_df(groupr = test_groupr, groups = c("company", "party", "color")))
})

test_that("drop dataframe returns correctly", {
  expect_true(is.groupr(drop_df(groupr = test_groupr, groups = "company")))
  expect_identical(test_groupr$n_1_group[-1], drop_df(groupr = test_groupr, groups = "company")$n_1_group)
  expect_identical(test_groupr$n_1_group[-2], drop_df(groupr = test_groupr, groups = "party")$n_1_group)
  expect_identical(names(drop_df(groupr = test_groupr, groups = "company")), c("n_0_group", "n_1_group", "n_2_group", "n_3_group"))
  expect_equal(length(drop_df(groupr = test_groupr, groups = c("company", "party", "color"))$n_3_group), 0)
  expect_identical(test_groupr$n_2_group[-1], drop_df(groupr = test_groupr, groups = c("company", "party"))$n_2_group)
  expect_warning(warn_drop <- drop_df(groupr = test_groupr, groups = c("company", "potato")), "Not in data: potato")
  expect_identical(test_groupr$n_1_group[-1], warn_drop$n_1_group)
})
