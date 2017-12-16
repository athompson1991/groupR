context("Object oriented")

function_list <- list(avg_salary = "mean(salary)", max_salary = "max(salary)")

test_groupr <- get_groups(
  df = main_df,
  groups = c("company", "party", "color"),
  functions = function_list
)

test_that("print works correctly", {
  string <- capture.output(print(test_groupr))
  expect_equal(paste0(string, collapse="\n"), "n_0_group\nn_1_group\n  |_company\n  |_party\n  |_color\nn_2_group\n  |_company...party\n  |_company...color\n  |_party...color\nn_3_group\n  |_company...party...color")
  string <- capture.output(print(test_groupr, include_colnames = T))
  expect_equal(paste0(string, collapse="\n"), paste0(
                                              "n_0_group\n",
                                              "n_1_group\n  |_company\n    |_company\n    |_avg_salary\n    |_max_salary\n  |_party\n    |_party\n    |_avg_salary\n    |_max_salary\n  |_color\n    |_color\n    |_avg_salary\n    |_max_salary\n",
                                              "n_2_group\n  |_company...party\n    |_company\n    |_party\n    |_avg_salary\n    |_max_salary\n  |_company...color\n    |_company\n    |_color\n    |_avg_salary\n    |_max_salary\n  |_party...color\n    |_party\n    |_color\n    |_avg_salary\n    |_max_salary\n",
                                              "n_3_group\n  |_company...party...color\n    |_company\n    |_party\n    |_color\n    |_avg_salary\n    |_max_salary", collapse="")
               )
})
