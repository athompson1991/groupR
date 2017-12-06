context("Core")

test_that("get_groups returns proper list", {
  expect_equal(names(grouping_obj), c("n_1_group", "n_2_group", "n_3_group"))
  expect_equal(names(grouping_obj$n_1_group), c("company", "party", "color"))
  expect_equal(names(grouping_obj$n_2_group), c("company...party", "company...color", "party...color"))
  expect_equal(names(grouping_obj$n_3_group), c("company...party...color"))
})

test_that("group object has right class of dataframes", {
  expect_true(dplyr::is.grouped_df(grouping_obj$n_1_group$company))
  expect_true(dplyr::is.grouped_df(grouping_obj$n_2_group$company...party))
  expect_true(dplyr::is.grouped_df(grouping_obj$n_3_group$company...party...color))
})
