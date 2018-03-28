context("Utils")

function_list <- list(avg_salary = "mean(salary)", max_salary = "max(salary)")

test_groupr <- groupr(
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

test_that("subset works", {
  expect_identical(subset(test_groupr, "color")[[2]], test_groupr[[2]][-c(1, 2)])
  expect_identical(subset(test_groupr, "color")[[3]], test_groupr[[3]][-c(1)])
  expect_identical(subset(test_groupr, "color")[[4]], test_groupr[[4]])

  # intersect

  expect_identical(subset(test_groupr, c("color", "company"))[[2]], test_groupr[[2]][-c(1,2,3)])
  expect_identical(subset(test_groupr, c("color", "company"))[[3]], test_groupr[[3]][-c(1,3)])
  expect_identical(subset(test_groupr, c("color", "company"))[[4]], test_groupr[[4]])

  # union

  expect_identical(subset(test_groupr, c("color", "company"), type="union")[[2]], test_groupr[[2]][-c(2)])
  expect_identical(subset(test_groupr, c("color", "company"), type="union")[[3]], test_groupr[[3]])
  expect_identical(subset(test_groupr, c("color", "company"), type="union")[[4]], test_groupr[[4]])

  # except intersect

  expect_identical(subset(test_groupr, c("color", "company"), type="except_intersect")[[2]], test_groupr[[2]][c(1,2,3)])
  expect_identical(subset(test_groupr, c("color", "company"), type="except_intersect")[[3]], test_groupr[[3]][c(1,3)])
  expect_identical(subset(test_groupr, c("color", "company"), type="except_intersect")[[4]], test_groupr[[4]][c(-1)])

  # except union

  expect_identical(subset(test_groupr, c("color", "company"), type="except_union")[[2]], test_groupr[[2]][c(2)])
  expect_identical(subset(test_groupr, c("color", "company"), type="except_union")[[3]], test_groupr[[3]][-c(1,2,3)])
  expect_identical(subset(test_groupr, c("color", "company"), type="except_union")[[4]], test_groupr[[4]][-c(1,2,3)])

})

test_that("auto_other_label works", {
  renamed_df <- other_label(permits, "existing_use", percentile = 0.9)
  expect_equal(sort(unique(renamed_df$existing_use)), c("1 family dwelling", "2 family dwelling", "apartments", "office", "other", "retail sales"))
  renamed_df <- other_label(permits, "status", custom=c("approved", "expired", "incomplete", "expired", "reinstated", "revoked", "suspend", "withdrawn"))
  expect_equal(sort(unique(renamed_df$status)), c("cancelled", "complete", "issued", "other"))
})
