context("Time series")
set.seed(12345)

companies <- rep(c("company_a", "company_b", "company_c"), 100)
states <- rep(c("WA", "CA"), length.out=300, each=3)
dates <- seq(as.Date("2017-01-01"), length.out = 50, by = "day")
obs <- rnorm(300)

test_df <- data.frame(companies, states, dates = rep(dates, each=6), obs)
test_groupr <- get_groups(test_df, groups = c("companies", "states", "dates"), functions = list(avg = "mean(obs)"))

test_that("extract_xts gives right classes", {
  extracted_groupr <- extract_xts(grouping_obj = test_groupr, value_choice = "avg", date_col = "dates", groups = list("companies", "states"))
  expect_identical(class(extracted_groupr), "list")
  expect_identical(class(extracted_groupr$n_1_group), "list")
  expect_identical(class(extracted_groupr$n_1_group$dates), c("xts", "zoo"))
  expect_identical(class(extracted_groupr$n_2_group$companies...dates), c("xts", "zoo"))
  expect_identical(class(extracted_groupr$n_2_group$states...dates), c("xts", "zoo"))
})

test_that("extract_xts gives right names", {
  extracted_groupr <- extract_xts(grouping_obj = test_groupr, value_choice = "avg", date_col = "dates", groups = list("companies", "states"))
  expect_identical(names(extracted_groupr), c("n_1_group", "n_2_group"))
  expect_identical(names(extracted_groupr$n_1_group), "dates")
  expect_identical(names(extracted_groupr$n_2_group), c("companies...dates", "states...dates"))
})
