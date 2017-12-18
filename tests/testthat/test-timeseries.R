context("Time series")
set.seed(12345)

dates <- seq(as.Date("2017-01-01"), length.out = 100, by = "day")
companies <- c("company_a", "company_b", "company_c")
states <- c("WA", "CA")
department <- c("sales", "marketing")
obs <- abs(rnorm(1200))

test_df <- cbind(expand.grid(companies, states, department, dates), obs)
names(test_df) <- c("companies", "states", "department", "dates", "obs")

groups <- c("companies", "states", "department")
test_groupr <- get_groups(test_df, groups = c(groups, "dates"), functions = list(avg = "mean(obs)"))
extracted_groupr <- extract_xts(grouping_obj = test_groupr, value_choice = "avg", date_col = "dates", groups = groups)

congress_groupr <- get_groups(df = rollcalls, groups = c("first_of_month", "congress", "chamber", "vote_result"))
congress_xts <- extract_xts(congress_groupr, groups = c("vote_result"), value_choice = "count", date_column = "first_of_month")

test_that("extract_xts gives right classes", {
  expect_identical(class(extracted_groupr), "list")
  expect_identical(class(extracted_groupr$n_1_group), "list")
  expect_identical(class(extracted_groupr$n_1_group$overall), c("xts", "zoo"))
  expect_identical(class(extracted_groupr$n_2_group$companies), c("xts", "zoo"))
  expect_identical(class(extracted_groupr$n_2_group$states), c("xts", "zoo"))
  expect_identical(class(extracted_groupr$n_2_group$department), c("xts", "zoo"))
})

test_that("extract_xts gives right group level names", {
  expect_identical(names(extracted_groupr), c("n_1_group", "n_2_group", "n_3_group"))
  expect_identical(names(extracted_groupr$n_1_group), "overall")
  expect_identical(names(extracted_groupr$n_2_group), c("companies", "states", "department"))
  expect_identical(names(extracted_groupr$n_3_group), c("companies...states", "companies...department", "states...department"))
})

test_that("extract_xts gives right xts names", {
  expect_identical(colnames(extracted_groupr$n_1_group$overall), "avg")
  expect_identical(colnames(extracted_groupr$n_2_group$companies),companies)
  expect_identical(colnames(extracted_groupr$n_2_group$states), states)
  expect_identical(colnames(extracted_groupr$n_2_group$department), department)
  expect_identical(colnames(extracted_groupr$n_3_group$companies...states), c("company_a/WA", "company_b/WA", "company_c/WA", "company_a/CA", "company_b/CA", "company_c/CA"))
})
