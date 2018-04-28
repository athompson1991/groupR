context("Time series")

##########
# Set up #
##########

bad_names <- c("company a", "company b", "company c")

time_groupr <- groupr(
  time_df,
  groups = c("companies", "states", "department", "dates"),
  functions = list(avg_budget = "mean(budget)")
)

extracted_groupr <- extract_xts(
  groupr = time_groupr,
  value_choice = "avg_budget",
  date_col = "dates"
)

intervals = rep(c("day", "week", "month"), 2)
cycles = rep(c("annual", "week"), each = 3)
start_values <- rep(c(2016.000, 1), each = 3)
end_values <- c(2016.273, 2017.89733, 2024.25, 15.143, NA, NA)
frequencies <- c(365.25, 52.17857, 12.00, 7.00, NA, NA)

test_df <- data.frame(
  stringsAsFactors = F,
  intervals,
  cycles,
  start_values,
  end_values,
  frequencies
)

set.seed(1000)
end_dates <- as.Date(c("2017-01-06", "2017-02-05", "2017-06-01"))
intervals <- c("day", "week", "month")
start <- as.Date("2017-01-01")

#########
# Tests #
#########

test_that("get_groups returns correct names", {
  expect_identical(get_groups(extracted_groupr), c("companies", "states", "department"))
})

test_that("fill_xts works as expected", {
  set.seed(1000)
  start <- as.Date("2017-01-01")

  intervals <- c("day", "week", "month", "quarter", "year")
  end_dates <- as.Date(c("2017-01-06", "2017-02-06", "2017-06-01", "2018-06-01", "2022-01-01"))

  for(i in 1:length(intervals)){
    end_date <- end_dates[i]
    interval = intervals[i]
    date_seq <- seq.Date(from = start, to = end_date, by = interval)
    messy_xts <- xts::xts(rnorm(5), order.by = date_seq[-3])
    filled <- fill_xts(messy_xts, interval = interval)

    expect_equal(class(filled), c("xts", "zoo"))
    expect_equal(filled[[3]], 0)
    expect_equal(summary(as.factor(filled == 0))["TRUE"], 1, check.names = F)

    filled <- fill_xts(messy_xts, interval = interval, fill_val = NA)
    expect_output(filled[[3]], NA)

    multi_mtx <- matrix(rnorm(40), ncol = 8)
    multi_xts <- xts::xts(multi_mtx, order.by = date_seq[-3])
    filled_multi <- fill_xts(multi_xts, interval = interval)
    expect_equal(ncol(filled_multi), 8)
    expect_equal(nrow(filled_multi), 6)
    expect_true(all(filled_multi[3, ] == 0))
  }
})

test_that("extract_xts gives right group level names", {
  group_names <- c("n_0_group", "n_1_group", "n_2_group", "n_3_group", "meta")
  n1 <- c("companies", "states", "department")
  n2 <- c("companies...states", "companies...department", "states...department")
  n3 <- c("companies...states...department")

  expect_identical(names(extracted_groupr), group_names)
  expect_identical(names(extracted_groupr$n_0_group), "overall")
  expect_identical(names(extracted_groupr$n_1_group), n1)
  expect_identical(names(extracted_groupr$n_2_group), n2)
  expect_identical(names(extracted_groupr$n_3_group), n3)
})

test_that("extract_xts gives right classes", {
  expect_identical(class(extracted_groupr), c("xts_groupr", "groupr"))
  expect_identical(class(extracted_groupr$n_0_group), c("xts", "zoo"))
  expect_identical(class(extracted_groupr$n_1_group$companies), c("xts", "zoo"))
  expect_identical(class(extracted_groupr$n_1_group$states), c("xts", "zoo"))
  expect_identical(class(extracted_groupr$n_1_group$department), c("xts", "zoo"))
})


test_that("extract_xts gives right xts names", {
  companies <- c("company_a", "company_b", "company_c")
  states <- c("wa", "ca")
  depts <- c("sales", "marketing")
  combs <- c("company_a/wa","company_a/ca","company_b/wa", "company_b/ca", "company_c/wa","company_c/ca")

  expect_identical(colnames(extracted_groupr$n_0_group), "overall")
  expect_identical(colnames(extracted_groupr$n_1_group$companies), companies)
  expect_identical(colnames(extracted_groupr$n_1_group$states), states)
  expect_identical(colnames(extracted_groupr$n_1_group$department), depts)
  expect_identical(colnames(extracted_groupr$n_2_group$companies...states), combs)
})

test_that("make_ts produces as expected", {

  # Annual Tests

  for(i in 1:3){
    row <- test_df[i, ]
    start <- as.Date("2016-01-01")
    dates <- seq(start, length.out = 100, by = row[["intervals"]])
    test_xts <- xts::xts(rnorm(100), order.by = dates)
    test_ts <- make_ts(test_xts, interval = row[["intervals"]])
    test_tsp <- tsp(test_ts)
    expect_tsp <- c(row[["start_values"]], row[["end_values"]], row[["frequencies"]])
    expect_equal(test_tsp, expect_tsp, tolerance = 0.0001)
  }

  # Custom
  dates <- seq(as.Date("2016-01-01"), length.out = 100, by = 4)
  test_xts <- xts::xts(rnorm(100), order.by = dates)
  test_ts <- make_ts(test_xts, interval = 4)
  expect_equal(tsp(test_ts), c(2016.000, 2040.75, 4.00), tolerance = 0.0001)

  # Weekly Tests

  # Daily
  dates <- seq(as.Date("2016-01-01"), length.out = 100, by = "day")
  test_xts <- xts::xts(rnorm(100), order.by = dates)
  test_ts <- make_ts(test_xts, interval = "day", cycle = "week")
  expect_equal(tsp(test_ts), c(1.000, 15.143, 7.00), tolerance = 0.0001)

  # Error
  expect_error(make_ts(test_xts, "potato"), "Bad interval choice")
  expect_error(make_ts(test_xts, interval = "potato", cycle = "week"), "Bad interval choice")
  expect_error(make_ts(test_xts, interval = "potato", cycle = "year"), "Bad interval choice")
  expect_error(make_ts(test_xts, interval = "week", cycle = "potato"), "Bad cycle choice")
})
