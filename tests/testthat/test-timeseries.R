context("Time series")

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

test_that("get_groups returns correct names", {
  expect_identical(get_groups(extracted_groupr),
                   c("companies", "states", "department"))
})


test_that("extract_xts gives right group level names", {
  expect_identical(
    names(extracted_groupr),
    c("n_0_group", "n_1_group", "n_2_group", "n_3_group", "meta")
  )
  expect_identical(names(extracted_groupr$n_0_group), "overall")
  expect_identical(names(extracted_groupr$n_1_group),
                   c("companies", "states", "department"))
  expect_identical(
    names(extracted_groupr$n_2_group),
    c(
      "companies...states",
      "companies...department",
      "states...department"
    )
  )
  expect_identical(names(extracted_groupr$n_3_group),
                   c("companies...states...department"))
})

test_that("extract_xts gives right classes", {
  expect_identical(class(extracted_groupr), c("xts_groupr", "groupr"))
  expect_identical(class(extracted_groupr$n_0_group), c("xts", "zoo"))
  expect_identical(class(extracted_groupr$n_1_group$companies), c("xts", "zoo"))
  expect_identical(class(extracted_groupr$n_1_group$states), c("xts", "zoo"))
  expect_identical(class(extracted_groupr$n_1_group$department), c("xts", "zoo"))
})


test_that("extract_xts gives right xts names", {
    expect_identical(colnames(extracted_groupr$n_0_group), "overall")
    expect_identical(
      colnames(extracted_groupr$n_1_group$companies),
      c("company_a", "company_b", "company_c")
    )
    expect_identical(colnames(extracted_groupr$n_1_group$states), c("wa", "ca"))
    expect_identical(colnames(extracted_groupr$n_1_group$department),
                     c("sales", "marketing"))
    expect_identical(
      colnames(extracted_groupr$n_2_group$companies...states),
      c(
        "company_a/wa",
        "company_a/ca",
        "company_b/wa",
        "company_b/ca",
        "company_c/wa",
        "company_c/ca"
      )
    )
  })

test_that("fill_xts works for day, week, month", {
  # Set up
  set.seed(1000)
  end_dates <- as.Date(c("2017-01-06", "2017-02-05", "2017-06-01"))
  intervals <- c("day", "week", "month")
  start <- as.Date("2017-01-01")

  for(i in 1:3){
    int = intervals[i]
    end <- end_dates[i]
    dates <- seq.Date(from = start, to = end, by = int)
    messy <- xts::xts(rnorm(5), order.by = dates[-3])

    # Test
    expect_equal(
      zoo::index(fill_xts(messy, int)),
      dates,
      check.attributes = F
    )
    expect_identical(as.vector(fill_xts(messy, int))[3], 0)
  }
})
