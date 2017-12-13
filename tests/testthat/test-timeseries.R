context("Time series")
set.seed(12345)

companies <- rep(c("company_a", "company_b", "company_c"), 100)
states <- rep(c("WA", "CA"), length.out=300, each=3)
dates <- seq(as.Date("2017-01-01"), length.out = 50, by = "day")
obs <- rnorm(300)

test_df <- data.frame(companies, states, dates = rep(dates, each=6), obs)
test_groupr <- get_groups(test_df, groups = c("companies", "states", "dates"), functions = list(avg = "mean(obs)"))

test_that("Extract xts functions properly", {
  extract_xts(grouping_obj = test_groupr, value_choice = "avg", date_col = "dates", groups = list("companies", "states"))
})
