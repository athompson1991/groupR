########
# Core #
########

name <- c("Joe","Bob","Janet","Heather","John","Winston","Melissa","Monica")
company <- c("University of Fakesville" ,"Coal Mine Incorporated" ,"University of Fakesville" ,"Coal Mine Incorporated" ,"Coal Mine Incorporated" ,"Coal Mine Incorporated" ,"University of Fakesville" ,"University of Fakesville")
party <- c("Whig" ,"Whig" ,"Libertarian" ,"Whig" ,"Libertarian" ,"Whig" ,"Libertarian" ,"Libertarian")
color <- c("Orange", "Blue", "Orange", "Green", "Green", "Blue", "Red", "Red")
salary <- c(100,70,80,80,60,100,120,80)

main_df <- data.frame(name, company, party, color, salary)


###############
# Time series #
###############

set.seed(12345)

dates <- seq(as.Date("2017-01-01"), length.out = 100, by = "day")
companies <- c("company_a", "company_b", "company_c")
states <- c("WA", "CA")
department <- c("sales", "marketing")
budget <- abs(rnorm(1200))

time_df <- cbind(expand.grid(companies, states, department, dates), budget)
names(time_df) <- c("companies", "states", "department", "dates", "budget")



devtools::use_data(main_df, grouping_obj, time_df, internal = T, overwrite = T)
rm(list = ls())

