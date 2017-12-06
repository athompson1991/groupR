name <- c("Joe","Bob","Janet","Heather","John","Winston","Melissa","Monica")
company <- c("University of Fakesville" ,"Coal Mine Incorporated" ,"University of Fakesville" ,"Coal Mine Incorporated" ,"Coal Mine Incorporated" ,"Coal Mine Incorporated" ,"University of Fakesville" ,"University of Fakesville")
party <- c("Whig" ,"Whig" ,"Libertarian" ,"Whig" ,"Libertarian" ,"Whig" ,"Libertarian" ,"Libertarian")
color <- c("Orange", "Blue", "Orange", "Green", "Green", "Blue", "Red", "Red")
salary <- c(100,70,80,80,60,100,120,80)

main_df <- data.frame(name, company, party, color, salary)

grouping_obj <- get_groups(
  df = main_df,
  groups = c("company", "party", "color"),
  functions = list(avg_salary = "mean(salary)")
)

devtools::use_data(main_df, grouping_obj, internal = T, overwrite = T)

rm(list = ls())
