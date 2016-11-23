# groupR

## Introduction

The code in this pseudo-package is used to conduct comprehensive analysis on somewhat abstract datasets. For example, suppose you have this:

| Person | Company                  | Political Party | Favorite Color | Salary |
|--------|--------------------------|-----------------|----------------|--------|
| Joe    | University of Fakesville | Whig            | Orange         | 100    |
| Bob    | Coal Mine Incorporated   | Whig            | Blue           | 70     |
| Janet  | University of Fakesville | Libertarian     | Orange         | 80     |
| Heather| Coal Mine Incorporated   | Whig            | Green          | 80     |
| John   | Coal Mine Incorporated   | Libertarian     | Green          | 60     |
| Winston| Coal Mine Incorporated   | Whig            | Blue           | 100    |
| Melissa| University of Fakesville | Libertarian     | Red            | 120    |
| Monica | University of Fakesville | Libertarian     | Red            | 80     |

Your boss asks, "Hey Alex, use our database to calculate the average salary by company". So you go and run the numbers. But then, unexpectedly, you are asked again to calculate the average salary, this time by favorite color. You return to your dataset, boot up a pivot table, and return the required result. While you very nearly produce the correct answer, the real demand was for average salary blown out by both company *and* favorite color. Going back to the drawing board, you realize there are any number of ways to calculate the various breakouts. What if (*gasp*) you are asked to break it out by Company vs Color vs *Political Party*?? That means you would have to calculate the average salary by each aggregation, like so:

1. Overall (Remember, we need to do average salary for the whole dataset as well)
2. Company
3. Political Party
4. Favorite Color
5. Company / Political Party
6. Company / Favorite Color
7. Politcal Party / Favorite Color
8. Company / Political Party / Favorite Color

This is a comprehensive list of potential data requests regarding average salary per group, assuming Person does not serve as a group. The code provided here can compute these, as well as any other similar function against the target data (mean, median, sd, etc.).

## Usage

First, let's create our dataset:

```R
name <- c("Joe","Bob","Janet","Heather","John","Winston","Melissa","Monica")
company <- c("University of Fakesville" ,"Coal Mine Incorporated" ,"University of Fakesville" ,"Coal Mine Incorporated" ,"Coal Mine Incorporated" ,"Coal Mine Incorporated" ,"University of Fakesville" ,"University of Fakesville")
party <- c("Whig" ,"Whig" ,"Libertarian" ,"Whig" ,"Libertarian" ,"Whig" ,"Libertarian" ,"Libertarian")
color <- c("Orange", "Blue", "Orange", "Green", "Green", "Blue", "Red", "Red")
salary <- c(100,70,80,80,60,100,120,80)

main_df <- data.frame(name, company, party, color, salary)
```

Now that the data has been loaded, the full list of averages can be easily produced:

```R
> grouping_obj <- get_groups(main_df, c("company", "party", "color"), functions = list("avg_salary" = "mean(salary)"))
> sapply(grouping_obj, names)

# $n_1_group
# [1] "company" "party"   "color"  
# 
# $n_2_group
# [1] "company/party" "company/color" "party/color"  
# 
# $n_3_group
# [1] "company/party/color"
# 
# $no_groups
# [1] "avg_salary"
```
With this, we can select any group we are interested in:

```R
> grouping_obj$n_1_group$company

#                    company avg_salary
# 1   Coal Mine Incorporated       77.5
# 2 University of Fakesville       95.0
```

This is the basic idea. Multiple functions can be applied to the data at the same time, so, for example, if we also wanted to count the employees in each group and list the maximum salary all in one dataframe for each combination, that would be possible like so:

```R
> function_list <- list(avg_salary = "mean(salary)", employee_count = "n()", max_salary = "max(salary)")
> grouping_obj <- get_groups(main_df, c("company", "party", "color"), functions = function_list)

> grouping_obj$n_1_group$company

#                    company avg_salary employee_count max_salary
# 1   Coal Mine Incorporated       77.5              4        100
# 2 University of Fakesville       95.0              4        120

> grouping_obj$n_2_group$`company/party`

#                    company       party avg_salary employee_count max_salary
#                    <fctr>      <fctr>      <dbl>          <int>      <dbl>
# 1   Coal Mine Incorporated Libertarian   60.00000              1         60
# 2   Coal Mine Incorporated        Whig   83.33333              3        100
# 3 University of Fakesville Libertarian   93.33333              3        120
# 4 University of Fakesville        Whig  100.00000              1        100
```
