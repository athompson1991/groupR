
<!-- README.md is generated from README.Rmd. Please edit that file -->
groupR
======

[![Build Status](https://travis-ci.org/athompson1991/groupR.svg?branch=master)](https://travis-ci.org/athompson1991/groupR) [![codecov.io](https://codecov.io/github/athompson1991/groupR/coverage.svg?branch=master)](https://codecov.io/github/athompson1991/groupR?branch=master)

Installation
------------

You can install groupR from github with:

``` r
# install.packages("devtools")
devtools::install_github("athompson1991/groupR")
```

Example
-------

Aggregate everything! In every way! For example, suppose you have this:

| Person  | Company                  | Political Party | Favorite Color | Salary |
|---------|--------------------------|-----------------|----------------|--------|
| Joe     | University of Fakesville | Whig            | Orange         | 100    |
| Bob     | Coal Mine Incorporated   | Whig            | Blue           | 70     |
| Janet   | University of Fakesville | Libertarian     | Orange         | 80     |
| Heather | Coal Mine Incorporated   | Whig            | Green          | 80     |
| John    | Coal Mine Incorporated   | Libertarian     | Green          | 60     |
| Winston | Coal Mine Incorporated   | Whig            | Blue           | 100    |
| Melissa | University of Fakesville | Libertarian     | Red            | 120    |
| Monica  | University of Fakesville | Libertarian     | Red            | 80     |

Your boss asks, "Hey Alex, use our database to calculate the average salary by company". So you go and run the numbers. But then, unexpectedly, you are asked again to calculate the average salary, this time by favorite color. You return to your dataset, boot up a pivot table, and return the required result. While you very nearly produce the correct answer, the real demand was for average salary blown out by both company *and* favorite color. Going back to the drawing board, you realize there are any number of ways to calculate the various breakouts. What if (*gasp*) you are asked to break it out by Company vs Color vs *Political Party*?? That means you would have to calculate the average salary by each aggregation, like so:

1.  Company
2.  Political Party
3.  Favorite Color
4.  Company / Political Party
5.  Company / Favorite Color
6.  Politcal Party / Favorite Color
7.  Company / Political Party / Favorite Color
8.  Overall (Remember, there's an overall average salary as well)

This is a comprehensive list of potential data requests regarding average salary per group, assuming Person does not serve as a group. The code provided here can compute these, as well as any other similar function against the target data (mean, median, sd, etc.).

Usage
-----

First, let's create our dataset:

``` r
name <- c("Joe","Bob","Janet","Heather","John","Winston","Melissa","Monica")
company <- c("University of Fakesville" ,"Coal Mine Incorporated" ,"University of Fakesville" ,"Coal Mine Incorporated" ,"Coal Mine Incorporated" ,"Coal Mine Incorporated" ,"University of Fakesville" ,"University of Fakesville")
party <- c("Whig" ,"Whig" ,"Libertarian" ,"Whig" ,"Libertarian" ,"Whig" ,"Libertarian" ,"Libertarian")
color <- c("Orange", "Blue", "Orange", "Green", "Green", "Blue", "Red", "Red")
salary <- c(100,70,80,80,60,100,120,80)

main_df <- data.frame(name, company, party, color, salary)
```

Now that the data has been loaded, the full list of averages can be easily produced:

``` r
my_groupr <- groupr(main_df, groups = c("company", "party", "color"), functions = list(avg_salary = "mean(salary)"))
print(my_groupr, include_colnames = T)
#> n_0_group
#> n_1_group
#>   |_company
#>     |_company
#>     |_avg_salary
#>   |_party
#>     |_party
#>     |_avg_salary
#>   |_color
#>     |_color
#>     |_avg_salary
#> n_2_group
#>   |_company...party
#>     |_company
#>     |_party
#>     |_avg_salary
#>   |_company...color
#>     |_company
#>     |_color
#>     |_avg_salary
#>   |_party...color
#>     |_party
#>     |_color
#>     |_avg_salary
#> n_3_group
#>   |_company...party...color
#>     |_company
#>     |_party
#>     |_color
#>     |_avg_salary
#> meta
#>   |_groups
#>   |_functions
```
