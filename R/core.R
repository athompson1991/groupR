#' Create groupr object.
#'
#' Constructor function for \code{groupr} object. This function takes a data
#' frame as input and returns a grouping object as output. The \code{groupr} is
#' essentially a list of lists of data frames, one data frame for every possible
#' combination of groups. For example, if there are two groups, A and B, four
#' data frames are returned: data frames for A, B, both A and B, and a data
#' frame summarizing the whole dataset.
#'
#' @export
#' @importFrom utils tail
#' @param df A data frame of raw data.
#' @param groups A character vector of column names in df that will be basis for
#'   aggregation.
#' @param functions A named list of functions for aggregation.
#' @param depth How far down the tree the combinations should work out.
#' @return A grouping object (list that aggregates functions by every potential
#'   combination)
#' @examples
#' default_data <- data.frame(
#'   my_group1 = rep(c("A", "B"), each = 4),
#'   my_group2 = rep(c("foo", "bar", "baz", "potato"), each=2),
#'   my_data = runif(n = 8)
#' )
#' groupr_object <- groupr(
#'   df = default_data,
#'   groups = "my_group1",
#'   functions = list(rando_sum = "sum(my_data)")
#' )
#' print(groupr_object)
groupr <- function(df, groups,
                   functions = list("count" = "n()"),
                   depth = length(groups)) {

  meta <- list(groups = groups, functions = functions)
  group_combinations <- get_combinations(depth, groups)
  function_count <- length(functions)
  column_names <- names(functions)
  get_str_fn <- function(f) functions[[f]]
  string_functions <- as.vector(sapply(column_names, get_str_fn))

  group_level_list <- lapply(group_combinations, function(mtx) {
    df_list <- apply(mtx, 2, function(column) {
      group_selection <- as.vector(column)
      loop_ls <- dplyr_loop(df, string_functions, group_selection)
      df <- df_work(loop_ls, group_selection)
      logic <- tail(1:ncol(df), function_count)
      colnames(df)[logic] <- column_names
      return(df)
    })
    name_fn <- function(column) paste(column, collapse = "...")
    names(df_list) <- apply(mtx, 2, name_fn)
    return(df_list)
  })

  overall_list <- overall_calc(functions, df)
  group_level_list <- c(overall_list, group_level_list)
  names(group_level_list) <-
    paste("n_", 0:depth, "_group", sep = "")
  group_level_list$meta <- meta
  out <- structure(group_level_list, class = "groupr")
  return(out)
}

dplyr_loop <- function(in_df, functions, selection){
  lapply(functions, function(f){
    out_df <- in_df %>%
      dplyr::group_by_(.dots = selection) %>%
      dplyr::summarize_(lazyeval::interp(f))
    out_df
  })
}

df_work <- function(loop_ls, group_selection){
  index <- 1:length(group_selection)
  groups_columns <- as.data.frame(loop_ls[[1]][, index])
  fn <- function(df) df[,-index]
  loop_ls <- lapply(loop_ls, fn)
  worked_on_df <- do.call(cbind, loop_ls)
  worked_on_df <- cbind(groups_columns, worked_on_df)
  worked_on_df <-
    dplyr::grouped_df(worked_on_df, group_selection)
}

overall_calc <- function(functions, df){
  fn <- function(s) dplyr::summarise_(df, lazyeval::interp(s))
  overall <- as.data.frame(sapply(functions, fn))
  colnames(overall) <- names(functions)
  overall <- dplyr::as.tbl(overall)
  overall_list <- list(overall = overall)
  return(overall_list)
}

#' Get the groups or functions from a \code{groupr} object
#'
#' Simple wrapper to pull group names or function definitions.
#'
#' @param groupr The \code{groupr} object in question
#' @export
get_groups <- function(groupr)
  return(names(groupr$n_1_group))

#' @rdname get_groups
get_functions <- function(groupr)
  return(groupr$meta$functions)

#' Perform function(s) on grouping object.
#'
#' Given a metric calculated using a \code{groupr} object, one could need to
#' perform additional analysis using the data produced. For example, a
#' \code{groupr} object could be used to count the number of employees in an
#' organization and get their total tenure, as broken out by various groups
#' (state, full/part time, etc.). A natural, additional calculation could be an
#' total tenure divided by count. This function is generalized so that any
#' function with one argument (\code{df}) can be passed.
#'
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom methods formalArgs
#' @param groupr A grouping object created with the \code{} function
#' @param new_functions A list of functions (each with one argument: \code{df})
#'   to apply to the grouping object. The \code{df} argument will be the
#'   dataframe in the grouping object.
#' @param is_cbind Boolean value for whether or not the functions applied should
#'   be added to the \code{df} passed, or if they should be returned as a list
#'   (with similar dimensions as the \code{groupr})
#' @examples
#' permits_groupr <- groupr(
#'   permits,
#'   groups = c("type_desc", "issued_date", "existing_const_type")
#' )
#' extract_df(permits_groupr, "existing_const_type")
#' applied_groupr <- gapply(permits_groupr,
#'   list(rounded = function(df) round(df$count, -2)),
#'   is_cbind = TRUE
#' )
#' extract_df(applied_groupr, "existing_const_type")
gapply.groupr <- function(groupr, new_functions, is_cbind = F) {
  check_functions(new_functions)
  core <- drop_grouping_level(groupr, length(groupr))

  top_applied <- lapply(core, function(df_list) {
    group_level_applied <- lapply(df_list, function(df) {
      has_no_columns <- is.null(ncol(df))
      if (!has_no_columns) {
        calcs <- lapply(new_functions, function(calc) {
          temp_data <- do.call(calc, list(df))
          return(temp_data)
        })
        if (is_cbind) {
          new_data <- do.call(cbind, calcs)
          df <- cbind(as.data.frame(df), as.data.frame(new_data))
          return_this <- df
        } else
          return_this <- calcs
        return(return_this)
      }
    })
    return(group_level_applied)
  })

  top_applied$meta <- groupr$meta
  top_applied$meta$new_functions <- new_functions
  top_applied <- structure(top_applied, class = "groupr")
  return(top_applied)
}

check_functions <- function(fn_list){
  if (is.null(names(fn_list)))
    stop("Functions must be named")

  for (i in fn_list)
    if (!identical(formalArgs(i), "df"))
      stop("Functions must take only one argument, named df")
}

