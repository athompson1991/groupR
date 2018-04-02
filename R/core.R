#' Create groupr object.
#'
#' This function takes a data frame as input and returns a grouping object as
#' output. The grouping object is a list of lists of data frames, one data frame
#' for every possible combination of groups. For example, if there are two
#' groups, A and B, four data frames are returned: data frames for A, B, both A
#' and B, and a data frame summarizing the whole dataset.
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
#'     my_group1 = rep(c("A", "B"), each = 4),
#'     my_group2 = rep(c("foo", "bar", "baz", "potato"), each=2),
#'     my_data = runif(n = 8)
#' )
#' groupr_object <- groupr(df = default_data,
#'                         groups = "my_group1",
#'                         functions = list(rando_sum = "sum(my_data)"))
#' print(groupr_object)
groupr <-
  function(df,
           groups,
           functions = list("count" = "n()"),
           depth = length(groups)) {
    group_combinations <- get_combinations(depth, groups)
    function_count <- length(functions)
    column_names <- names(functions)
    functions_raw <- functions
    df_raw <- df
    functions <-
      as.vector(sapply(column_names, function(f)
        functions[[f]]))

    group_level_list <- lapply(group_combinations, function(mtx) {
      df <- apply(mtx, 2, function(column) {
        group_selection <- as.vector(column)
        dplyr_list <-
          dplyr_loop(in_df = df,
                     functions = functions,
                     selection = group_selection)
        groups_columns <-
          as.data.frame(dplyr_list[[1]][, 1:length(group_selection)])
        dplyr_list <-
          lapply(dplyr_list, function(df)
            df[, -c(1:length(group_selection))])
        worked_on_df <- do.call(cbind, dplyr_list)
        worked_on_df <- cbind(groups_columns, worked_on_df)
        worked_on_df <-
          dplyr::grouped_df(worked_on_df, group_selection)
        colnames(worked_on_df)[tail(1:ncol(worked_on_df), function_count)] <-
          column_names
        return(worked_on_df)
      })
      names(df) <-
        apply(mtx, 2, function(column)
          paste(column, collapse = "..."))
      df
    })

    overall <-
      as.data.frame(sapply(functions, function(s)
        dplyr::summarise_(df, lazyeval::interp(s))))
    colnames(overall) <- column_names
    overall <- dplyr::as.tbl(overall)
    overall_list <- list(overall = overall)

    group_level_list <- c(overall_list, group_level_list)
    names(group_level_list) <-
      paste("n_", 0:depth, "_group", sep = "")

    group_level_list$meta <-
      list(groups = groups, functions = functions_raw)
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


#' Get the groups or functions from a \code{groupr} object
#'
#' Simple wrapper to pull group names or function definitions.
#'
#' @param groupr The \code{groupr} object in question
#' @export
get_groups <- function(groupr){
  return(names(groupr$n_1_group))
}

#' @rdname get_groups
get_functions <- function(groupr){
  return(groupr$meta$functions)
}

#' Perform function(s) on grouping object.
#'
#' Given a metric calculated using a \code{groupr} object, one could need to
#' perform additional analysis using the data produced. For example, a
#' \code{groupr} object could be used to count the number of employees in an
#' organization and get their total tenure, as broken out by various groups
#' (state, full/part time, etc.). A natural, additional calculation could be an
#' total tenure divided by count, applied to all the resultant data frames. This
#' function is generalized so that any function with one argument (\code{df})
#' can be passed.
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
#'          permits,
#'          groups = c("type_desc", "issued_date", "existing_const_type")
#'          )
#' extract_df(permits_groupr, "existing_const_type")
#' applied_groupr <- gapply(permits_groupr,
#'                          list(rounded = function(df) round(df$count, -2)),
#'                          is_cbind = TRUE)
#' extract_df(applied_groupr, "existing_const_type")
gapply.groupr <- function(groupr, new_functions, is_cbind = F) {
  raw_names <- names(new_functions)

  if (is.null(names(new_functions)))
    stop("Functions must be named")

  for (i in new_functions)
    if (!identical(formalArgs(i), "df"))
      stop("Functions must take only one argument, named df")

  core_groupr <-
    drop_grouping_level(groupr = groupr, group_level = length(groupr))

  top_applied <- lapply(core_groupr, function(df_list) {
    group_level_applied <- lapply(df_list, function(df) {
      has_no_columns <- is.null(ncol(df))
      if (!has_no_columns) {
        df_level <- lapply(new_functions, function(f) {
          temp_data <- do.call(f, list(df))
          return(temp_data)
        })
        if (is_cbind) {
          new_data <- do.call(cbind, df_level)
          df <- cbind(as.data.frame(df), as.data.frame(new_data))
          return_this <- df
        } else
          return_this <- df_level
        return(return_this)
      }
    })
    return(group_level_applied)
  })

  top_applied$meta <- groupr$meta
  top_applied <- structure(top_applied, class = "groupr")
  return(top_applied)
}

