#' Create grouping object.
#'
#' This function takes a data frame as input and returns a grouping object as output. The grouping object is a list of lists of data frames, one data frame for every possible combination of groups.
#' For example, if there are two groups, A and B, four data frames are returned: data frames for A, B, both A and B, and a data frame summarizing the whole dataset.
#'
#'
#' @export
#' @param df A data frame of raw data.
#' @param groups A character vector of column names in df that will be basis for aggregation.
#' @param functions A named list of functions for aggregation.
#' @return A grouping object (list that aggregates functions by every potential combination)
#' @examples
#' default_data <- data.frame(  my_group1 = c(rep("A", 4), rep("B", 4))
#'                            , my_group2 = c(rep("foo", 2), rep("bar", 2), rep("baz", 2), rep("potato", 2))
#'                            , my_data = runif(n = 8)
#'                           )
#' grouping_obj <- get_groups(df = default_data, groups = "my_group1", functions = list(rando_sum = "sum(my_data)"))
#' print(grouping_obj)
get_groups <- function(df, groups, functions = list("count" = "n()"), depth = length(groups)){
  group_combinations <- get_combinations(depth, groups)
  function_count <- length(functions)
  column_names <- names(functions)
  functions <- as.vector(sapply(column_names, function(f) functions[[f]]))

  out_list <- lapply(group_combinations, function(mtx){

    df <- apply(mtx, 2, function(column){
      group_selection <- as.vector(column)
      dplyr_list <- dplyr_loop(in_df = df, functions = functions, selection = group_selection)
      groups_columns <- as.data.frame(dplyr_list[[1]][ ,1:length(group_selection)])
      dplyr_list <- lapply(dplyr_list, function(df) df[ ,-c(1:length(group_selection))])
      out_df <- do.call(cbind, dplyr_list)
      out_df <- cbind(groups_columns, out_df)
      out_df <- dplyr::grouped_df(out_df, group_selection)
      colnames(out_df)[tail(1:ncol(out_df), function_count)] <- column_names
      return(out_df)
    })
    names(df) <- apply(mtx, 2, function(column) paste(column, collapse = "..."))
    df
  })

  overall <- as.data.frame(sapply(functions, function(s) dplyr::summarise_(df, lazyeval::interp(s))))
  colnames(overall) <- column_names
  overall <- dplyr::as.tbl(overall)
  overall_list <- list(overall = overall)

  out_list <- c(overall_list, out_list)
  names(out_list) <- paste("n_", 0:depth, "_group", sep = "")
  out <- structure(out_list, class="groupr")
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

#' Perform function(s) on grouping object.
#'
#' @export
#' @importFrom magrittr "%>%"
#' @param groupr A grouping object created with the \code{get_groups} function
#' @param new_functions A list of functions (each with one argument: \code{df}) to apply to the grouping object. The \code{df} argument will be the dataframe in the grouping object.
#' @param is_cbind Boolean value for whether or not the functions applied should be added to the \code{df} passed, or if they should be returned as a list (with similar dimensions as the \code{group_obj})
#' @examples
#' groupr <- get_groups(permits_clean, groups = c("type_desc", "issued_date", "existing_const_type"))
#' extract_df(groupr, "existing_const_type")
#' applied_groupr <- group_obj_apply(groupr, list(rounded = function(df) round(df$count, -3)), is_cbind=T)
#' extract_df(applied_groupr, "existing_const_type")
group_obj_apply <- function(groupr, new_functions, is_cbind = F){
  raw_names <- names(new_functions)

  if(is.null(names(new_functions)))
    stop("Functions must be named")

  out <- lapply(groupr, function(df_list){
    lapply(df_list, function(df){
      if(!is.null(ncol(df))){
        new_data_ls <- lapply(new_functions, function(f){
          temp_data <- do.call(f, list(df))
          if(xts::is.xts(temp_data) | is.data.frame(temp_data)){
            colnames(temp_data) <- paste(raw_names[i], colnames(temp_data), sep = "_")
          }
          return(temp_data)
        })

        return_this <- NULL
        if(is_cbind){
          if(xts::is.xts(df)){
            new_data <- do.call(merge, new_data_ls)
            y <- merge(y, new_data)
          }else{
            new_data <- do.call(cbind, new_data_ls)
            df <- cbind(as.data.frame(df), as.data.frame(new_data))
          }
          return_this <- df
        }else{
          return_this <- new_data_ls
        }
        return(return_this)
      }
    })
  })
  out <- structure(out, class = "groupr")
  return(out)
}
