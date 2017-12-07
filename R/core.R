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
get_groups <- function(df, groups, functions = list("count" = "n()"), depth = length(groups)){
  group_combn <- get_combinations(depth, groups)
  n_fn <- length(functions)
  col_names <- names(functions)
  functions <- as.vector(sapply(col_names, function(f) functions[[f]]))

  out_list <- lapply(group_combn, function(mtx){

    temp_df <- apply(mtx, 2, function(col){
      temp_groups <- as.vector(col)
      temp_ls <- lapply(functions, function(f){
        temp_fn <- df %>%
          dplyr::group_by_(.dots = temp_groups) %>%
          dplyr::summarize_(lazyeval::interp(f))
        temp_fn
      })
      main_cols <- as.data.frame(temp_ls[[1]][ ,1:length(temp_groups)])
      temp_ls <- lapply(temp_ls, function(df) df[ ,-c(1:length(temp_groups))])
      temp_out <- do.call(cbind, temp_ls)
      temp_out <- cbind(main_cols, temp_out)
      temp_out <- dplyr::grouped_df(temp_out, temp_groups)
      return(temp_out)
    })
    comb_names <- apply(mtx, 2, function(col) paste(col, collapse = "..."))
    names(temp_df) <- comb_names
    temp_df
  })

  ls_nm <- paste("n_", 1:depth, "_group", sep = "")
  names(out_list) <- ls_nm
  out_list <- lapply(out_list, function(ls){
    lapply(ls, function(df){
      df <- df[ ,!duplicated(colnames(df))]
      colnames(df)[tail(1:ncol(df), n_fn)] <- col_names
      df
    })
  })
}

#' Perform function(s) on grouping object.
#'
#' @export
#' @importFrom magrittr "%>%"
#' @param group_obj A grouping object created with the \code{get_groups} function
#' @param new_functions A list of functions (each with one argument: \code{df}) to apply to the grouping object. The \code{df} argument will be the dataframe in the grouping object.
#' @param is_cbind Boolean value for whether or not the functions applied should be added to the \code{df} passed, or if they should be returned as a list (with similar dimensions as the \code{group_obj})
#'
group_obj_apply <- function(group_obj, new_functions, is_cbind = F){
  raw_names <- names(new_functions)
  lapply(group_obj, function(df_list){
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
}

