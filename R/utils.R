#' Make every combination of character vector
#' @param n Maximum rows per combination data frame
#' @param char_vec Character vector to be used
#' @return A list of data frames with every column a unique combination of the given character vector
get_combinations <- function(n, char_vec){
  full_list <- list()
  for(i in 1:n){
    temp_combn <- combn(char_vec, i)
    full_list <- c(full_list, list(temp_combn))
  }
  return(full_list)
}

# From StackOverflow: http://stackoverflow.com/questions/26539441
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}

new_xts_names <- function(obs, groups){
  all_unique <- sapply(groups, function(group){
    unique(as.character(dplyr::pull(obs, group)))
  })
  data_combinations <- expand.grid(all_unique)
  new_col_names <- apply(data_combinations, 1, paste0, collapse="/")
  return(new_col_names)
}

extract_grouping_level <- function(groupr, group_level){
  work_groupr <- unclass(groupr)
  return(work_groupr[[group_level]])
}

drop_grouping_level <- function(groupr, group_level){
  work_groupr <- unclass(groupr)
  work_groupr <- work_groupr[-group_level]
  work_groupr <- as.groupr(work_groupr)
  return(work_groupr)
}

extract_df <- function(groupr, group_level, df_name){
  work_groupr <- unclass(groupr)
  return(work_groupr[[group_level]][[df_name]])
}

drop_df <- function(groupr, group_level, df_name){
  work_groupr <- unclass(groupr)
  df_index <- which(names(work_groupr[[group_level]]) == df_name)
  work_groupr[[group_level]] <- work_groupr[[group_level]][-df_index]
  work_groupr <- as.groupr(work_groupr)
  return(work_groupr)
}

