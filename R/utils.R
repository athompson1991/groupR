extract_grouping_level <- function(groupr, group_level){
  work_groupr <- unclass(groupr)
  return(work_groupr[[group_level]])
}

drop_grouping_level <- function(groupr, group_level){
  work_groupr <- unclass(groupr)
  work_groupr <- work_groupr[-group_level]
  work_groupr <- as.groupr(work_groupr)
  work_groupr <- extract_drop_util(groupr=groupr, groups=groups, return_type = "drop")
  return(work_groupr)
}

#' Extract or drop an observation
#'
#' Pull a leaf from the \code{groupr} tree. This will typically return a dataframe or an \code{xts} object.
#'
#' @export
#' @param groupr The groupr object to pull from
#' @param groups The desired groups
#' @return The desired observation, typically a dataframe or \code{xts} object
extract_df <- function(groupr, groups){
  out <- extract_drop_util(groupr = groupr, groups, return_type="extract")
  return(out)
}

#' @describeIn extract_df Drop an observation
#'
drop_df <- function(groupr, groups){
  out <- extract_drop_util(groupr=groupr, groups=groups, return_type = "drop")
  return(out)
}

########################################################################################


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

calculate_df_index <- function(main_list, group_level, groups){
  group_level_list <- main_list[[group_level]]
  if(length(group_level_list) > 1){
    logic_matrix <- sapply(groups, function(s) grepl(s, names(group_level_list)))
    df_index <- which(apply(logic_matrix, 1, all))
  } else {
    df_index <- 1
  }
  return(df_index)
}

drop_overall_df <- function(main_groupr){
  work_groupr <- unclass(main_groupr)
  if(names(work_groupr)[1] == "n_0_group")
    work_groupr <- work_groupr[-1]
  return(work_groupr)
}

clean_raw_groups <- function(work_groupr, groups){
  extracted_groups <- names(work_groupr[[1]])
  valid_groups <- groups[groups %in% extracted_groups]
  crap_groups <- groups[!(groups %in% extracted_groups)]
  print_msg <- paste0(crap_groups, collapse = ",")
  if(length(crap_groups) > 0)
    warning(paste("Not in data:", print_msg))
  return(valid_groups)
}

reassign_overall_df <- function(original, worked_on){
  if(names(original)[1] == "n_0_group"){
    overall_df <- original$n_0_group
    worked_on <- c(list(overall_df), worked_on)
    names(worked_on)[1] <- "n_0_group"
    return(worked_on)
  }
}

extract_drop_util <- function(groupr, groups, return_type){
  return_this <- NULL
  work_groupr <- drop_overall_df(groupr)
  valid_groups <- clean_raw_groups(work_groupr, groups)
  group_level <- length(valid_groups)
  group_level_list <- work_groupr[[group_level]]
  df_index <- calculate_df_index(main_list = work_groupr, group_level = group_level, groups = valid_groups)
  if(return_type == "extract"){
    return_this <- group_level_list[df_index]
    return_this <- return_this[[1]]
  }
  if(return_type == "drop"){
    work_groupr[[group_level]] <- work_groupr[[group_level]][-df_index]
    work_groupr <- reassign_overall_df(groupr, work_groupr)
    work_groupr <- as.groupr(work_groupr)
    return_this <- work_groupr
  }
  return(return_this)
}

#'
#'
#'
#'
subset.groupr <- function(groupr, groups, type = "intersect"){
  worked_on <- lapply(groupr[-1], function(level){
    if(type == "intersect")
      return_dfs <- sapply(level, function(df) all(groups %in% colnames(df)))
    else if(type == "union")
      return_dfs <- sapply(level, function(df) any(groups %in% colnames(df)))
    else if(type == "except_intersect")
      return_dfs <- sapply(level, function(df) !all(groups %in% colnames(df)))
    else if(type == "except_union")
      return_dfs <- sapply(level, function(df) !any(groups %in% colnames(df)))
    else
      stop("Bad type, must be intersect, union, except_intersect, or except_union")
    fixed_level <- level[return_dfs]
    return(fixed_level)
  })
  worked_on <- reassign_overall_df(groupr, worked_on)
  worked_on <- as.groupr(worked_on)
  return(worked_on)
}
