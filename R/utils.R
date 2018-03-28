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
#' @return Will extract he desired observation, a single dataframe or \code{xts} object and return it,
#' or drop that single observation and return the groupr object without it.
#' @examples
#' groupr <- get_groups(permits, groups = c("issued_month", "status", "location", "existing_use"))
#' extract_df(groupr, "status")
#' extract_df(groupr, c("existing_use", "status"))
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


#' Automatically assign "other" to low observation count grouping values
#'
#' Often times, a dataset will have groups which are distributed
#' somewhat exponentially - almost all rows fall into a few group values, but there are many smaller group values for
#' the remaining observations. For example, you may have a dataset with employee level observations and want to use
#' "US State" as a group, but 90\% of the observations fall into New York, California, Texas, and perhaps 6 other states.
#' All remaining observations are distributed amongst the remaining 41 states, but you might prefer to lump all of those
#' observations into a single bucket. This functions provides a way to reassign all those observations to "other".
#'
#' @export
#' @param df The dataframe to be manipulated
#' @param column Which column to relabel
#' @param percentile Which percentage to cut off the data at
#' @param custom A custom vector of values to reassign to "other" in the dataset
#' @return The dataframe with reassigned column
#' @examples
#' summary(as.factor(permits$type_desc))
#' permits_cleaned <- other_label(permits, "type_desc")
#' summary(as.factor(permits_cleaned$type_desc))
other_label <- function(df, column, percentile=0.9, custom=NULL){
  work_vector <- df[ ,column]
  if(is.character(work_vector))
    work_vector <- as.factor(work_vector)

  if(is.null(custom)){
    counts <- summary(work_vector)
    ordered_counts <- counts[order(counts, decreasing=T)]
    pct_contrib <- ordered_counts/sum(ordered_counts)
    pct_cumsum <- cumsum(pct_contrib)
    rename_these <- names(which(pct_cumsum > percentile))
  } else {
    rename_these = custom
  }
  relabelled_vector <- plyr::mapvalues(df[ ,column], from=rename_these, to=rep("other", length(rename_these)))
  df[ ,column] <- relabelled_vector
  return(df)
}
