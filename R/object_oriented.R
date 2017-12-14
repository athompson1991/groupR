is.groupr <- function(x) inherits(x, "groupr")
as.groupr <- function(in_list) structure(in_list, class="groupr")

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

subset.groupr <- function(groupr, groups, group_level = 1){

  work_groupr <- unclass(groupr)
  new_list <- list()

  new_list <- lapply(work_groupr[group_level], function(group_level){
    full_df_list <- names(group_level)
    logic_matrix <- as.matrix(sapply(groups, function(string) grepl(string, full_df_list)))

    # Last grouping level needs to be treated differently
    if(ncol(logic_matrix) == 1)
      logic_vector <- TRUE
    else
      logic_vector <- as.vector(apply(logic_matrix, 1, any))

    returned_dfs <- group_level[logic_vector]
    return(returned_dfs)
  })

  out <- new_list
  if(identical(group_level, 1)){
    if(length(groups) == 1)
      out <- out[[1]][[groups]]
    else{
      out <- out[[1]][groups]
    }
  }else{
    out <- as.groupr(out)
  }
  return(out)
}

print.groupr <- function(groupr_obj, include_colnames = F){
  groups <- names(groupr_obj$n_1_group)

  indent <- "  "
  tree_stem <- "|_"
  newline <- "\n"
  for(grouping_level in names(groupr_obj)){
    cat(grouping_level)
    cat(newline)
    for(df_name in names(groupr_obj[[grouping_level]])){
      cat(indent)
      cat(tree_stem)
      cat(df_name)
      if(include_colnames) {
        for(col in colnames(groupr_obj[[grouping_level]][[df_name]])){
          cat(newline)
          cat(indent)
          cat(indent)
          cat(tree_stem)
          cat(col)
        }
      }
      cat(newline)
    }
  }
}
