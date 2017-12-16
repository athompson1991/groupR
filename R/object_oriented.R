is.groupr <- function(x) inherits(x, "groupr")
as.groupr <- function(in_list) structure(in_list, class="groupr")


#' Subset a groupr object
#'
#'
#'
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

#' Printing groupr objects
#'
#' Print a groupr object as tree.
#'
#' @param groupr_obj A grouping object created with \code{get_groups}
#' @param include_colnames Boolean for whether or not to print colnames in tree
#' @examples
#' bball_groupr <- get_groups(baseball, groups = c("league", "team"), functions = list(toal_home_runs = "sum(HR, na.rm=T)", n="n()"))
#' print(bball_groupr)
#' print(bball_groupr, include_colnames = T)
print.groupr <- function(groupr_obj, include_colnames = F){
  groups <- names(groupr_obj$n_1_group)

  indent <- "  "
  tree_stem <- "|_"
  newline <- "\n"
  for(grouping_level in names(groupr_obj)){
    cat(grouping_level)
    cat(newline)
    if(grouping_level != "n_0_group"){
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
}
