is.groupr <- function(x) inherits(x, "groupr")
as.groupr <- function(in_list) structure(in_list, class="groupr")
as.xts_groupr <- function(in_list) structure(in_list, class=c("xts_groupr", "groupr"))

#' Printing groupr objects
#'
#' Print a groupr object as tree.
#'
#' @export
#' @param groupr_obj A grouping object created with \code{groupr}
#' @param include_colnames Boolean for whether or not to print colnames in tree
#' @examples
#' groupr <- groupr(permits, groups = c('type_desc', 'issued_month', 'existing_use'))
#' print(groupr)
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
