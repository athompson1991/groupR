is.groupr <- function(x) inherits(x, "groupr")
as.groupr <- function(in_list) structure(in_list, class="groupr")
as.xts_groupr <- function(in_list) structure(in_list, class=c("xts_groupr", "groupr"))

#' Apply across a groupr object
#'
#' An S3 method for applying a function to a groupr object. This is typically a way to simply loop through
#' the object which simplifies complicated \code{lapply} functions.
#'
#' @export
#' @param groupr The \code{groupr} object to loop through
#' @param new_functions The new functions to apply
#' @param is_cbind Whether or not to cbind to the original dataframes
gapply <- function(groupr, new_functions, is_cbind = F) UseMethod("gapply")

#' Printing groupr objects
#'
#' Print a groupr object as tree.
#'
#' @export
#' @param x A grouping object created with \code{groupr}
#' @param include_colnames Boolean for whether or not to print colnames in tree
#' @param ... Additional arguments
#' @examples
#' groupr <- groupr(permits, groups = c('type_desc', 'issued_month', 'existing_use'))
#' print(groupr)
print.groupr <- function(x, include_colnames = F, ...){
  groups <- names(x$n_1_group)

  indent <- "  "
  tree_stem <- "|_"
  newline <- "\n"
  for(grouping_level in names(x)){
    cat(grouping_level)
    cat(newline)
    if(grouping_level != "n_0_group"){
      for(df_name in names(x[[grouping_level]])){
        cat(indent)
        cat(tree_stem)
        cat(df_name)
        if(include_colnames) {
          for(col in colnames(x[[grouping_level]][[df_name]])){
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
