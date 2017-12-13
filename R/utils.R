
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

#' Write grouping object to csv files.
#'
#' @export
#' @param grouping_obj Grouping object made with \code{get_groups}
#' @param path Out directory for files
#' @return
#'
write_grouping_files <- function(grouping_obj, path){
  for(i in 1:(length(grouping_obj) - 1)){
    temp_ls <- grouping_obj[[i]]
    for(j in 1:length(temp_ls)){
      temp_nm <- names(temp_ls)[j]
      temp_nm <- gsub("/", "__", temp_nm)
      f_nm <- paste(path, temp_nm, ".csv", sep = "")
      write.csv(temp_ls[[j]], f_nm, row.names = F)
    }
  }
  f_nm <- paste(path, "no_groups.csv", sep = "/")
  write.csv(grouping_obj[["no_groups"]], f_nm, row.names = F)
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
