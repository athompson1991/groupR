
#' Make every combination of character vector
#' @param n Maximum rows per combination data frame
#' @param char_vec Character vector to be used
#' @return A list of data frames with every column a unique combination of the given character vector
#' @examples
#' x <- c("foo", "bar", "baz", "potato")
#' y <- get_combinations(3, x)
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



