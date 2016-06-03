library(dplyr)

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



#' Create grouping object.
#'
#' This function takes a data frame as input and returns a grouping object as output. The grouping object is a list of lists of data frames, one data frame for every possible combination of groups.
#' For example, if there are two groups, A and B, four data frames are returned: data frames for A, B, both A and B, and a data frame summarizing the whole dataset.
#'
#' 
#' @param df A data frame of raw data.
#' @param groups A character vector of column names in df that will be basis for aggregation.
#' @param functions A named list of functions for aggregation. 
#' @return A grouping object (list that aggregates functions by every potential combination)
#' @examples 
#' default_data <- data.frame(  my_group1 = c(rep("A", 4), rep("B", 4))
#'                            , my_group2 = c(rep("foo", 2), rep("bar", 2), rep("baz", 2), rep("potato", 2))
#'                            , my_data = runif(n = 8)
#'                           )
#' grouping_obj <- get_groups(df = default_data, groups = "my_group", functions = list(rando_sum = "sum(my_data)"))
get_groups <- function(df, groups, functions = list("count" = "n()")){
  group_combn <- get_combinations(length(groups), groups)
  n_fn <- length(functions)
  col_names <- names(functions)
  functions <- as.vector(sapply(col_names, function(f) functions[[f]]))
  
  out_list <- lapply(group_combn, function(mtx){
    
    temp_df <- apply(mtx, 2, function(col){
      temp_groups <- as.vector(col)
      temp_ls <- lapply(functions, function(f){
        temp_fn <- df %>%
          group_by_(.dots = temp_groups) %>%
          summarize_(lazyeval::interp(f))
        temp_fn
      })
      temp_out <- do.call(cbind, temp_ls)
    })
    
    comb_names <- apply(mtx, 2, function(col) paste(col, collapse = "/"))
    names(temp_df) <- comb_names
    temp_df
  })
  
  names(out_list) <- paste("n_", 1:length(groups), "_group", sep = "")
  out_list <- lapply(out_list, function(ls){
    lapply(ls, function(df){
      df <- df[ ,!duplicated(colnames(df))]
      colnames(df)[tail(1:ncol(df), n_fn)] <- col_names
      df
    })
  })
  
  no_group <- lapply(functions, function(f){
    df %>% summarize_(lazyeval::interp(f))
  })
  no_group_df <- do.call(cbind, no_group)
  colnames(no_group_df) <- col_names
  out_list[["no_groups"]] <- no_group_df

  out_list
}


#' Plot timeseries groups of grouping object.
#'
#' @param gr_obj Grouping object created with \code{get_groups}
#' @param choice Grouping choices from grouping object
#' @param data_choice Calculated column to be plotted (created with the function argument in \code{get_groups})
#' @param filter Character string that filters the groups to be plotted
#' @param main Title of plot
#' @param date_col Group that represents date for plotting
#' @param window Date subset (\code{xts} subsetting)
#' @param ... Graphics arguments
#' @param date_axis Date format for axis
#'
do_ts_plot <- function(gr_obj, choice, data_choice, filter = NA, main = "Timeseries", date_col = "dd_dt", window = NA, ..., date_axis = "%b-%Y"){
  
  
  choice_addition <- paste(choice, collapse = " + ")
  n = length(choice) + 1
  choice = c(date_col, choice)
  ls_name <- paste("n", n, "group", sep = "_")
  df_name <- paste(choice, collapse = "/")
  fn <- paste(date_col," ~ ", choice_addition, sep = "")
  writeLines(paste("df: ", df_name, "\nls: ", ls_name, "\nfn: ",fn, sep = ""))
  cast_obj <- reshape::cast(gr_obj[[ls_name]][[df_name]], fn, fun.aggregate = sum, value = lazyeval::interp(data_choice))
  date_ls <- cast_obj[ ,date_col]
  cast_obj <- cast_obj[ ,colnames(cast_obj) != date_col]
  if(!is.na(filter)){cast_obj <- cast_obj[ ,eval(parse(text = filter))]}
  xts_obj <- xts::xts(cast_obj, order.by = date_ls)
  if(!is.na(window)){xts_obj <- xts_obj[window]}

    my_panel <- function(x, y, ..., pf = parent.frame(), temp_names){
      n_dt <- length(index(xts_obj))
      n_col <- ncol(xts_obj)
      panel_num <- pf$panel.number
      at_vals <- index(xts_obj)[seq(1, n_dt, by = ceiling(n_dt / 10))]
      lab_vals <- as.character(index(xts_obj)[seq(1, n_dt, by = ceiling(n_dt / 10))], date_axis)

      y_rng <- range(y)
      y_chg <- ceiling((y_rng[2] - y_rng[1]) / 10)
      y_lg <- floor(log10(y_chg))
      y_chg <- round(y_chg, -y_lg)
      y_bt <- round(y_rng[1], -y_lg) - y_chg
      y_tp <- round(y_rng[2], -y_lg) + y_chg
      y_int <- seq(y_bt, y_tp, by = y_chg)
      
      abline(h = y_int, v = at_vals, lty = 4, col = "grey")
      lines(x, y, ...)
      title(main = paste("\n", temp_names[panel_num], sep = ""), cex = 0.5, ...)
      
      axis(side = 2, at = y_int, labels = y_int, las = 1)
      if(panel_num == n_col || n_col > 4 & panel_num == ceiling(n_col / 2)){
        axis(side = 1, at = at_vals, labels = lab_vals, las = 2)
      }
  }
  
  plot(as.zoo(xts_obj), plot.type = "multiple", main = main, panel = my_panel, temp_names = colnames(xts_obj), ylab = NA, xlab = NA, las = 2, ..., xaxt = "n", yaxt = "n")
}

#' Write grouping object to csv files.
#' 
#' @param grouping_obj Grouping object made with \code{get_groups}
#' @param path Out directory for files
#' @return 
#'
#'
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
  write.csv(grouping_obj[["no_groups"]], "no_groups.csv", row.names = F)
}

#' Creates \code{xts} object from grouping object.
#'
#' @param grouping_obj Grouping object created with get_groups
#' @param choice Vector of grouping objects 
#' @return An \code{xts} object casted with unique group values as column names
make_xts <- function(grouping_obj, choice, date_col = "dd_dt"){
  choice_addition <- paste(choice, collapse = " + ")
  n = length(choice) + 1
  full_choice = c(date_col, choice)
  ls_name <- paste("n", n, "group", sep = "_")
  df_name <- paste(full_choice, collapse = "/")
  temp_df <- grouping_obj[[ls_name]][[df_name]]
  formula_text <- as.formula(paste(date_col, '~', choice_addition, sep = " "))
  casted_df  <- cast(temp_df, formula_text)
  temp_colnames <- colnames(casted_df)
  casted_xts <- xts::xts(casted_df[ ,-1], order.by = casted_df[ ,1])
  colnames(casted_xts) <- temp_colnames[-1]
  casted_xts
}
