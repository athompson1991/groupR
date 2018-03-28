#' Extracts time series data from grouping object.
#'
#' @export
#' @param groupr Grouping object created with \code{get_groups}
#' @param groups Groups that will be converted to time series data (\code{xts} object)
#' @param value_choice The column which will has values that will be in time series. The function \code{cast} from the \code{reshape} package is called to aggregate data.The function  will be \code{sum} but each value should be unique.
#' @param date_col The column with date data which will represent unique index for the returned \code{xts}
#' @return An \code{xts} object with the date column cast against the groups column, using \code{sum} to summarize the value column
#'
extract_xts <- function(groupr, value_choice, date_column = "dd_dt"){
  groups <- names(groupr$n_1_group)
  groups <- groups[groups != date_column]
  fixed_groupr <- subset(groupr, date_column, type = "intersect")
  out <- lapply(fixed_groupr, function(grouping_level){
    xts_list <- lapply(grouping_level, function(df){
      column_names <- colnames(df)
      return_xts <- NULL
      is_overall <- setequal(column_names, c(date_column, value_choice))
      if(is_overall){
        date_index <- which(colnames(df) == date_column)
        return_xts <- xts::xts(df[ ,-date_index], order.by = dplyr::pull(df, date_column))
      } else{
        groups_subset <- column_names[which(column_names %in% groups)]
        formula_text <- paste(date_column, "~", paste(groups_subset, collapse = "+"))
        if(length(groups_subset) > 0){
          casting_formula <- as.formula(formula_text)
          casted_df <- reshape::cast(df, casting_formula, fun.aggregate = sum, value = value_choice)
          date_index <- which(colnames(casted_df) == date_column)
          return_xts <- xts::xts(casted_df[ ,-date_index], order.by = casted_df[ ,date_index])
          new_colnames <- new_xts_names(df, groups_subset)
          if(length(new_colnames) == ncol(return_xts))
            colnames(return_xts) <- new_colnames
        }
      }
      return(return_xts)
    })
    replacement_pattern <- paste0("(*\\.\\.\\.", date_column, ")|(", date_column, "\\.\\.\\.*)")
    names(xts_list) <- gsub(names(xts_list), pattern = replacement_pattern, replacement="")
    xts_list[sapply(xts_list, is.null)] <- NULL
    return(xts_list)
  })

  out <- as.groupr(out)
  out <- clean_extracted_groupr(out)
  return(out)
}

#' Models time series list with an ARIMA model.
#'
#' @export
#' @param xts_gr_obj A list of \code{xts} data produced with \code{extract_xts}.
#' @param ... Arguments to be passed to \code{Arima} function (from the \code{forecast} package).
#' @param is_auto_arima A boolean value to either specify a model explicitly or to use the \code{auto.arima} function from the \code{forecast} package.
#'
xts_to_arima_model <- function(xts_gr_obj,  ..., is_auto_arima=T, parallelize=F, interval="month"){
  if(parallelize){
    core_count <- parallel::detectCores()-1
    cl <- parallel::makeCluster(core_count)
    arima_mdls <- function(df) parallel::parApply(cl, df, 2, FUN = do_modeling, ...=..., is_auto_arima=is_auto_arima, interval=interval)
  }
  else
    arima_mdls  <- function(df) apply(df, 2, do_modeling, ... = ..., is_auto_arima = is_auto_arima, interval=interval)
  out <- group_obj_apply(xts_gr_obj, list(mdl = arima_mdls), is_cbind = F)
  if(parallelize)
    parallel::stopCluster(cl)
  return(out)
}

do_modeling <- function(xts_column, is_auto_arima = F, interval, ...){
  index   <- as.Date(names(xts_column))
  index_year    <- lubridate::year(xts::first(index))
  index_month   <- lubridate::month(xts::first(index))
  index_day     <- lubridate::day(xts::first(index))

  if(interval == "month")
    ts_data <- stats::ts(xts_column, frequency = 12, start = c(index_year, index_month))
  else if(interval == "week")
    ts_data <- stats::ts(xts_column, frequency = 52)
  else if(interval == "day")
    ts_data <- stats::ts(xts_column, frequency = 7)
  else if(is.numeric(interval))
    ts_data <- stats::ts(xts_column, frequency = interval)
  else
    stop("Bad interval choice")

  if(is_auto_arima)
    model <- forecast::auto.arima(ts_data)
  else
    model <- forecast::Arima(y = ts_data, ...)

  model
}

clean_extracted_groupr <- function(groupr){
  groupr$n_0_group <- NULL
  names(groupr) <- paste("n", 0:(length(groupr) - 1), "group", sep= "_")
  groupr$n_0_group <- groupr$n_0_group[[1]]
  names(groupr$n_0_group) <- "overall"
  return(groupr)
}

new_xts_names <- function(df, groups){
  data_combinations <- as.data.frame(dplyr::distinct(df[ ,groups]))
  new_col_names <- apply(data_combinations, 1, paste0, collapse="/")
  new_col_names <- gsub(pattern = " ", "_", new_col_names)
  new_col_names <- tolower(new_col_names)
  new_col_names[new_col_names == ""] <- "blank_string"
  return(new_col_names)
}
