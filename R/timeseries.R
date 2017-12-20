#' Extracts time series data from grouping object.
#'
#' @export
#' @param grouping_obj Grouping object created with \code{get_groups}
#' @param groups Groups that will be converted to time series data (\code{xts} object)
#' @param value_choice The column which will has values that will be in time series. The function \code{cast} from the \code{reshape} package is called to aggregate data.The function  will be \code{sum} but each value should be unique.
#' @param date_col The column with date data which will represent unique index for the returned \code{xts}
#' @return An \code{xts} object with the date column cast against the groups column, using \code{sum} to summarize the value column
#'
extract_xts <- function(grouping_obj, groups, value_choice, date_column = "dd_dt"){
  out_list <- lapply(grouping_obj[-length(grouping_obj)], function(grouping_level){
    xts_list <- lapply(grouping_level, function(df){
      column_names <- colnames(df)
      return_xts <- NULL
      if(date_column %in% column_names){
        groups_subset <- column_names[column_names %in% groups]
        if(length(groups_subset) > 0){
          casting_formula <- as.formula(paste(date_column, "~", paste(groups_subset, collapse = "+")))
          casted_df <- reshape::cast(df, casting_formula, fun.aggregate = sum, value = value_choice)
          date_index <- which(colnames(casted_df) == date_column)
          return_xts <- xts::xts(casted_df[ ,-date_index], order.by = casted_df[ ,date_index])
          colnames(return_xts) <- new_xts_names(df, groups_subset)
        }else{
          date_index <- which(colnames(df) == date_column)
          return_xts <- xts::xts(df[ ,-date_index], order.by = dplyr::pull(df, date_column))
        }
      }else{
        return_xts <- NULL
      }
      return(return_xts)
    })
    names(xts_list) <- gsub(names(xts_list), pattern = "*\\.\\.\\.dates", replacement="")
    return(xts_list)
  })

  out_list <- rmNullObs(out_list)
  out_list <- out_list[1:length(groups)]
  names(out_list) <- paste("n", 0:(length(groups)- 1), "group", sep="_")
  out_list$n_0_group <- out_list[[1]][[1]]
  names(out_list$n_0_group) <- "overall"
  out_list <- as.groupr(out_list)
  return(out_list)
}

do_modeling <- function(z_data, is_auto_arima = F, ...){
  z_ind   <- as.Date(names(z_data))
  z_yr    <- lubridate::year(xts::first(z_ind))
  z_mth   <- lubridate::month(xts::first(z_ind))
  ts_data <- stats::ts(z_data, frequency = 12, start = c(z_yr, z_mth))

  if(is_auto_arima){
    temp_model <- forecast::auto.arima(ts_data)
  }else{
    temp_model <- forecast::Arima(ts_data, ...)
  }
  temp_model
}

#' Models time series list with an ARIMA model.
#'
#' @export
#' @param xts_gr_obj A list of \code{xts} data produced with \code{extract_xts}.
#' @param ... Arguments to be passed to \code{Arima} function (from the \code{forecast} package).
#' @param is_auto_arima A boolean value to either specify a model explicitly or to use the \code{auto.arima} function from the \code{forecast} package.
#'
xts_to_arima_model <- function(xts_gr_obj, ..., is_auto_arima){
  arima_mdls  <- function(df) apply(df, 2, do_modeling, ... = ..., is_auto_arima = is_auto_arima)
  group_obj_apply(xts_gr_obj, list(mdl = arima_mdls), is_cbind = F)
}


eliminate_nondates <- function(groupr, date_name){
  worked_on <- lapply(groupr[-1], function(level){
    fixed_level <- lapply(level, function(df){
      if(date_name %in% names(df))
        return(df)
    })
    fixed_level[sapply(fixed_level, is.null)] <- NULL
    return(fixed_level)
  })
  worked_on <- reassign_overall_df(groupr, worked_on)
  worked_on <- as.groupr(worked_on)
  return(worked_on)
}

