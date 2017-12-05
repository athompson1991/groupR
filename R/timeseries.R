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

get_forecast_stats <- function(forecast_ls, original_ls, date_type = "months"){
  out_ls <- list()
  for(i in 1:length(forecast_ls)){
    grouping_n <- forecast_ls[[i]]
    field_ls <- list()
    for(j in 1:length(grouping_n)){
      df_ls <- list()
      temp_field <- grouping_n[[j]]
      for(k in 1:length(temp_field)){

        temp_fcst  <- temp_field[[k]]
        temp_mean  <- floor(temp_fcst$mean)
        temp_lower <- floor(temp_fcst$lower)
        temp_upper <- floor(temp_fcst$upper)

        temp_xts <- original_ls[[i]][[j]][ ,k]
        original_dates <- as.Date(index(temp_xts))
        previous_dates <- last(original_dates, length(temp_mean))
        previous_obs   <- temp_xts[previous_dates]

        last_date <- last(original_dates)
        if(date_type == "months"){
          next_dates <- as.Date(sapply(1:length(temp_mean), function(n) last_date + months(n)))
        }

        out_df <- data.frame(row.names= 1:length(temp_mean), previous_dates, previous_obs, next_dates, temp_mean, temp_upper, temp_lower)
        colnames(out_df) <- c("last_dates", "last_values", "next_dates", "point_fcst", "upper95", "upper80", "lower95", "lower80")
        df_ls[[k]] <- out_df
        names(df_ls)[k] <- colnames(original_ls[[i]][[j]])[k]
      }
      field_ls[[j]] <- df_ls
      names(field_ls)[j] <- names(grouping_n)[j]
    }
    out_ls[[i]] <- field_ls
    names(out_ls)[i] <- names(forecast_ls)[i]
  }
  return(out_ls)
}

custom_forecast_plot <- function(fcst_obj){
  upper <- fcst_obj$upper
  lower <- fcst_obj$lower

  colnames(upper) <- gsub("\\%", "", colnames(upper))
  colnames(upper) <- paste("upper",colnames(upper), sep = "_")
  colnames(lower) <- gsub("\\%", "", colnames(lower))
  colnames(lower) <- paste("lower",colnames(lower), sep = "_")

  new_data <- fcst_obj$mean
  old_data <- fcst_obj$x
  old_dates <- as.Date(names(old_data))
  last_date <- last(old_dates)
  new_dates <- as.Date(sapply(1:length(new_data), function(n) last_date + months(n)))
  full_dates <- c(old_dates, new_dates)
  full_data <- c(old_data, new_data)


  upper_lower_df <- as.data.frame(cbind(upper, lower))
  upper_lower_df$dates <- new_dates

  new_xts <- xts(full_data, order.by = full_dates)
  df <- data.frame(fcst = new_xts)
  df$dates <- full_dates
  p <- ggplot(df, aes(x = dates, y = fcst))
  p_full <- p +
    geom_line() +
    geom_polygon(data = upper_lower_df, aes(x = rep(dates, 2), y = c(upper_95, lower_95)), fill = "red", alpha = 0.6) +
    geom_polygon(data = upper_lower_df, aes(x = rep(dates, 2), y = c(upper_80, lower_80)), fill = "red", alpha = 0.6)
  p_full
}


do_modeling <- function(z_data, is_auto_arima = F, ...){
  z_ind   <- as.Date(names(z_data))
  z_yr    <- year(first(z_ind))
  z_mth   <- month(first(z_ind))
  ts_data <- ts(z_data, frequency = 12, start = c(z_yr, z_mth))

  if(is_auto_arima){
    temp_model <- forecast::auto.arima(ts_data)
  }else{
    temp_model <- forecast::Arima(ts_data, ...)
  }
  temp_model
}

#' Plot timeseries groups of grouping object.
#'
#' Though a grouping object can be converted to more sophisticated time series data and then plotted,
#' it can be convenient to go directly from a grouping object to a time series plot. This function provides that capability.
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

#' Extracts time series data from grouping object.
#'
#' @export
#' @param grouping_obj Grouping object created with \code{get_groups}
#' @param groups Groups that will be converted to time series data (\code{xts} object)
#' @param value_choice The column which will has values that will be in time series. Function applied will be \code{sum} but each value should be unique.
#' @param date_col The column with date data which will represent unique index for the returned \code{xts}
#' @return An \code{xts} object with the date column cast against the groups column, using \code{sum} to summarize the value column
#'
extract_xts <- function(grouping_obj, groups, value_choice, date_col = "dd_dt"){

  # From StackOverflow: http://stackoverflow.com/questions/26539441
  is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))
  rmNullObs <- function(x) {
    x <- Filter(Negate(is.NullOb), x)
    lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
  }

  main_ls <- lapply(grouping_obj[-length(grouping_obj)], function(x){
    lapply(x, function(y){
      c_nms <- colnames(y)
      if(date_col %in% c_nms){
        temp_groups <- c_nms[c_nms %in% groups]
        if(length(temp_groups) > 0){
          temp_fm <- as.formula(paste(date_col, "~", paste(temp_groups, collapse = "+")))
          temp_dt <- reshape::cast(y, temp_fm, fun.aggregate = sum, value = value_choice)
          temp_ind <- which(colnames(temp_dt) == date_col)
          xts::xts(temp_dt[ ,-temp_ind], order.by = temp_dt[ ,temp_ind])
        }else{
          temp_ind <- which(colnames(y) == date_col)
          xts::xts(y[ ,-temp_ind], order.by = y[ ,temp_ind])
        }
      }else{
        NULL
      }
    })
  })

  main_ls <- rmNullObs(main_ls)
  main_ls <- main_ls[1:length(groups)]
  main_ls
}

get_forecasts <- function(model_ls){
  lapply(model_ls, function(i){
    lapply(i, function(j){
      temp_group <- j[["mdl"]]
      lapply(temp_group, function(k){
        forecast(k)
      })
    })
  })
}
