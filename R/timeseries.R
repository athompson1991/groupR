#' Extracts time series data from grouping object.
#'
#' If one of the groups in a \code{groupr} object is a date, then the object can
#' be manipulated into an \code{xts} based \code{groupr} object. This is done by
#' casting the date column against the unique values in the other grouping
#' columns. A value column must be chosen in order to "fill in" the returned
#' dataframe. The combination between the unique group values for the groups
#' which aren't dates can result in many columns per resultant dataframe, so
#' caution should be exercised when producing \code{xts} based \code{groupr}
#' objects.
#'
#' @export
#' @importFrom stats as.formula
#' @param groupr Grouping object created with \code{groupr}
#' @param value_choice The column which will has values that will be in time
#'   series. The function \code{cast} from the \code{reshape} package is called
#'   to aggregate data. The function  will be \code{sum} but each value should
#'   be unique when cast against the remaining groups.
#' @param date_column The column with date data which will represent unique
#'   index for the returned \code{xts}
#' @return An \code{xts_groupr} object with the date column cast against the
#'   groups column, using \code{sum} to summarize the value column
#' @examples
#' permits_groupr <- groupr(
#'   permits,
#'   groups = c("issued_month", "existing_const_type")
#' )
#' permits_xts <- extract_xts(
#'   permits_groupr,
#'   value_choice = "count",
#'   date_column = "issued_month"
#' )
extract_xts <- function(groupr, value_choice, date_column = "dd_dt") {

  groups <- get_groups(groupr)
  non_date_groups <- groups[groups != date_column]
  new_meta <- groupr$meta
  new_meta$groups <- non_date_groups
  new_meta$date_column <- date_column
  core <- drop_grouping_level(groupr, length(groupr))
  fixed_groupr <- subset(core, date_column, type = "intersect")

  top_applied <- lapply(fixed_groupr, function(grouping_level) {
    xts_list <- lapply(grouping_level, function(df) {
      column_names <- colnames(df)
      return_xts <- NULL
      is_overall <- setequal(column_names, c(date_column, value_choice))
      if (is_overall)
        return_xts <- df_to_xts(df, date_column, is_overall = T)
      else{
        logic <- which(column_names %in% non_date_groups)
        groups_subset <- column_names[logic]
        collapse <- paste(groups_subset, collapse = "+")
        formula_text <- paste(date_column, "~", collapse)
        if (length(groups_subset) > 0) {
          casting_formula <- as.formula(formula_text)
          casted_df <-
            reshape::cast(df,
                          casting_formula,
                          fun.aggregate = sum,
                          value = value_choice)
          return_xts <- df_to_xts(casted_df, date_column)
          new_colnames <- new_xts_names(df, groups_subset)
          colnames(return_xts) <- new_colnames
        }
      }
      return(return_xts)
    })
    xts_list <- rename_xts_list(xts_list, date_column)
    xts_list[sapply(xts_list, is.null)] <- NULL
    return(xts_list)
  })

  top_applied <- clean_extracted_groupr(top_applied)
  top_applied$meta <- new_meta

  top_applied <- as.xts_groupr(top_applied)
  return(top_applied)
}

df_to_xts <- function(df, date_column, is_overall = F) {
  date_index <- which(colnames(df) == date_column)
  if(is_overall)
    order_by <- dplyr::pull(df, date_column)
  else
    order_by <- df[, date_index]
  series <- df[, -date_index]
  out <- xts::xts(x = series, order.by = order_by)
  return(out)
}

rename_xts_list <- function(xts_list, date_column){
  d1 <- "(\\.\\.\\.*"
  d2 <- "*\\.\\.\\.)"
  replace <- paste0(d1, date_column, ")|(", date_column, d2)
  names(xts_list) <- gsub(names(xts_list), pattern = replace, replacement = "")
  return(xts_list)
}

#' Model time series list with ARIMA
#'
#'
#'
#' @export
#' @param xts_gr_obj A list of \code{xts} data produced with \code{extract_xts}.
#' @param ... Arguments to be passed to \code{Arima} function (from the
#'   \code{forecast} package).
#' @param is_auto_arima A boolean value to either specify a model explicitly or
#'   to use the \code{auto.arima} function from the \code{forecast} package.
#' @param parallelize Whether or not to use the \code{parallel} package to do
#'   calculations
#' @param interval Either "month", "week", or "day" for calculation.
#'   Alternatively, you can provide a numeric value to be passed to
#'   \code{frequency} for the \code{ts} object.
xts_to_arima_model <- function(xts_groupr, ..., is_auto_arima = TRUE,
           parallelize = FALSE, interval = "month") {
    if (parallelize) {
      core_count <- parallel::detectCores() - 1
      cl <- parallel::makeCluster(core_count)
      parallel::clusterExport(cl, list("make_ts", "fill_xts"))
      arima_mdls <-
        function(df)
          parallel::parApply(cl, df, 2, FUN = do_modeling, ... = ...,
                             is_auto_arima = is_auto_arima, interval = interval
          )
    }
    else
      arima_mdls  <- function(df)
          apply(df, 2, do_modeling, ...  = ...,
                is_auto_arima = is_auto_arima,interval = interval)
    out <- gapply(xts_groupr, list(mdl = arima_mdls), is_cbind = F)

    if (parallelize)
      parallel::stopCluster(cl)
    return(out)
  }

fill_xts <- function(xts_ser, interval, fill_val = 0){
  dt_range <- range(zoo::index(xts_ser))
  dt_seq <- seq.Date(from = dt_range[1], to = dt_range[2], by = interval)
  zeros <- xts::xts(rep(0, length(dt_seq)), order.by = dt_seq)
  merged <- zoo::merge.zoo(zeros, xts_ser)
  out <- merged[ ,2]
  out[is.na(out)] <- fill_val
  out <- xts::as.xts(out)
  return(out)
}

do_modeling <- function(xts_column, is_auto_arima = F, interval, ...) {

  ts_data <- make_ts(xts_column, interval)

  if (is_auto_arima)
    model <- forecast::auto.arima(ts_data)
  else
    model <- forecast::Arima(y = ts_data, ...)
  return(model)
}

make_ts <- function(xts_column, interval){
  index   <- as.Date(names(xts_column))
  first_ind <- xts::first(index)
  ind_ymd <- lubridate::ymd(first_ind)
  st <- lubridate::decimal_date(first_ind)

  index_year    <- lubridate::year(first_ind)
  index_month   <- lubridate::month(first_ind)
  index_day     <- lubridate::day(first_ind)
  time_list <- list(year = index_year,
                    month = index_month,
                    day = index_day)

  if (interval == "month")
    ts_data <- stats::ts(xts_column, frequency = 12, start = st)
  else if (interval == "week")
    ts_data <- stats::ts(xts_column, frequency = 52, start = st)
  else if (interval == "day")
    ts_data <- stats::ts(xts_column, frequency = 365, start = st)
  else if (is.numeric(interval))
    ts_data <- stats::ts(xts_column, frequency = interval, start = st)
  else
    stop("Bad interval choice")
  return(ts_data)
}

clean_extracted_groupr <- function(groupr) {
  groupr$n_0_group <- NULL
  vals <- 0:(length(groupr) - 1)
  names(groupr) <- paste("n", vals, "group", sep = "_")
  groupr$n_0_group <- groupr$n_0_group[[1]]
  names(groupr$n_0_group) <- "overall"
  return(groupr)
}

new_xts_names <- function(df, groups) {
  data_combn <- as.data.frame(dplyr::distinct(df[, groups]))
  new_col_names <- apply(data_combn, 1, paste0, collapse = "/")
  new_col_names <- gsub(pattern = " ", "_", new_col_names)
  new_col_names <- tolower(new_col_names)
  new_col_names[new_col_names == ""] <- "blank_string"
  return(new_col_names)
}
