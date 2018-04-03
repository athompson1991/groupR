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
  groups <- groups[groups != date_column]
  core <- drop_grouping_level(groupr, length(groupr))
  fixed_groupr <- subset(core, date_column, type = "intersect")
  out <- lapply(fixed_groupr, function(grouping_level) {
    xts_list <- lapply(grouping_level, function(df) {
      column_names <- colnames(df)
      return_xts <- NULL
      is_overall <- setequal(column_names, c(date_column, value_choice))
      if (is_overall) {
        date_index <- which(colnames(df) == date_column)
        series <- df[, -date_index]
        order_by <- dplyr::pull(df, date_column)
        return_xts <- xts::xts(series, order.by = order_by)
      } else{
        logic <- which(column_names %in% groups)
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
          date_index <- which(colnames(casted_df) == date_column)
          order_by <- casted_df[, date_index]
          series <- casted_df[, -date_index]
          return_xts <- xts::xts(x = series, order.by = order_by)
          new_colnames <- new_xts_names(df, groups_subset)
          if (length(new_colnames) == ncol(return_xts))
            colnames(return_xts) <- new_colnames
        }
      }
      return(return_xts)
    })
    d1 <- "(\\.\\.\\.*"
    d2 <- "*\\.\\.\\.)"
    replace <- paste0(d1, date_column, ")|(", date_column, d2)
    names(xts_list) <-
      gsub(names(xts_list),
           pattern = replace,
           replacement = "")
    xts_list[sapply(xts_list, is.null)] <- NULL
    return(xts_list)
  })

  out <- clean_extracted_groupr(out)
  new_meta <- groupr$meta
  new_meta$groups <- new_meta$groups[new_meta$groups != date_column]
  new_meta$date_column <- date_column
  out$meta <- new_meta

  out <- as.xts_groupr(out)
  return(out)
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
xts_to_arima_model <-
  function(xts_gr_obj,
           ...,
           is_auto_arima = TRUE,
           parallelize = FALSE,
           interval = "month") {
    if (parallelize) {
      core_count <- parallel::detectCores() - 1
      cl <- parallel::makeCluster(core_count)
      arima_mdls <-
        function(df)
          parallel::parApply(
            cl,
            df,
            2,
            FUN = do_modeling,
            ... = ...,
            is_auto_arima = is_auto_arima,
            interval = interval
          )
    }
    else
      arima_mdls  <-
        function(df)
          apply(
            df,
            2,
            do_modeling,
            ... = ...,
            is_auto_arima = is_auto_arima,
            interval = interval
          )
    out <- gapply(xts_gr_obj, list(mdl = arima_mdls), is_cbind = F)
    if (parallelize)
      parallel::stopCluster(cl)
    return(out)
  }

do_modeling <- function(xts_column, is_auto_arima = F, interval, ...) {
  index   <- as.Date(names(xts_column))
  index_year    <- lubridate::year(xts::first(index))
  index_month   <- lubridate::month(xts::first(index))
  index_day     <- lubridate::day(xts::first(index))

  if (interval == "month")
    ts_data <-
    stats::ts(xts_column,
              frequency = 12,
              start = c(index_year, index_month))
  else if (interval == "week")
    ts_data <- stats::ts(xts_column, frequency = 52)
  else if (interval == "day")
    ts_data <- stats::ts(xts_column, frequency = 7)
  else if (is.numeric(interval))
    ts_data <- stats::ts(xts_column, frequency = interval)
  else
    stop("Bad interval choice")

  if (is_auto_arima)
    model <- forecast::auto.arima(ts_data)
  else
    model <- forecast::Arima(y = ts_data, ...)

  model
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
