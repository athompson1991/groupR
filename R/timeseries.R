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

  init <- make_init_list(groupr, date_column)

  top_applied <- lapply(init$fgroupr, function(grouping_level) {
    xts_list <- lapply(grouping_level, function(df) {
      column_names <- colnames(df)
      return_xts <- NULL
      is_overall <- setequal(column_names, c(date_column, value_choice))
      if (is_overall)
        return_xts <- df_to_xts(df, date_column, is_overall = T)
      else{
        logic <- which(column_names %in% init$groups)
        groups_subset <- column_names[logic]
        if (length(groups_subset) > 0) {
          casted_df <- do_extract(
            groups_subset, date_column, df, value_choice
          )
          return_xts <- df_to_xts(casted_df, date_column)
          colnames(return_xts) <- new_xts_names(df, groups_subset)
        }
      }
      return(return_xts)
    })
    xts_list <- rename_xts_list(xts_list, date_column)
    xts_list[sapply(xts_list, is.null)] <- NULL
    return(xts_list)
  })

  top_applied <- clean_extracted_groupr(top_applied)
  top_applied$meta <- init$new_meta
  top_applied <- as.xts_groupr(top_applied)
  return(top_applied)
}

do_extract <- function(groups, date_column, df, value_choice) {
  plus <- paste(groups, collapse = "+")
  formula_text <- paste(date_column, "~", plus)
  casting_formula <- as.formula(formula_text)
  casted_df <- reshape::cast(
    data = df,
    formula = casting_formula,
    fun.aggregate = sum,
    value = value_choice
  )
  return(casted_df)
}

make_init_list <- function(groupr, date_column){
  groups <- get_groups(groupr)
  groups <- groups[groups != date_column]
  new_meta <- groupr$meta
  new_meta$groups <- groups
  new_meta$date_column <- date_column
  core <- drop_grouping_level(groupr, length(groupr))
  fixed_groupr <- subset(core, date_column, type = "intersect")
  out <- list(
    groups = groups,
    new_meta = new_meta,
    fgroupr = fixed_groupr
  )
  return(out)
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

#' Model with ARIMA
#'
#' This function allows the user to model all the combinations of \code{xts}
#' observations extracted using \code{extract_xts}. Because there can be many
#' columns in an \code{xts_groupr} object, there is the option to do model estimation in parallel.
#'
#' @export
#' @param xts_groupr A \code{groupr} object produced with \code{extract_xts}.
#' @param ... Arguments to be passed to \code{Arima} function (from the
#'   \code{forecast} package).
#' @param is_auto_arima A boolean value to either specify a model explicitly or
#'   to use the \code{auto.arima} function from the \code{forecast} package.
#' @param parallelize Whether or not to use the \code{parallel} package to do
#'   calculations
#' @param interval Either "month", "week", or "day" for calculation.
#'   Alternatively, you can provide a numeric value to be passed to
#'   \code{frequency} for the \code{ts} object.
xts_to_arima_model <- function(
  xts_groupr,
  ...,
  is_auto_arima = TRUE,
  parallelize = FALSE,
  interval = "month",
  cycle = "annual"
  ) {
    if (parallelize) {
      core_count <- parallel::detectCores() - 1
      cl <- parallel::makeCluster(core_count)
      parallel::clusterExport(cl, list("make_ts", "fill_xts"))
      arima_mdls <- function(df)
        parallel::parApply(
          cl, df, 2, FUN = do_modeling, ... = ...,
          is_auto_arima = is_auto_arima, interval = interval, cycle = cycle
        )
    }
    else
      arima_mdls  <- function(df)
        apply(
          df, 2, do_modeling, ...  = ...,
          is_auto_arima = is_auto_arima, interval = interval
        )
    out <- gapply(xts_groupr, list(mdl = arima_mdls), is_cbind = F)

    if (parallelize)
      parallel::stopCluster(cl)
    return(out)
}

fill_xts <- function(xts_series, interval, fill_val = 0) {
  bad_interval = !(interval %in% c("day", "week", "month", "quarter", "year"))
  if(bad_interval)
    stop("Bad interval choice, see details in seq.Date documentation")

  date_range <- range(zoo::index(xts_series))
  dt_seq <- seq.Date(
    from = date_range[1], to = date_range[2],
    by = interval)
  zeros <- xts::xts(rep(0, length(dt_seq)), order.by = dt_seq)
  merged <- zoo::merge.zoo(zeros, xts_series)
  out <- merged[, 2]
  out[is.na(out)] <- fill_val
  out <- xts::as.xts(out)
  return(out)
}

do_modeling <- function(xts_column, is_auto_arima = F, interval, cycle, ...) {
  ts_data <- make_ts(xts_column, interval = interval, cycle = cycle)

  if (is_auto_arima)
    model <- forecast::auto.arima(ts_data)
  else
    model <- forecast::Arima(y = ts_data, ...)
  return(model)
}

make_ts <- function(xts_column, interval = "month", cycle = "year"){
  index   <- zoo::index(xts_column)
  first_ind <- xts::first(index)

  valid_intervals <- c("month", "week", "day")
  valid_cycles <- c("year", "week")
  bad_interval <- !(interval %in%  valid_intervals | is.numeric(interval))
  bad_cycle <- !(cycle %in% valid_cycles)

  if(bad_interval)
    stop("Bad interval choice")

  if(bad_cycle)
    stop("Bad cycle choice")

  if(cycle == "year") {
    start <- lubridate::decimal_date(first_ind)
    if (interval == "month")
      ts_data <- stats::ts(xts_column, frequency = 12, start = start)
    else if (interval == "week")
      ts_data <- stats::ts(xts_column, frequency = 365.25 / 7, start = start)
    else if (interval == "day")
      ts_data <- stats::ts(xts_column, frequency = 365.25, start = start)
    else if (is.numeric(interval))
      ts_data <- stats::ts(xts_column, frequency = interval, start = start)
    else
      stop("Bad interval choice")
  } else if(cycle == "week") {
    if (interval == "day")
      ts_data <- stats::ts(xts_column, frequency = 7, start = 1)
  }

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
