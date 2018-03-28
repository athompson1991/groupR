#' Building permits for San Francisco (From Kaggle.com, user Aparna Shasty)
#'
#' This dataset pertains to all types of structural permits from Jan 1, 2013-Feb 25th 2018.
#'
#' @format a data frame with 7 columns
#' \describe{
#' \item{permit_number}{Unique key to identify building permit}
#' \item{type}{Numeric value to identify type}
#' \item{type_desc}{Description for type}
#' \item{status}{Status of permit}
#' \item{status_date}{Date "as of" status}
#' \item{issued_date}{When the permit was issued}
#' \item{location}{Which neighborhood it was issued for in San Francisoco}
#' \item{use}{Existing use of the building}
#' }
#' @source \url{https://www.kaggle.com/aparnashastry/building-permit-applications-data/data}
"permits"
