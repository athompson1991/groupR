#' Batting data for MLB teams.
#'
#' Team batting statistics at the player level. Data pulled on 2017-12-14.
#'
#' @format A data frame with 1494 rows and 31 columns
#' \describe{
#'    \item{league}{American League or National League}
#'    \item{division}{East, Central, or West}
#'    \item{team}{Team name}
#'    \item{Rk}{Player ranking}
#'    \item{Pos}{Player position}
#'    \item{Name}{Player name}
#'    \item{Age}{Player’s age at midnight of June 30th of that year}
#'    \item{G}{Games Played or Pitched}
#'    \item{PA}{Plate Appearances}
#'    \item{AB}{At Bats}
#'    \item{R}{Runs Scored}
#'    \item{H}{Hits}
#'    \item{2B}{Doubles}
#'    \item{3B}{Triples}
#'    \item{HR}{Home Runs}
#'    \item{RBI}{Runs Batted In}
#'    \item{SB}{Stolen Bases}
#'    \item{CS}{Caught Stealing}
#'    \item{BB}{Bases on Balls}
#'    \item{SO}{Strikeouts}
#'    \item{BA}{Batting Average}
#'    \item{OBP}{On-Base Percentage}
#'    \item{SLG}{Slugging Percentage}
#'    \item{OPS}{On-Base Plus Slugging}
#'    \item{OPS_adj}{Adjusted OPS+}
#'    \item{TB}{Total Bases}
#'    \item{GDP}{Double Plays Grounded Into}
#'    \item{HBP}{Hit By Pitch}
#'    \item{SH}{Sacrifice Hits}
#'    \item{SF}{Sacrifice Flies}
#'    \item{IBB}{Intentional Bases on Balls}
#' }
#' @source \url{https://www.baseball-reference.com}
"baseball"