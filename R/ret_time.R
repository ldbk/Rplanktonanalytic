#' ret_time
#'
#' Return time vectors from a dataframe containing an as.Date object
#'
#' @param x a dataframe containing a as.Date object named as "date"
#' @return a dataframe containing the time vectors (year, month...)
#' @examples
#' \dontrun{
#' date <- seq(as.Date("2000/1/1"), as.Date("2001/1/1"), length.out = 10)
#' value <- c(seq(1, 10, 1))
#' df <- data.frame(date, value)
#' ret_time(df)
#' }
#' @export
#'
ret_time <- function(x) {
  x$day <- lubridate::yday(x$date)
  x$week <- lubridate::week(x$date)
  x$month <- lubridate::month(x$date)
  x$year <- lubridate::year(x$date)
  return(x)
}
