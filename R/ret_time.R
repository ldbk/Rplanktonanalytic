#' ret_time
#'
#'Return time vectors from a dataframe containing an as.Date object
#' 
#' @param x a dataframe containing a as.Date object named as "date"
#' @return a dataframe containing the time vectors (year, month...)
#' @examples
#' \dontrun{
#'	  date <- seq(as.Date("2000/1/1"), as.Date("2001/1/1"),length.out=10)
#'    value <- c(seq(1,10,1))
#'    df <- data.frame(date,value)
#'    ret_time(df)
#' }
#' @export
#' 
ret_time <- function(x){
  x$day <- yday(x$date)
  x$week <- week(x$date)
  x$month <- month(x$date)
  x$year <- year(x$date)
  return(x)
}

