#' spline_points
#'
#'Identification of bloom descriptors dates based on smoothing splines
#' 
#' @param value a numerical vector of values (abundances...) 
#' @param time a numerical vector of temporal units (month, week...)
#' @param s_param the smoothing parameter corresponding to the spar parameters
#' of the smooth.spline function (default 0.35).
#'
#' @return a dataframe containing the time vector, the value vector and info
#' a character vector with 'Start', 'Max' and 'End' for the seasonal peak and NA
#' for the other date
#' @examples
#' \dontrun{
#'	  time<-1:12
#'	  value<-c(0,0,1,2,5,7,3,0,0,0,0,0)
#' 	  spline_points(value,time,s_param=0.35)
#' }
#' @export
#' 
spline_points <- function(value,time,s_param=0.35){
#	if(F){
#	  time<-1:12
#	  value<-c(0,0,1,2,5,7,3,0,0,0,0,0)
#	  spline_points(value,time,s_param=0.35)
#  	smt <- stats::smooth.spline(time, value,spar=0.35)
#	  plot(time,value)
#	lines(predict(smt))
#	lines(predict(smt,deriv=1))
#	  plot(time,value)
#	}
  
  smt <- stats::smooth.spline(time, value,spar=s_param)
  smt_pre <- smt$y
  smt_der <- stats::predict(smt,deriv = 1)
  
  smt_df <- data.frame(time,value,smt_pre)
  smt_df$der <- smt_der$y
  maxder<-max(smt_df$der)
  minder<-min(smt_df$der)
  maxderpos<-maxder>0
 
  smt_df$info<-NA_character_
  smt_df$info[(smt_df$der>=maxder)&(maxderpos)]<-"Start"
  smt_df$info[(smt_df$der<=minder)&(maxderpos)]<-"End"
  smt_df$info[(smt_df$value>=max(smt_df$value))&(max(smt_df$value)>0)]<-"Max"
  
  #output=
  #  smt_df %>% mutate(info = case_when(
  #    der>=max(der) & max(der)>0
  #    # & ab_treshold=tres
  #    ~ "Start",
  #    der<=min(der) & max(der)>0 ~ "End",
  #    value>=max(value) & max(value)>0 ~ "Max",
  #    TRUE ~ NA_character_))
  
  return(smt_df)

}
