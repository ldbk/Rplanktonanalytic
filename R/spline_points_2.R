#' spline_points_2
#'
#'Identification of bloom descriptors dates based on smoothing splines
#' 
#' @param value a numerical vector of values (abundances...) 
#' @param time a numerical vector of temporal units (month, week...)
#' @param s_param the smoothing parameter corresponding to the spar parameters
#' of the smooth.spline function (default 0.35).
#' @param control control the flexibility of the function (default 0).
#'
#'
#' @return a dataframe containing the time vector, the value vector and info
#' a character vector with 'Start', 'Max' and 'End' for the seasonal peak and NA
#' for the other date
#' @examples
#' \dontrun{
#'	  time<-1:12
#'	  value<-c(0,0,1,2,5,7,3,0,0,0,0,0)
#' 	  spline_points_2(value,time,s_param=0.35,control=0)
#' }
#' @export
#' 
spline_points_2 <- 
  function(value,time,s_param=0.35,control=0){
    
    smt <- stats::smooth.spline(time, value,spar=s_param)
    smt_pre <- smt$y
    smt_der <- stats::predict(smt,deriv = 1)
    
    smt_df <- data.frame(time,value,smt_pre)
    smt_df$der <- smt_der$y
    maxder<-max(smt_df$der)
    minder<-min(smt_df$der)
    maxderpos<-maxder>0
    
    tre_der <- maxder-(maxder*control)
    max_der_n <- which.max(smt_df$der)
    max_time <- smt_df[max_der_n,1]
    quant_der <- quantile(subset(smt_df,der>0)$der,0.9)
    der_cond <- subset(smt_df,der>=tre_der & der>=quant_der & time<max_time)
    time_ref <- der_cond[1,1]
    
    smt_df$info<-NA
    if (!is.na(which(smt_df$time==time_ref)[1])) {
      smt_df$info[which(smt_df$time==time_ref)] <- "Start"
    } else {
      smt_df$info[which((smt_df$der>=maxder)&(maxderpos))] <- "Start"
    }
    smt_df$info[(smt_df$der<=minder)&(maxderpos)]<-"End"
    smt_df$info[(smt_df$value>=max(smt_df$value))&(max(smt_df$value)>0)]<-"Max"
    
    return(smt_df)
  }
