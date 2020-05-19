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
  function(value,time,s_param=0.4,control=0.35){
    
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
    #find start
    if (!is.na(which(smt_df$time==time_ref)[1])) {
      smt_df$info[which(smt_df$time==time_ref)] <- "Start"
    } else {
      smt_df$info[which((smt_df$der>=maxder)&(maxderpos))] <- "Start"
    }
    #find max
    max_val <- max(smt_df$value)
    max_cond <- subset(smt_df,smt_df$value==max_val & max_val>0 )
    max_time_ref <- max_cond[nrow(max_cond),1]
    
    if (length(max_cond$time)>1) {
      smt_df$info[which(smt_df$time==max_time_ref)] <- "Max"
    } else {
      smt_df$info[(smt_df$value>=max(smt_df$value))&(max(smt_df$value)>0)]<-"Max"
    }
    
    #find end
    ref_end <- which(smt_df$info=="Max")
    smt_df_end <- smt_df[ref_end:nrow(smt_df), ]
    minder<-min(smt_df_end$der)
    
    min_der_row <- which(smt_df_end$der==minder)+(ref_end)-1
    smt_df$info[min_der_row]<-"End"
    
    return(smt_df)
  }
