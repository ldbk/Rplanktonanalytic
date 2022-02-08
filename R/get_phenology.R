get_pheno <- function(x){
  #organize data 
  start_data <- subset(x,info=="Start")
  max_data <- subset(x,info=="Max")
  end_data <- subset(x,info=="End")
  duration_data <- subset(x,info %in% c("Start","End"))
  duration_data$year <- as.factor(duration_data$year)
  #extract attributes 
  inter_start_var <- sd(start_data$time,na.rm=TRUE)#
  inter_max_var <- sd(max_data$time,na.rm=TRUE)#
  mean_max <- mean(max_data$value,na.rm=TRUE)#
  magnitude <- mean(start_data$der,na.rm=TRUE)#
  mean_abundance <- mean(x$value,na.rm=TRUE)#
  rel_freq <- (nrow(x[x$value>0, ])/nrow(x))*100#
  # extract mean_month
  mean_month_step1 <- as.integer(Mode(start_data$month,na.rm=TRUE))
  mean_month <- ifelse(length(mean_month_step1)>1,mean_month_step1[2],mean_month_step1)#
  #extract duration
  data_dur <- NULL
  for(i in levels(duration_data$year)) {
    durations <- diff(duration_data$time)
    durations <- durations[seq(1, length(durations), by = 2)]
    #extract progression (trend of the duration)
    sldata_duration <- trend::sens.slope(durations, conf.level = 0.99)
    duration_trend <- sldata_duration$estimates#
    pval_duration<- sldata_duration$p.value#
    duration_ts <- rbind(data_dur,durations)
  }
  mean_duration <- mean(duration_ts,na.rm=TRUE)#
  #extract starting bloom trend and significance
  sldata_start <- trend::sens.slope(start_data$time, conf.level = 0.99)
  start_trend <- sldata_start$estimates#
  pval_start<- sldata_start$p.value#
  #extract max bloom trend and significance
  sldata_max <- trend::sens.slope(max_data$time, conf.level = 0.99)
  max_trend <- sldata_max$estimates#
  pval_max<- sldata_max$p.value#
  #create and return output dataframe
  dat <- data.frame(cbind(mean_month,mean_abundance,mean_max,rel_freq,magnitude,
                          inter_start_var,inter_max_var, mean_duration,duration_trend,
                          start_trend,max_trend,pval_duration,pval_start,pval_max))
  return(dat)
}