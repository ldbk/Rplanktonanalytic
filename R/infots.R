#' infots 
#'
#' This function computes indices related to the time series
#' 
#' @param value a numerical vector of values (abundances...) 
#' @param time a numerical vector of temporal units (month, week...)
#'
#' @section Details:
#' The seasonal peak timing is defined throughout the entire growing season using
#' the time scale coordinate (number of the month, week, day...) of the centre of
#' gravity of the area below the plot of the corresponding values.
#' 
#' @section Details:
#' For monthly data the seasonal peak timing \eqn{T} is:
#' \deqn{T= \frac{\sum_{i=1}^{12}M.x_m}{\sum_{i=1}^{12}x_m}}
#' with \eqn{x_m} is the abundance in month \eqn{M} (January=1,...Decembre=12).
#'
#' @return a numerical value corresponding to the timing of the seasonal peak in time unit corresponding the time vector 
#' @author Laurent Dubroca
#'
#' @source Edwards, M Richardson A.J. Impact of climate change on marine pelagic
#' phenology and trophic mismatch. Nature, 2004, 430, 881-884.
#' \url{http://dx.doi.org/10.1038/nature02808}. 
#' @examples
#' \dontrun{
#' time<-1:12
#' value<-c(0,0,1,2,5,7,3,0,0,0,0,0)
#' edwards(time,value)
#' 
#' }
#' @export
tsinfo<-function(value,time){
	if(F){
		library(dplyr)
		library(tsfeatures)
 time<-1:12
 value<-c(0,0,1,2,5,7,3,0,0,0,0,0)
 edwards(time,value)
	}
	tsval<-ts(c(value,value),freq=12,start=2018)
	plot(tsval)
	tsfeatures::tsfeatures(tsval)%>%as.data.frame()
	T<-sum(time*value,na.rm=T)/sum(value,na.rm=T)
	return(T)
}
