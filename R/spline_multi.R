#' spline_multi
#'
#'Identification of annual bloom descriptors dates based on smoothing splines for all the species of the dataframe. 
#'The function automatically exludes those years containing less than  75% of
#'the possible observations for the temporal scale considered.
#' 
#' @param x a dataframe having the first 7 columns named as "location", "station", "date", "year", "month", "week", "day",
#' other columns corresponds to the species columns
#' @param ab_treshold quantile of the positive abundance distribution (default 0.75)
#' @param obs_year minimum number of samples for each year in which the species shows an abundance>0 (default 2)
#' @param s_param the smoothing parameter corresponding to the spar parameters
#' of the smooth.spline function (default 0.35).
#' @param t_scale temporal scale resolution of the data, 52 for weekly data, 12 for monthly data (default 52)
#' 
#' @return a dataframe containing the species names, the annual time vector, the value vector and info
#' a character vector with 'Start', 'Max' and 'End' for the seasonal peak and NA
#' for the other date
#' @examples
#' \dontrun{
#'	  data("phytopknar")
#'    phytopknar_ret <- ret_time(phytopknar)
#'    phytopknar_ret_ord <- phytopknar_ret %>% dplyr::select(location,station,date,year,month,week,day,everything())
#'    spline_multi(phytopknar_ret_ord,ab_treshold=0.75,obs_year=2,s_param=0.35,t_scale=52)
#' }
#' @export
#' 
spline_multi<- function(x,ab_treshold=0.75,obs_year=2,s_param=0.35,t_scale=52){
  df_emp2 <- data.frame()
  for ( j in 8:ncol(x) ) {
    S.NAME <- names(x[j])
    df1.1 <- x %>% select(1:7,j)
    
    df2.1 <- spline_uni(df1.1,ab_treshold=ab_treshold,obs_year=obs_year,s_param=s_param,t_scale=t_scale,S.NAME=S.NAME)
    df_emp2 <- rbind(df_emp2,df2.1) %>% as.data.frame()
  }
  return(df_emp2)
}