#' spline_uni_2
#'
#'Identification of annual bloom descriptors dates based on smoothing splines. 
#'The function automatically exludes those years containing less than  75% of
#'the possible observations for the temporal scale considered.
#' 
#' @param x a dataframe having the first 7 columns named as "location", "station", "date", "year", "month", "week", "day",
#' other columns corresponds to the species columns
#' @param ab_treshold quantile of the positive abundance distribution (default 0.75)
#' @param obs_year minimum number of samples for each year in which the species shows an abundance>0 (default 2)
#' @param s_param the smoothing parameter corresponding to the spar parameters
#' @param t_scale temporal scale resolution of the data, 52 for weekly data, 12 for monthly data (default 52)
#' of the smooth.spline function (default 0.35).
#' @param control control the flexibility of the function (default 0)
#' @param S.NAME name of the species column
#' 
#' @return a dataframe containing the annual time vector, the value vector and info
#' a character vector with 'Start', 'Max' and 'End' for the seasonal peak and NA
#' for the other date
#' @examples
#' \dontrun{
#'	  data("phytopknar")
#'    phytopknar_ret <- ret_time(phytopknar)
#'    phytopknar_ret_ord <- phytopknar_ret %>% dplyr::select(location,station,date,year,month,week,day,everything())
#'    spline_uni_2(phytopknar_ret_ord,ab_treshold=0.75,obs_year=2,s_param=0.35,t_scale=52,control=0,S.NAME="Cylindrotheca closterium")
#' }
#' @export
#' 
spline_uni_2 <- 
  function(x,ab_treshold=0.5,obs_year=2,s_param=0.35,t_scale=52,S.NAME="",control=0){
    ## 1: remove NAs keep robust years
    x <- x %>% select(1:7,S.NAME)
    colnames(x)[ncol(x)]="value"
    df1 <- na.omit(x)
    ## 2: keep years containing N>=75% of possible observations
    ok_years <- 
      df1  %>% group_by(year) %>% tally() %>% filter(n>(t_scale*0.75)) %>% pull(year) 
    df2 <- df1 %>% filter(year %in% ok_years)
    ## 3: consider only years in which species has/have at least X observations > 0 
    year_ob <- 
      df2  %>% filter(value>0) %>% group_by(year) %>%
      tally() %>% filter(n>obs_year) %>% pull(year) 
    df3 <- df2 %>% filter(year %in% year_ob) %>% droplevels()
    y_levels <- unique(df3$year) 
    
    ## 4: calculate bloom abundance treshold 
    tre <- df3 %>% filter(value>0) %>% select(value) 
    tre <- quantile(tre$value,ab_treshold)
    #### loop spline_point for each year
    df_emp <- data.frame()
    for ( i in y_levels ) {
      
      dft <- df3 %>% filter(year==i)
      
      dfn <- spline_points_2(dft$value,dft$day,s_param=s_param,control=control)
      
      dfn$year=rep(i,nrow(dfn))
      dfn$species=rep(S.NAME,nrow(dfn))
      dfn$treshold=rep(tre,nrow(dfn))
      
      dfn1 <- dfn %>% select(species,year,time,value,treshold,everything())
      df_emp <- rbind(df_emp,dfn1) %>% as.data.frame()
  }
  return(df_emp)
}
