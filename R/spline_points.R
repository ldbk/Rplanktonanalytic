#' Identification of bloom descriptors dates based on smoothing splines
#' 
#' @param x A dataframe containing the temporal information and the abundance of a species.
#' @param s_param The smoothing parameter (default 0.35).
#' @return A new dataframe containing temporal and species abundance info and date of start, maximum abundance and end of the bloom of the species.
#' @examples
#' \dontrun{
#' spline.points(x,s_param=0.35)
#' }
#' @export
#' 
spline_points <- function(x,s_param=0.35){
  
  smt <- stats::smooth.spline(x$day, x$sp,spar=s_param)
  smt_pre <- smt$y
  smt_der <- stats::predict(smt,deriv = 1)
  
  smt_df <- cbind(x,smt_pre)
  smt_df$der <- smt_der$y
  
  output=
    smt_df %>% mutate(info = case_when(
      der>=max(der) & max(der)>0
      # & ab_treshold=tres
      ~ "Start",
      der<=min(der) & max(der)>0 ~ "End",
      sp>=max(sp) & max(sp)>0 ~ "Max",
      TRUE ~ NA_character_))
  
  outoput <- output %>% select(date:sp,info)
  return(output)
}

