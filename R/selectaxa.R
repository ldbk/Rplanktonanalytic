#' selectaxa
#'
#'Calculate the relative frequency of species in a dataframe and return a new dataframe containing species with a relative frequency higher than a user-selected treshold
#' 
#' @param x a dataframe 
#' @param fre_treshold relative frequency treshold
#' @param start number of column of the first species of the dataframe
#' @return a dataframe containing species above the specified treshold 
#' @examples
#' \dontrun{
#'    a <- seq(1,10,1)
#'    sp1 <- c(1,1,1,1,1,1,1,1,1,1)
#'    sp2 <- c(1,1,0,1,0,0,0,1,1,0)
#'    sp3 <- c(1,0,0,1,0,0,0,1,0,0)
#'    sp4 <- c(1,1,0,1,0,1,1,1,0,0)
#'    df <- data.frame(a,sp1,sp2,sp3,sp4)
#'    selectaxa(df)
#' }
#' @export
#' 
selectaxa <- function(x,fre_treshold=50,start=2){
  obs <- nrow(x)
  taxa_list <- 
    x %>% dplyr::select(start:ncol(x)) %>% reshape2::melt() %>% dplyr::filter(value>0) %>% 
    dplyr::group_by(variable) %>% dplyr::tally() %>% dplyr::mutate(fre=(n/obs)*100) %>% 
    dplyr::filter(fre>fre_treshold) %>% dplyr::pull(variable) %>% droplevels()
  
  index_names <- match(taxa_list,names(x))
  x <- cbind(x[1:(start-1)],x[index_names]) %>% as.data.frame()
  return(x)
}

