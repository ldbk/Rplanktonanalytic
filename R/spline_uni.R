#' spline_uni
#'
#' Identification of annual bloom descriptors dates based on smoothing splines.
#' The function automatically exludes those years containing less than  75% of
#' the possible observations for the temporal scale considered.
#'
#' @param x a dataframe having the first 7 columns named as "location", "station", "date", "year", "month", "week", "day",
#' other columns corresponds to the species columns
#' @param ab_treshold quantile of the positive abundance distribution (default 0.75)
#' @param obs_year minimum number of samples for each year in which the species shows an abundance>0 (default 2)
#' @param t_scale temporal scale resolution of the data, 52 for weekly data, 12 for monthly data (default 52)
#' of the smooth.spline function (default 0.35).
#' @param S.NAME name of the species column
#' @inheritParams spline_points
#' @return a dataframe containing the annual time vector, the value vector and info
#' a character vector with 'Start', 'Max' and 'End' for the seasonal peak and NA
#' for the other date
#' @examples
#' \dontrun{
#' data("phytopknar")
#' phytopknar_ret <- ret_time(phytopknar)
#' phytopknar_ret_ord <- phytopknar_ret %>% dplyr::select(
#'   location, station, date,
#'   year, month, week, day, everything()
#' )
#' spline_uni(phytopknar_ret_ord,
#'   ab_treshold = 0.75, obs_year = 2, s_param = 0.35,
#'   t_scale = 52, control = 0, S.NAME = "Cylindrotheca closterium", past = 0
#' )
#' }
#' @export
#'
spline_uni <-
  function(x, ab_treshold = NULL, S.NAME = "", obs_year = NULL, s_param = NULL, t_scale = NULL, control = NULL, past = NULL) {
    ## 1: remove NAs keep robust years
    x <- x %>% dplyr::select(1:7, S.NAME)
    colnames(x)[ncol(x)] <- "value"
    df1 <- na.omit(x)
    ## 2: keep years containing N>=75% of possible observations
    ok_years <-
      df1 %>%
      dplyr::group_by(.data$year) %>%
      dplyr::tally() %>%
      dplyr::filter(.data$n > (t_scale * 0.75)) %>%
      dplyr::pull(.data$year)
    df2 <- df1 %>% dplyr::filter(.data$year %in% ok_years)
    ## 3: consider only years in which species has/have at least X observations > 0
    year_ob <-
      df2 %>%
      dplyr::filter(.data$value > 0) %>%
      dplyr::group_by(.data$year) %>%
      dplyr::tally() %>%
      dplyr::filter(.data$n > obs_year) %>%
      dplyr::pull(.data$year)
    df3 <- df2 %>%
      dplyr::filter(.data$year %in% year_ob) %>%
      droplevels()
    y_levels <- unique(df3$year)

    ## 4: calculate bloom abundance treshold
    tre <- df3 %>%
      dplyr::filter(.data$value > 0) %>%
      dplyr::select(.data$value)
    tre <- quantile(tre$value, ab_treshold)
    #### loop spline_point for each year
    df_emp <- data.frame()
    for (i in y_levels) {
      dft <- df3 %>% dplyr::filter(.data$year == i)

      dfn <- spline_points(dft$value, dft$day, s_param = s_param, control = control, past = past)

      dfn$year <- rep(i, nrow(dfn))
      dfn$species <- rep(S.NAME, nrow(dfn))
      dfn$treshold <- rep(tre, nrow(dfn))
      dfn$date <- dft$date

      dfn1 <- dfn %>% dplyr::select(.data$species, .data$year, .data$date, .data$time, .data$value, .data$treshold, tidyselect::everything())
      df_emp <- rbind(df_emp, dfn1) %>% as.data.frame()
    }

    return(df_emp)
  }
