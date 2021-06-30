#' spline_points
#'
#' Identification of bloom descriptors dates based on smoothing splines
#'
#' @param value a numerical vector of values (abundances...)
#' @param time a numerical vector of temporal units (month, week...)
#' @param s_param the smoothing parameter corresponding to the spar parameters
#' of the smooth.spline function (default 0.35).
#' @param control control the flexibility of the function (default 0).
#' @param past number of observations before the the maximum derivative. Default 
#' 0, other values are useful whether the user wants to pick the moment just before 
#' the max derivative (max growth).
#'
#' @return a dataframe containing the time vector, the value vector and info
#' a character vector with 'Start', 'Max' and 'End' for the seasonal peak and NA
#' for the other date
#' @examples
#' \dontrun{
#' time <- 1:12
#' value <- c(0, 0, 1, 2, 5, 7, 3, 0, 0, 0, 0, 0)
#' spline_points(value, time, s_param = 0.35, control = 0, past=0)
#' }
#' @export
#'
spline_points <-
  function(value, time, s_param = NULL, control = NULL, past = NULL) {
    smt <- stats::smooth.spline(time, value, spar = s_param)
    smt_pre <- smt$y
    smt_der <- stats::predict(smt, deriv = 1)

    smt_df <- data.frame(time, value, smt_pre)
    smt_df$der <- smt_der$y
    maxder <- max(smt_df$der)
    minder <- min(smt_df$der)
    maxderpos <- maxder > 0
    
    tre_der <- maxder-(maxder*control)
    max_der_n <- which.max(smt_df$der) - past
    max_time <- smt_df[max_der_n, 1]
    #quant_der <- quantile(subset(smt_df, der > 0)$der, 0.9)
    der_cond <- subset(smt_df, der >= tre_der 
                       #& der >= quant_der 
                       & time < max_time)
    time_ref <- der_cond[1, 1]

    smt_df$info <- NA

    # find start
    if (!is.na(which(smt_df$time == time_ref)[1])) {
      smt_df$info[which(smt_df$time == time_ref)] <- "Start"
    } else {
      smt_df$info[which((smt_df$der >= maxder) & (maxderpos))] <- "Start"
    }

    # find max
    ref_max <- which(smt_df$info == "Start")
    smt_df_max <- smt_df[ref_max:nrow(smt_df), ]
    max_val <- max(smt_df_max$value)

    max_cond <- subset(smt_df, smt_df$value == max_val & max_val > 0)
    max_time_ref <- max_cond[nrow(max_cond), 1]
    smt_df$info[which(smt_df$time == max_time_ref)] <- "Max"

    # find end
    ref_end <- which(smt_df$info == "Max")
    tre_min <- quantile(subset(smt_df, value > 0)$value, 0.33)
    n_obs <- round(nrow(smt_df) * 0.075)

    smt_df_end <- smt_df[ref_end:nrow(smt_df), ]
    minder <- min(smt_df_end$der)
    smt_df_end$test_tremin <- ifelse(smt_df_end$value < tre_min, "T", "F")

    y <- which(smt_df_end$test_tremin == "T")
    y <- y + ref_end - 1
    startIndx <- y[!(y - 1) %in% y]
    stopIndex <- y[!(y + 1) %in% y] + 1
    rows_seq_length <- rbind(startIndx, stopIndex)
    potential_ends <- rows_seq_length[1, ]
    ends_seq <- data.frame(t(rbind(potential_ends, diff(rows_seq_length))))
    end_rows <- subset(ends_seq, stopIndex >= n_obs)

    if (nrow(end_rows) >= 1) {
      end_row <- min(end_rows$potential_ends)
      smt_df$info[end_row] <- "End"
    } else {
      smt_df$info[which(smt_df$der == minder)] <- "End"
    }

    return(smt_df)
  }
