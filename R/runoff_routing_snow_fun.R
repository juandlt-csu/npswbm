#' @title Route runoff with separate rain and snowmelt components
#'
#' @description
#' Routes a daily runoff series into streamflow with separate rain and
#' snowmelt paths, an IHACRES-style filter based on `expuh` in the hydromad
#' package (Andrews et al. 2021). Because the model's rain and melt totals
#' don't include AET losses, they are only used to split the runoff: each
#' day's runoff is divided between melt and rain by `melt / (rain + melt)`.
#' Then:
#' * rain runoff is split into slowflow (`vs_rain`, time constant
#'   `tau_s_rain`) and quickflow (time constant `tau_q_rain`);
#' * melt runoff is one slowflow (time constant `tau_s_melt`).
#'
#' The melt slowflow currently uses the rain slowflow constant
#' (`tau_s_rain`) for its share of the inflow, and `tau_s_melt` only for its
#' recession.
#'
#' Up to 5 missing days are filled with the nearest value and dropped again
#' from the result; with more than 5 missing days it returns a message
#' (a string) instead.
#'
#' @param df Data frame. The daily series, with a `date` column (always
#'   read as `df$date`, whatever `date_col` says) and runoff, rain and melt
#'   columns. Required.
#' @param tau_q_rain Numeric of 0 or more. The rain quickflow time constant,
#'   in days. Required.
#' @param tau_s_rain Numeric of 0 or more. The rain slowflow time constant,
#'   in days. Required.
#' @param vs_rain Numeric from 0 to 1. The fraction of rain runoff that goes
#'   to slowflow. Required.
#' @param tau_s_melt Numeric of 0 or more. The snowmelt slowflow time
#'   constant, in days. Required.
#' @param delay Numeric. The number of days (rounded) by which all components
#'   are shifted later, filling the start with 0; `0` for none. Required.
#' @param return_components Logical. Whether to return the components.
#'   * `TRUE`: adds `Q_slow_rain`, `Q_quick_rain` and `Q_slow_melt`.
#'   * `FALSE`: returns only `Q_tot`.
#'
#'   Default: `FALSE`.
#' @param runoff_col String. The name of the runoff column. Default:
#'   `"runoff_in_wbm"`.
#' @param rain_col String. The name of the rain column. Default:
#'   `"rain_in_wbm"`.
#' @param melt_col String. The name of the melt column. Default:
#'   `"melt_in_wbm"`.
#' @param date_col String. The name of the date column. Not used yet; the
#'   column must be called `date`. Default: `"date"`.
#'
#' @return A data frame with `date`, `melt_runoff`, `rain_runoff` and
#'   `Q_tot` (in runoff units), plus `Q_slow_rain`, `Q_quick_rain` and
#'   `Q_slow_melt` when `return_components = TRUE`. Or the string "More than
#'   5 days of missing data. Fill gaps before routing..." when too many days
#'   are missing.
#' @references
#' Andrews, F., Guillaume, J., Vervoort, W., and Buzacott, A. (2021).
#'   hydromad: Hydrological Model Assessment and Development.
#'   <https://github.com/josephguillaume/hydromad>
#' @export
#' @seealso [runoff_routing_gw()] for one runoff path with groundwater loss;
#'   [test_model_fit()].
#' @examples
#' wb <- data.frame(date = seq(as.Date("2021-04-01"), by = "day", length.out = 8),
#'                  runoff_in_wbm = c(0, 0.5, 1.0, 0.8, 0.3, 0, 0, 0.2),
#'                  rain_in_wbm = c(0, 0.2, 0, 0, 0, 0, 0, 0.4),
#'                  melt_in_wbm = c(0, 0.4, 1.1, 0.9, 0.4, 0.1, 0, 0))
#' runoff_routing_snow_fun(wb, tau_q_rain = 2, tau_s_rain = 15, vs_rain = 0.3,
#'                         tau_s_melt = 30, delay = 0, return_components = TRUE)

runoff_routing_snow_fun <- function(df, 
                               tau_q_rain, 
                               tau_s_rain, 
                               vs_rain,
                               tau_s_melt,
                               delay,
                               return_components = FALSE,
                               runoff_col = "runoff_in_wbm",
                               rain_col = "rain_in_wbm",
                               melt_col = "melt_in_wbm",
                               date_col = "date") {

  
  # Check for missing dates
  full_dates <- seq(min(df$date), max(df$date), by = "day")
  missing_dates <- full_dates[!full_dates %in% df$date]
  
  # Fill missing dates using linear interpolation IF:
  #  <= 5 days of missing data
  if (length(missing_dates) > 0 & length(missing_dates) <= 5) {
    
    df <- data.frame(date = full_dates) %>%
      dplyr::left_join(., df, by = "date") %>%
      dplyr::arrange(date) %>%
      tidyr::fill(!!dplyr::sym(runoff_col), .direction = "downup") %>%
      tidyr::fill(!!dplyr::sym(melt_col), .direction = "downup") %>%
      tidyr::fill(!!dplyr::sym(rain_col), .direction = "downup")
    
    missing_idx <- match(missing_dates,df$date)
    
  # If > 5 days of missing data, prompt user to fill gaps first.
  } else if (length(missing_dates) > 5) {
    return("More than 5 days of missing data. Fill gaps before routing...")
  }
  
  # Calculate the fraction of runoff that is from snowmelt
  df <- df %>%
    dplyr::mutate(melt_frac = 
                    ifelse(!!dplyr::sym(melt_col) + !!dplyr::sym(rain_col) > 0, 
                           !!dplyr::sym(melt_col) / (!!dplyr::sym(melt_col) + !!dplyr::sym(rain_col)),
                           0),
                  melt_runoff = melt_frac * !!dplyr::sym(runoff_col),
                  rain_runoff = (1- melt_frac) * !!dplyr::sym(runoff_col))
  
  # Ensure all recession coefficients are non-negative:
  stopifnot(all(c(tau_s_rain, tau_q_rain, tau_s_melt) >= 0))
  
  # Compute partitioning for rain quick flow and slow flow:
  vq_rain <- 1-vs_rain 
  
  # Calculate alpha coefficients for exponential "decay" of each WBM component
  # controls the rate of decay
  alpha_s_rain <- exp(-1/tau_s_rain)
  alpha_q_rain <- exp(-1/tau_q_rain)
  alpha_s_melt <- exp(-1/tau_s_melt)
  
  # Calculate beta coefficients based on input parameters
  # proportion of water contributing to a specific flow component 
  # Higher beta means quicker contribution to system
  beta_s_rain <- vs_rain*(1-alpha_s_rain)
  beta_q_rain <- vq_rain*(1-alpha_q_rain)
  beta_s_melt <- (1-alpha_s_rain)
  
  # Round delay to the nearest integer (i.e., day)
  delay <- round(delay,0)
  
  # Initialize flow components at time = 0
  Xsr_0 <- 0 # Initial slow flow for rain
  Xqr_0 <- 0 # quickflow
  Xsm_0 <- 0 # snowmelt

  # place to store results for each flow component
  Xsr <- Xqr <- Xsm <- vector(mode = "numeric", length = nrow(df))
  
  # Slow flow rain. sr = slow recession 
  Xsr[] <- stats::filter(beta_s_rain * df$rain_runoff, 
                        alpha_s_rain, 
                        method = "recursive", 
                        init = Xsr_0) 
  
  # Quick flow rain qr = quick recession
  Xqr[] <- stats::filter(beta_q_rain * df$rain_runoff, 
                          alpha_q_rain, 
                          method = "recursive", 
                          init = Xqr_0)
  
  # Slow flow melt sm = snow melt, similar to slow flow rain?
  Xsm[] <- stats::filter(beta_s_melt * df$melt_runoff, 
                         alpha_s_melt, 
                         method = "recursive", 
                         init = Xsm_0) 
  
  # Apply delay to flow components if specified
  if (delay != 0) {
    Xsr <- dplyr::lag(Xsr, delay)  # Delay slowflow rain
    Xsr[is.na(Xsr)] <- 0 # Fill missing values with 0
    Xqr <- dplyr::lag(Xqr, delay)
    Xqr[is.na(Xqr)] <- 0
    Xsm <- dplyr::lag(Xsm, delay)
    Xsm[is.na(Xsm)] <- 0
  }
  
  df_ret <- df %>%
    dplyr::select(date, melt_runoff, rain_runoff)
  
  # Remove rows of interpolated dates if applicable
  if (length(missing_dates) >0 & length(missing_dates) <= 5) {
    Xqr <- Xqr[-missing_idx]
    Xsr <- Xsr[-missing_idx]
    Xsm <- Xsm[-missing_idx]
    df_ret <- df_ret[-missing_idx,]
  }

  # sum all components for each day
  Q_tot <- Xsr + Xqr + Xsm
  
  if (return_components == FALSE) {
    return(df_ret %>%
           dplyr::bind_cols(Q_tot = Q_tot)
         )
  } else if (return_components == TRUE) {
      return(df_ret %>%
             dplyr::bind_cols(Q_slow_rain = Xsr,
                       Q_quick_rain = Xqr,
                       Q_slow_melt = Xsm,
                       Q_tot = Q_tot)
           )
  }
    
}

