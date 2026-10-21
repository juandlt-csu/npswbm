#' @title Route runoff with quick, slow and groundwater components
#'
#' @description
#' Routes a daily series of runoff (e.g. `runoff_in_wbm` from
#' [run_NPS_WBM_points()]) into streamflow with two parallel linear
#' reservoirs, an IHACRES-style filter based on `expuh` in the hydromad
#' package (Andrews et al. 2021):
#' * slowflow gets `vs` of the runoff and recedes with time constant `tau_s`;
#' * quickflow gets the rest and recedes with `tau_q`.
#'
#' A fraction `loss` can be taken out as groundwater recharge, and a constant
#' `baseflow` (e.g. spring flow not tied to runoff) can be added. Streamflow
#' (`Q_tot`) is slowflow plus quickflow plus `baseflow`, in the units of the
#' runoff.
#'
#' @param df Data frame. The daily series, with a date column and a runoff
#'   column. Converted to a data.table and sorted by date. Required.
#' @param tau_q Numeric of 0 or more. The quickflow time constant, in time
#'   steps (days for daily data). Required.
#' @param tau_s Numeric of 0 or more. The slowflow time constant, in time
#'   steps. Required.
#' @param vs Numeric from 0 to 1. The fraction of runoff that goes to
#'   slowflow; `1 - vs` goes to quickflow. Required.
#' @param delay Numeric. The number of time steps (rounded) by which all
#'   components are shifted later, filling the start with 0. Default: `0`.
#' @param loss Numeric from 0 to 1. The fraction taken out as groundwater
#'   recharge (see `slow_only`). Default: `0`.
#' @param baseflow Numeric. A constant flow added every time step, in runoff
#'   units. Default: `0`.
#' @param runoff_col String. The name of the runoff column. Default:
#'   `"runoff_in_wbm"`.
#' @param date_col String. The name of the date column. Default: `"date"`.
#' @param return_components Logical. Whether to return the components.
#'   * `TRUE`: adds `Q_slow`, `Q_quick` and `Q_gw_recharge`.
#'   * `FALSE`: returns only `Q_tot`.
#'
#'   Default: `FALSE`.
#' @param slow_only Logical. Where the groundwater loss comes from.
#'   * `TRUE`: `loss` of the slowflow only, at most `max_inf` per time step.
#'   * `FALSE`: `loss` of both slowflow and quickflow, with no cap.
#'
#'   Default: `FALSE`.
#' @param max_inf Numeric. The most recharge per time step when
#'   `slow_only = TRUE`; ignored otherwise. Default: `50`.
#'
#' @return A data frame with `date` and `Q_tot`, plus `Q_slow`, `Q_quick`
#'   and `Q_gw_recharge` when `return_components = TRUE`.
#' @references
#' Andrews, F., Guillaume, J., Vervoort, W., and Buzacott, A. (2021).
#'   hydromad: Hydrological Model Assessment and Development.
#'   <https://github.com/josephguillaume/hydromad>
#' @export
#' @seealso [runoff_routing_snow_fun()] to route rain and snowmelt
#'   separately; [test_model_fit()] to score a routing run against a gage.
#' @examples
#' runoff <- data.frame(date = seq(as.Date("2021-04-01"), by = "day", length.out = 10),
#'                      runoff_in_wbm = c(0, 0, 1.2, 0.4, 0, 0, 0, 0.8, 0, 0))
#' runoff_routing_gw(runoff, tau_q = 2, tau_s = 20, vs = 0.4)
#'
#' # With 10% groundwater loss, a little baseflow, and the components
#' runoff_routing_gw(runoff, tau_q = 2, tau_s = 20, vs = 0.4, loss = 0.1,
#'                   baseflow = 0.05, return_components = TRUE)

runoff_routing_gw <- function(df, tau_q, tau_s, vs, delay = 0, loss = 0, 
                              baseflow = 0, 
                              runoff_col = "runoff_in_wbm",
                              date_col = "date",
                              return_components = FALSE,
                              slow_only = FALSE,
                              max_inf = 50) {
  
  # Ensure input is data.table
  if (!inherits(df, "data.table")) {
    df <- data.table::as.data.table(df)
  }
  
  data.table::setkeyv(df, date_col)
  stopifnot(all(c(tau_s, tau_q) >= 0))
  
  # Extract runoff data
  runoff <- df[[runoff_col]]
  
  # Calculate Parameters
  vq <- 1 - vs
  alpha_s <- exp(-1 / tau_s)
  alpha_q <- exp(-1 / tau_q)
  beta_s <- vs * (1 - alpha_s)
  beta_q <- vq * (1 - alpha_q)
  
  delay <- round(delay, 0)
  
  # Initialize storage vectors
  Xs <- Xq <- vector(mode = "numeric", length = nrow(df))
  
  # 1. Generate Flow Components
  Xs[] <- stats::filter(beta_s * runoff, alpha_s, method = "recursive", init = 0) 
  Xq[] <- stats::filter(beta_q * runoff, alpha_q, method = "recursive", init = 0)
  
  # 2. Partition slowflow/quickflow to groundwater (Loss)
  if (slow_only) {
    Xg <- loss * Xs
    Xg[Xg > max_inf] <- max_inf
    Xs <- Xs - Xg
  } else {
    Xg <- loss * (Xq + Xs)
    Xq <- (1 - loss) * Xq
    Xs <- (1 - loss) * Xs
  }
  
  # 3. Apply the Delay (The Fix)
  # data.table::shift physically moves the values. 
  # fill = 0 ensures we don't get NAs at the start of the record.
  if (delay > 0) {
    Xs <- data.table::shift(Xs, n = delay, fill = 0, type = "lag")
    Xq <- data.table::shift(Xq, n = delay, fill = 0, type = "lag")
    Xg <- data.table::shift(Xg, n = delay, fill = 0, type = "lag")
  }
  
  # Total Flow
  Q_tot <- Xs + Xq + baseflow
  
  # Create output
  df_ret <- data.frame(date = df[[date_col]], Q_tot = Q_tot)
  
  if (return_components) {
    df_ret$Q_slow <- Xs
    df_ret$Q_quick <- Xq
    df_ret$Q_gw_recharge <- Xg
  }
  
  return(df_ret)
}
  
