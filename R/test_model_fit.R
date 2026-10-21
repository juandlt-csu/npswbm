#' @title Score a model run against observations
#'
#' @description
#' Runs a model (usually a routing function) and scores its output against
#' observations with an objective function such as [NSE()]. Built to be the
#' function an optimizer like [stats::optim()] calls; see the example.
#'
#' Steps: runs `calibration_function` with `calibration_function_args`,
#' takes the `result_col` column of its output, drops days with no
#' observation, optionally averages to months or years, and calls
#' `obj_fun(sim = ..., obs = ...)`. If the output length doesn't match the
#' observations, or has non-finite values, it returns a penalty instead
#' (`-9999` when `obj_fun` is [NSE()], otherwise `1e10`).
#'
#' @param params Numeric vector. The parameters being fitted. Not used by
#'   the function: build `calibration_function_args` from them in a wrapper
#'   (see the example). Required.
#' @param calibration_function Function. The model to run, e.g.
#'   [runoff_routing_gw()]. Required.
#' @param calibration_function_args List. The arguments for
#'   `calibration_function`, passed with [do.call()]. Required.
#' @param result_col String or `NULL`. The output column to score.
#'   * A string, e.g. `"Q_tot"`: that column.
#'   * `NULL`: the last column.
#'
#'   Default: `NULL`.
#' @param obs_vals Numeric vector. The observations, the same length as the
#'   model output (use `NA` for days without one). Required.
#' @param date Date vector or `NULL`. The dates of the observations, the
#'   same length as `obs_vals`; needed for `time_ag`. Stops with an error if
#'   the lengths differ. Default: `NULL`.
#' @param obj_fun Function. The objective function, called as
#'   `obj_fun(sim = , obs = )`: one of [NSE()], [RSR()], [PBIAS()], [COD()],
#'   [SSE()], [RMSE()] or [MAE()]. Required.
#' @param return_dat Logical. What to return.
#'   * `TRUE`: a data frame of the dates, observations and full model output.
#'   * `FALSE`: the score.
#'
#'   Default: `FALSE`.
#' @param silence Logical. Whether to stay quiet.
#'   * `TRUE`: prints nothing.
#'   * `FALSE`: prints "Current Fit: ..." each call (and a message when it
#'     returns a penalty).
#'
#'   Default: `FALSE`.
#' @param time_ag String or `NULL`. The time step to score at.
#'   * `"Monthly"` or `"Annual"`: averages observations and model output by
#'     month or year first (needs `date`).
#'   * `"Daily"` or `NULL`: scores each day.
#'
#'   Default: `NULL`.
#'
#' @return The score (a number), or a data frame when `return_dat = TRUE`.
#' @export
#' @seealso The objective functions, e.g. [NSE()]; [runoff_routing_gw()]
#'   and [runoff_routing_snow_fun()].
#' @examples
#' set.seed(1)
#' runoff <- data.frame(date = seq(as.Date("2021-01-01"), by = "day", length.out = 120),
#'                      runoff_in_wbm = pmax(0, rnorm(120, 0.2, 0.4)))
#' # "Observed" flow: the routed runoff with known parameters, plus noise
#' obs <- runoff_routing_gw(runoff, tau_q = 3, tau_s = 25, vs = 0.5)$Q_tot *
#'   runif(120, 0.9, 1.1)
#'
#' # Score one set of parameters
#' test_model_fit(params = NULL,
#'                calibration_function = runoff_routing_gw,
#'                calibration_function_args = list(df = runoff, tau_q = 2,
#'                                                 tau_s = 20, vs = 0.4),
#'                result_col = "Q_tot", obs_vals = obs, obj_fun = NSE)
#'
#' # Fit tau_q, tau_s and vs with optim(), minimizing 1 - NSE
#' fit_fn <- function(p) {
#'   1 - test_model_fit(params = p,
#'                      calibration_function = runoff_routing_gw,
#'                      calibration_function_args = list(df = runoff, tau_q = p[1],
#'                                                       tau_s = p[2], vs = p[3]),
#'                      result_col = "Q_tot", obs_vals = obs, obj_fun = NSE,
#'                      silence = TRUE)
#' }
#' fit <- optim(c(2, 20, 0.4), fit_fn, method = "L-BFGS-B",
#'              lower = c(0.1, 1, 0), upper = c(20, 200, 1))
#' round(fit$par, 2)

test_model_fit <- function(params, 
                           calibration_function = NULL,
                           calibration_function_args = NULL,
                           result_col = NULL,
                           obs_vals = NULL,
                           date = NULL,
                           obj_fun = NULL, 
                           return_dat = FALSE,
                           silence = FALSE,
                           time_ag = NULL
) {
  
  # 1. Run the routing function
  mod_vals_all <- do.call(calibration_function, args = calibration_function_args) 
  
  # 2. Extract the result column
  if (!is.null(result_col)) {
    mod_vals <- mod_vals_all %>% dplyr::pull(dplyr::all_of(result_col))
  } else {
    mod_vals <- mod_vals_all %>% dplyr::pull(dplyr::last(names(.)))
  }
  
  # 3. CRITICAL: Check for NAs or Length Mismatches
  # If the model fails or produces non-finite numbers, return a penalty value
  if (length(mod_vals) != length(obs_vals) || any(!is.finite(mod_vals))) {
    if (!silence) message("Warning: Model produced non-finite values or length mismatch. Penalizing fit.")
    return(ifelse(identical(obj_fun, NSE), -9999, 1e10)) # NSE uses -9999, error functions use a huge number
  }
  
  if (!is.null(date)) {
    if (length(obs_vals) != length(date)) {
      stop("Error: Date and observations must have the same length.")
    }
  }
  
  # 4. Create calibration dataframe
  if (!is.null(date)) {
    df_cal <- data.frame(date = as.Date(date), obs_vals, mod_vals)
  } else {
    df_cal <- data.frame(obs_vals, mod_vals)
  }
  
  # Filter to periods with observations
  dat_cal <- df_cal %>% tidyr::drop_na(obs_vals)
  
  # 5. Time aggregation (Monthly/Annual)
  if (!is.null(time_ag) && "date" %in% names(dat_cal) && time_ag != "Daily") {
    dat_cal <- dat_cal %>%
      dplyr::mutate(dt = if (time_ag == "Monthly") lubridate::ym(substr(date, 1, 7)) else lubridate::year(date)) %>% 
      dplyr::group_by(dt) %>%
      dplyr::summarize(mod_vals = mean(mod_vals, na.rm = TRUE),
                       obs_vals = mean(obs_vals, na.rm = TRUE), .groups = "drop")
  }
  
  # 6. Objective function evaluation
  obj_val <- obj_fun(sim = dat_cal$mod_vals, obs = dat_cal$obs_vals)
  
  if (!silence) print(paste("Current Fit:", round(obj_val, 4)))
  
  # 7. Return Data or Fit
  if (return_dat) {
    # Prepare full output dataframe
    if (!is.null(date)) {
      if ("date" %in% names(mod_vals_all)) {
        mod_vals_all <- mod_vals_all %>% dplyr::select(-date)
      } 
      df_out <- data.frame(date = as.Date(date), obs_vals, mod_vals_all)
    } else {
      df_out <- data.frame(obs_vals, mod_vals_all)
    }
    return(df_out) 
  } else {
    return(obj_val)
  }
}








# 
# 
# 
# test_model_fit <- function(params, 
#                          calibration_function = NULL,
#                          calibration_function_args = NULL,
#                          result_col = NULL,
#                          obs_vals = NULL,
#                          date = NULL,
#                          obj_fun = NULL, #SSE, MAE
#                          return_dat = FALSE,
#                          silence = FALSE,
#                          time_ag = NULL
# ) {
#   
#   #  mod_vals <- calibration_function(params) %>% pull(last(names(.)))
#   mod_vals_all <- do.call(calibration_function, args = calibration_function_args) 
#   
#   if (!is.null(result_col)) {
#         mod_vals <- mod_vals_all %>% dplyr::pull(result_col)
#   } else {
#         mod_vals <- mod_vals_all %>% dplyr::pull(last(names(.)))
#   }
#   
#   
#   if ( !(length(mod_vals) == length(obs_vals) )) {
#     stop("Error: The observations and modeled values must have the same length.")
#   }
#   
#   if (!is.null(date)) {
#     if (!(length(obs_vals) == length(date))) {
#       stop("Error: Date and modeled values must have the same length.")
#     }
#   }
#   
#   # Join modeled flow with input dataframe
#   if (!is.null(date)) {
#     df_cal <- data.frame(date = as.Date(date), obs_vals, mod_vals)
#   } else {
#     df_cal <- data.frame(obs_vals, mod_vals)
#   }
#   
#   # Select subset of data for calibration - only periods with observations
#   dat_cal <- df_cal %>%
#     drop_na(obs_vals)
#   
#   if (!is.null(time_ag) && "date" %in% names(dat_cal) && time_ag != "Daily") {
#     # Time aggregation
#     dat_cal <- dat_cal %>%
#       dplyr::mutate(dt = if (time_ag == "Monthly") lubridate::ym(substr(date, 1, 7)) else year(date)) %>% 
#       dplyr::group_by(dt) %>%
#       dplyr::summarize(mod_vals = mean(mod_vals, na.rm = TRUE),
#                        obs_vals = mean(obs_vals, na.rm = TRUE), .groups = "drop")
#   }
#   
#   # Objective function evaluation
#   obj_val <- do.call(obj_fun, list(sim = dat_cal$mod_vals, obs = dat_cal$obs_vals))
#   
#   # Join modeled flow with input dataframe
#   if (!is.null(date)) {
#     if ("date" %in% names(mod_vals_all)) {
#       mod_vals_all <- mod_vals_all %>% dplyr::select(-date)
#     } 
#     df_out <- data.frame(date = as.Date(date), obs_vals, mod_vals_all)
#   } else {
#     df_out <- data.frame(obs_vals, mod_vals_all)
#   }
#   
#   
#   if (!silence) {
#     print(obj_val)
#   }
#   
#   if(return_dat){
#     return(df_out) 
#   } else {
#     return(obj_val)
#   }
# } 
