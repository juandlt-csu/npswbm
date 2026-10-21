# Define some objective functions for runoff routing 

#Several summary statistics are used to calibrate and quantify the routing model 
#performance compared to observed discharge at NWIS gages. Statistics include:
# 1.  Nash-Sutcliffe Efficiency (NSE), 
# 2.  Ratio of the Root Mean Square Error to the Standard Deviation (RSR),
# 3.  Percent Bias (PBIAS) (Moriasi, et al., 2007), 
# 4.  The Coefficient of Determination (R2)(Legates and McCabe, 1999). 
# 5.  Sum of squares (SSE)
# 6.  Root-mean squared error (RMSE)
# 7.  Mean absolute error (MAE)

#These statistics are summarized at:
#  https://www.hec.usace.army.mil/confluence/hmsdocs/hmstrm/calibration/calibration-summary-statistics


#' @title Nash-Sutcliffe efficiency (NSE)
#'
#' @description
#' Calculates the Nash-Sutcliffe efficiency of simulated against observed
#' values: `1 - sum((obs - sim)^2) / sum((obs - mean(obs))^2)`. 1 is a
#' perfect match; above 0.75 is an excellent fit; 0.5 to 0.75 satisfactory to
#' good; 0 to 0.5 marginal; below 0 the mean of the observations would do
#' better (Moriasi et al. 2007).
#'
#' The metrics are summarized at
#' <https://www.hec.usace.army.mil/confluence/hmsdocs/hmstrm/calibration/calibration-summary-statistics>.
#'
#' @param obs Numeric vector. Observed values, e.g. daily streamflow at a
#'   gage. Pairs where `obs` or `sim` is `NA` are dropped first. Required.
#' @param sim Numeric vector. Simulated values, the same length as `obs`.
#'   Required.
#'
#' @return A number from minus infinity to 1. Returns `-9999` (a penalty
#'   for optimizers) when no pairs are left or the observations are all the
#'   same.
#' @references
#' Moriasi, D.N. et al. (2007). Model evaluation guidelines for systematic
#'   quantification of accuracy in watershed simulations. Transactions of the
#'   ASABE, 50(3), 885-900.
#'
#' Legates, D.R., and McCabe, G.J. (1999). Evaluating the use of
#'   "goodness-of-fit" measures in hydrologic and hydroclimatic model
#'   validation. Water Resources Research, 35(1), 233-241.
#' @export
#' @seealso The other fit metrics: [NSE()], [RSR()], [PBIAS()], [COD()],
#'   [SSE()], [RMSE()], [MAE()]; [test_model_fit()], which uses them to score
#'   a routing run.
#' @examples
#' obs <- c(2.1, 3.4, 8.0, 5.2, 3.3, NA, 2.4)
#' sim <- c(2.4, 3.1, 7.2, 5.9, 3.0, 2.8, 2.2)
#' NSE(obs, sim)

NSE <- function(obs, sim) {
  
  # Remove any pairs where either obs or sim is NA
  # This ensures the sums don't return NA
  data_clean <- stats::na.omit(data.frame(obs, sim))
  
  obs_val <- data_clean$obs
  sim_val <- data_clean$sim
  
  # If no data is left after cleaning, return penalty
  if (length(obs_val) == 0) return(-9999)
  
  # Calculate the numerator: Sum of Squared Errors (Residuals)
  numerator <- sum((obs_val - sim_val)^2)
  
  # Calculate the denominator: Sum of Squared Deviations from the Mean
  denominator <- sum((obs_val - mean(obs_val))^2)
  
  # Calculate NSE
  # If denominator is 0 (obs are all the same value), return penalty to avoid Inf
  if (denominator == 0) return(-9999)
  
  nse_val <- 1 - (numerator / denominator)
  
  return(nse_val)
}



# RSR (RMSE-observations standard deviation ratio)
# RSR standardizes the RMSE using the standard deviation of the observations.
#' @title Ratio of RMSE to the standard deviation of the observations (RSR)
#'
#' @description
#' Calculates the RMSE divided by the standard deviation of the
#' observations (both as root sums of squares). 0 to 0.5 is a very good fit;
#' 0.5 to 0.6 good; 0.6 to 0.7 satisfactory; above 0.7 unsatisfactory
#' (Moriasi et al. 2007).
#'
#' The metrics are summarized at
#' <https://www.hec.usace.army.mil/confluence/hmsdocs/hmstrm/calibration/calibration-summary-statistics>.
#'
#' @param obs Numeric vector. Observed values, e.g. daily streamflow at a
#'   gage. Pairs where `obs` or `sim` is `NA` are dropped first. Required.
#' @param sim Numeric vector. Simulated values, the same length as `obs`.
#'   Required.
#'
#' @return A number of 0 or more (lower is better). Returns `1e10` (a
#'   penalty for optimizers) when no pairs are left or the observations are
#'   all the same.
#' @references
#' Moriasi, D.N. et al. (2007). Model evaluation guidelines for systematic
#'   quantification of accuracy in watershed simulations. Transactions of the
#'   ASABE, 50(3), 885-900.
#'
#' Legates, D.R., and McCabe, G.J. (1999). Evaluating the use of
#'   "goodness-of-fit" measures in hydrologic and hydroclimatic model
#'   validation. Water Resources Research, 35(1), 233-241.
#' @export
#' @seealso The other fit metrics: [NSE()], [RSR()], [PBIAS()], [COD()],
#'   [SSE()], [RMSE()], [MAE()]; [test_model_fit()], which uses them to score
#'   a routing run.
#' @examples
#' obs <- c(2.1, 3.4, 8.0, 5.2, 3.3, NA, 2.4)
#' sim <- c(2.4, 3.1, 7.2, 5.9, 3.0, 2.8, 2.2)
#' RSR(obs, sim)

RSR <- function(obs, sim) {
  data_clean <- stats::na.omit(data.frame(obs, sim))
  if (nrow(data_clean) == 0) return(1e10) # Return high penalty for bad fit
  
  numerator <- sqrt(sum((data_clean$obs - data_clean$sim)^2))
  denominator <- sqrt(sum((data_clean$obs - mean(data_clean$obs))^2))
  
  if (denominator == 0) return(1e10)
  
  return(numerator / denominator)
}


# PBIAS (Percent Bias)
# Measures the average tendency of the simulated data to be larger or smaller 
# than their observed counterparts.

#' @title Percent bias (PBIAS)
#'
#' @description
#' Calculates the percent bias: `100 * sum(obs - sim) / sum(obs)`.
#' Positive values mean the model underestimates, negative values that it
#' overestimates. For streamflow, within 10% is very good; 10-15% good;
#' 15-25% satisfactory; beyond 25% unsatisfactory (Moriasi et al. 2007).
#'
#' The metrics are summarized at
#' <https://www.hec.usace.army.mil/confluence/hmsdocs/hmstrm/calibration/calibration-summary-statistics>.
#'
#' @param obs Numeric vector. Observed values, e.g. daily streamflow at a
#'   gage. Pairs where `obs` or `sim` is `NA` are dropped first. Required.
#' @param sim Numeric vector. Simulated values, the same length as `obs`.
#'   Required.
#'
#' @return A percentage (can be negative). Returns `1e10` (a penalty for
#'   optimizers) when no pairs are left or the observations sum to 0.
#' @references
#' Moriasi, D.N. et al. (2007). Model evaluation guidelines for systematic
#'   quantification of accuracy in watershed simulations. Transactions of the
#'   ASABE, 50(3), 885-900.
#'
#' Legates, D.R., and McCabe, G.J. (1999). Evaluating the use of
#'   "goodness-of-fit" measures in hydrologic and hydroclimatic model
#'   validation. Water Resources Research, 35(1), 233-241.
#' @export
#' @seealso The other fit metrics: [NSE()], [RSR()], [PBIAS()], [COD()],
#'   [SSE()], [RMSE()], [MAE()]; [test_model_fit()], which uses them to score
#'   a routing run.
#' @examples
#' obs <- c(2.1, 3.4, 8.0, 5.2, 3.3, NA, 2.4)
#' sim <- c(2.4, 3.1, 7.2, 5.9, 3.0, 2.8, 2.2)
#' PBIAS(obs, sim)

PBIAS <- function(obs, sim) {
  data_clean <- stats::na.omit(data.frame(obs, sim))
  if (nrow(data_clean) == 0) return(1e10)
  
  denom <- sum(data_clean$obs)
  if (denom == 0) return(1e10)
  
  # Note: Removed abs() to allow negative values (indicates overestimation)
  # If your optimization requires absolute PBIAS, wrap this in abs()
  pbias_val <- (sum(data_clean$obs - data_clean$sim) * 100) / denom
  return(pbias_val)
}




# COD (Coefficient of Determination / $R^2$)
# Describes the proportion of the variance in measured data explained by the 
# model.

#' @title Coefficient of determination (R squared)
#'
#' @description
#' Calculates the squared Pearson correlation between simulated and
#' observed values: the share of the variance in the observations the model
#' explains (Legates and McCabe 1999).
#'
#' The metrics are summarized at
#' <https://www.hec.usace.army.mil/confluence/hmsdocs/hmstrm/calibration/calibration-summary-statistics>.
#'
#' @param obs Numeric vector. Observed values, e.g. daily streamflow at a
#'   gage. Pairs where `obs` or `sim` is `NA` are dropped first. Required.
#' @param sim Numeric vector. Simulated values, the same length as `obs`.
#'   Required.
#'
#' @return A number from 0 to 1 (higher is better). Returns `0` when fewer
#'   than 2 pairs are left or either series doesn't vary.
#' @references
#' Moriasi, D.N. et al. (2007). Model evaluation guidelines for systematic
#'   quantification of accuracy in watershed simulations. Transactions of the
#'   ASABE, 50(3), 885-900.
#'
#' Legates, D.R., and McCabe, G.J. (1999). Evaluating the use of
#'   "goodness-of-fit" measures in hydrologic and hydroclimatic model
#'   validation. Water Resources Research, 35(1), 233-241.
#' @export
#' @seealso The other fit metrics: [NSE()], [RSR()], [PBIAS()], [COD()],
#'   [SSE()], [RMSE()], [MAE()]; [test_model_fit()], which uses them to score
#'   a routing run.
#' @examples
#' obs <- c(2.1, 3.4, 8.0, 5.2, 3.3, NA, 2.4)
#' sim <- c(2.4, 3.1, 7.2, 5.9, 3.0, 2.8, 2.2)
#' COD(obs, sim)

COD <- function(obs, sim) {
  data_clean <- stats::na.omit(data.frame(obs, sim))
  if (nrow(data_clean) < 2) return(0) # Need at least 2 points for correlation
  
  obs_val <- data_clean$obs
  sim_val <- data_clean$sim
  
  numerator <- sum((obs_val - mean(obs_val)) * (sim_val - mean(sim_val)))
  denominator <- sqrt(sum((obs_val - mean(obs_val))^2)) * sqrt(sum((sim_val - mean(sim_val))^2))
  
  if (denominator == 0) return(0)
  
  r_sq <- (numerator / denominator)^2
  return(r_sq)
}

# SSE (Sum of Squared Errors)
# A simple measure of the total deviation between the model and observations.

#' @title Sum of squared errors (SSE)
#'
#' @description
#' Calculates the sum of squared differences between simulated and
#' observed values.
#'
#' The metrics are summarized at
#' <https://www.hec.usace.army.mil/confluence/hmsdocs/hmstrm/calibration/calibration-summary-statistics>.
#'
#' @param obs Numeric vector. Observed values, e.g. daily streamflow at a
#'   gage. Pairs where `obs` or `sim` is `NA` are dropped first. Required.
#' @param sim Numeric vector. Simulated values, the same length as `obs`.
#'   Required.
#'
#' @return A number of 0 or more, in squared data units (lower is better).
#'   Returns `1e10` (a penalty for optimizers) when no pairs are left.
#' @references
#' Moriasi, D.N. et al. (2007). Model evaluation guidelines for systematic
#'   quantification of accuracy in watershed simulations. Transactions of the
#'   ASABE, 50(3), 885-900.
#'
#' Legates, D.R., and McCabe, G.J. (1999). Evaluating the use of
#'   "goodness-of-fit" measures in hydrologic and hydroclimatic model
#'   validation. Water Resources Research, 35(1), 233-241.
#' @export
#' @seealso The other fit metrics: [NSE()], [RSR()], [PBIAS()], [COD()],
#'   [SSE()], [RMSE()], [MAE()]; [test_model_fit()], which uses them to score
#'   a routing run.
#' @examples
#' obs <- c(2.1, 3.4, 8.0, 5.2, 3.3, NA, 2.4)
#' sim <- c(2.4, 3.1, 7.2, 5.9, 3.0, 2.8, 2.2)
#' SSE(obs, sim)

SSE <- function(obs, sim) {
  data_clean <- stats::na.omit(data.frame(obs, sim))
  if (nrow(data_clean) == 0) return(1e10)
  
  return(sum((data_clean$sim - data_clean$obs)^2))
}



# RMSE (Root Mean Square Error)
# Commonly used to express the magnitude of error in the same units as the data.

#' @title Root mean square error (RMSE)
#'
#' @description
#' Calculates the root mean square error between simulated and observed
#' values, in the units of the data.
#'
#' The metrics are summarized at
#' <https://www.hec.usace.army.mil/confluence/hmsdocs/hmstrm/calibration/calibration-summary-statistics>.
#'
#' @param obs Numeric vector. Observed values, e.g. daily streamflow at a
#'   gage. Pairs where `obs` or `sim` is `NA` are dropped first. Required.
#' @param sim Numeric vector. Simulated values, the same length as `obs`.
#'   Required.
#'
#' @return A number of 0 or more, in data units (lower is better). Returns
#'   `1e10` (a penalty for optimizers) when no pairs are left.
#' @references
#' Moriasi, D.N. et al. (2007). Model evaluation guidelines for systematic
#'   quantification of accuracy in watershed simulations. Transactions of the
#'   ASABE, 50(3), 885-900.
#'
#' Legates, D.R., and McCabe, G.J. (1999). Evaluating the use of
#'   "goodness-of-fit" measures in hydrologic and hydroclimatic model
#'   validation. Water Resources Research, 35(1), 233-241.
#' @export
#' @seealso The other fit metrics: [NSE()], [RSR()], [PBIAS()], [COD()],
#'   [SSE()], [RMSE()], [MAE()]; [test_model_fit()], which uses them to score
#'   a routing run.
#' @examples
#' obs <- c(2.1, 3.4, 8.0, 5.2, 3.3, NA, 2.4)
#' sim <- c(2.4, 3.1, 7.2, 5.9, 3.0, 2.8, 2.2)
#' RMSE(obs, sim)

RMSE <- function(obs, sim) {
  data_clean <- stats::na.omit(data.frame(obs, sim))
  if (nrow(data_clean) == 0) return(1e10)
  
  return(sqrt(mean((data_clean$sim - data_clean$obs)^2)))
}


# MAE (Mean Absolute Error)
# Similar to RMSE but less sensitive to extreme outliers/outlier peaks.

#' @title Mean absolute error (MAE)
#'
#' @description
#' Calculates the mean absolute difference between simulated and observed
#' values. Like RMSE, but less affected by a few large errors such as
#' missed peaks.
#'
#' The metrics are summarized at
#' <https://www.hec.usace.army.mil/confluence/hmsdocs/hmstrm/calibration/calibration-summary-statistics>.
#'
#' @param obs Numeric vector. Observed values, e.g. daily streamflow at a
#'   gage. Pairs where `obs` or `sim` is `NA` are dropped first. Required.
#' @param sim Numeric vector. Simulated values, the same length as `obs`.
#'   Required.
#'
#' @return A number of 0 or more, in data units (lower is better). Returns
#'   `1e10` (a penalty for optimizers) when no pairs are left.
#' @references
#' Moriasi, D.N. et al. (2007). Model evaluation guidelines for systematic
#'   quantification of accuracy in watershed simulations. Transactions of the
#'   ASABE, 50(3), 885-900.
#'
#' Legates, D.R., and McCabe, G.J. (1999). Evaluating the use of
#'   "goodness-of-fit" measures in hydrologic and hydroclimatic model
#'   validation. Water Resources Research, 35(1), 233-241.
#' @export
#' @seealso The other fit metrics: [NSE()], [RSR()], [PBIAS()], [COD()],
#'   [SSE()], [RMSE()], [MAE()]; [test_model_fit()], which uses them to score
#'   a routing run.
#' @examples
#' obs <- c(2.1, 3.4, 8.0, 5.2, 3.3, NA, 2.4)
#' sim <- c(2.4, 3.1, 7.2, 5.9, 3.0, 2.8, 2.2)
#' MAE(obs, sim)

MAE <- function(obs, sim) {
  data_clean <- stats::na.omit(data.frame(obs, sim))
  if (nrow(data_clean) == 0) return(1e10)
  
  return(mean(abs(data_clean$sim - data_clean$obs)))
}
