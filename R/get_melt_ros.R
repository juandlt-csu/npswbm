#' @title Daily snowmelt with rain-on-snow acceleration
#'
#' @description
#' The same degree-day melt as [get_melt()], with a higher melt factor on days
#' when rain falls on melting snow. Snow melts when `tmean` is at least
#' `jtemp - 3` and there is snowpack. The melt is
#' `(tmean - (jtemp - 3)) * hock`, but never more than the snowpack of the day
#' before. On a melting day with rain, `hock` becomes
#' `hock + 0.0125 * rain`. With no rain it returns what [get_melt()] returns.
#'
#' The source describes the logic as based on the USACE and SNOW-17
#' temperature-index wet melt formulations.
#'
#' @param tmean Numeric vector. The daily mean temperature, in degrees C.
#'   Required.
#' @param jtemp Numeric vector. The Jennings threshold temperature, in
#'   degrees C, one value per day (repeat the site value). Required.
#' @param hock Numeric. The dry melt factor, in mm per degree C per day (e.g.
#'   `4`, the [nps_wbm()] default). Required.
#' @param snow Numeric vector. Daily snowfall, in mm (from [get_snow()]).
#'   Required.
#' @param rain Numeric vector. Daily rainfall, in mm (from [get_rain()]).
#'   Required.
#' @param sp.0 Numeric or `NULL`. The snowpack before the first day, in mm.
#'   `NULL` uses 0. Default: `NULL`.
#'
#' @return A numeric vector of daily melt, in mm.
#' @export
#' @seealso [get_melt()], the same melt without the rain term; [nps_wbm()],
#'   which uses this function with `ROS = TRUE`; [get_snowpack()], which uses
#'   the melt.
#' @examples
#' tmean <- c(-6, 2, 4, 5)
#' snow <- c(20, 0, 0, 0)
#' rain <- c(0, 10, 0, 2)
#' get_melt_ros(tmean, jtemp = rep(1.5, 4), hock = 4, snow = snow, rain = rain)
#'
#' # The same days without the rain term melt less on day 2
#' get_melt(tmean, jtemp = rep(1.5, 4), hock = 4, snow = snow)

get_melt_ros = function(tmean, jtemp, hock, snow, rain, sp.0=NULL){
  sp.0 <- ifelse(!is.null(sp.0), sp.0, 0)
  melt <- vector()

  # Calculate melt factor for day 1: adjust hock if it rains and it is warm enough to melt
  is_melting_1 <- tmean[1] >= (jtemp[1] - 3)
  hock_eff_1 <- ifelse(is_melting_1 && rain[1] > 0, hock + (0.0125 * rain[1]), hock)

  melt[1] <- ifelse(!is_melting_1 || sp.0 == 0, 0,
                    ifelse((tmean[1] - (jtemp[1] - 3)) * hock_eff_1 > sp.0,
                           sp.0, (tmean[1] - (jtemp[1] - 3)) * hock_eff_1))

  snowpack <- vector()
  snowpack[1] <- sp.0 + snow[1] - melt[1]

  for(i in 2:length(tmean)){
    is_melting_i <- tmean[i] >= (jtemp[i] - 3)
    # Dynamically scale Hock up when rain falls concurrently with warm melting temps
    hock_eff_i <- ifelse(is_melting_i && rain[i] > 0, hock + (0.0125 * rain[i]), hock)

    melt[i] <- ifelse(!is_melting_i || snowpack[i-1] == 0, 0,
                      ifelse((tmean[i] - (jtemp[i] - 3)) * hock_eff_i > snowpack[i-1],
                             snowpack[i-1], (tmean[i] - (jtemp[i] - 3)) * hock_eff_i))

    snowpack[i] <- snowpack[i-1] + snow[i] - melt[i]
  }
  return(melt)
}
