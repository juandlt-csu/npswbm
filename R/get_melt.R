#' @title Daily snowmelt (degree-day method)
#'
#' @description
#' Calculates daily snowmelt from mean temperature with a degree-day (Hock)
#' melt factor, tracking the snowpack as it goes. Snow melts when
#' `tmean` is at least `jtemp - 3` and there is snowpack; the melt is
#' `(tmean - (jtemp - 3)) * hock`, but never more than the snowpack of the
#' day before.
#'
#' These model functions are based on Dave Thoma's water balance spreadsheet
#' model; they were first written on 2019-10-30 by ARC and updated on
#' 2021-04-14 by MGS.
#'
#' @param tmean Numeric vector. The daily mean temperature, in degrees C.
#'   Required.
#' @param jtemp Numeric vector. The Jennings threshold temperature, in
#'   degrees C, one value per day (repeat the site value). Required.
#' @param hock Numeric. The melt factor, in mm per degree C per day (e.g.
#'   `4`, the [nps_wbm()] default). Required.
#' @param snow Numeric vector. Daily snowfall, in mm (from [get_snow()]).
#'   Required.
#' @param sp.0 Numeric or `NULL`. The snowpack before the first day, in mm.
#'   `NULL` uses 0. Default: `NULL`.
#'
#' @return A numeric vector of daily melt, in mm.
#' @references
#' Hock, R. (2003). Temperature index melt modelling in mountain areas.
#'   Journal of Hydrology, 282(1-4), 104-115.
#'   <https://doi.org/10.1016/S0022-1694(03)00257-9>
#' @export
#' @seealso [get_snowpack()], which uses the melt; [get_snow()].
#' @examples
#' tmean <- c(-6, -4, -1, 2, 5, 8)
#' snow <- c(20, 15, 5, 0, 0, 0)
#' get_melt(tmean, jtemp = rep(1.5, 6), hock = 4, snow = snow)

get_melt = function(tmean, jtemp, hock, snow, sp.0=NULL){
  sp.0 <- ifelse(!is.null(sp.0), sp.0, 0)
  melt <- vector()
  melt[1] <- ifelse(tmean[1] < (jtemp[1]-3)||sp.0==0, 0,
                   ifelse((tmean[1]-(jtemp[1]-3))*hock>sp.0, 
                          sp.0, (tmean[1]-(jtemp[1]-3))*hock))
  snowpack <- vector()
  snowpack[1] <- sp.0 + snow[1] - melt[1]
  for(i in 2:length(tmean)){
    melt[i] <- ifelse(tmean[i]<(jtemp[i]-3) | snowpack[i-1]==0, 0, 
                     ifelse((tmean[i]-(jtemp[i]-3))*hock>snowpack[i-1], 
                            snowpack[i-1], (tmean[i]-(jtemp[i]-3))*hock))
    snowpack[i] <- snowpack[i-1]+snow[i]-melt[i]
  }
  return(melt)
}
