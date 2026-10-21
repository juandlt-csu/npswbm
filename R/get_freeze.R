#' @title Freeze factor for splitting precipitation into rain and snow
#'
#' @description
#' Calculates the fraction of precipitation that falls as rain (0-1) from the
#' daily mean temperature and the Jennings et al. (2018) rain-snow threshold
#' temperature for the site:
#' * 0 (all snow) when `tmean <= jtemp - 3`;
#' * 1 (all rain) when `tmean >= jtemp + 3`;
#' * a straight line from 0 to 1 in between.
#'
#' These model functions are based on Dave Thoma's water balance spreadsheet
#' model; they were first written on 2019-10-30 by ARC and updated on
#' 2021-04-14 by MGS.
#'
#' @param jtemp Numeric. The Jennings threshold temperature for the site,
#'   in degrees C (from the Jennings raster, see [load_wbm_rasters()] and
#'   [get_nps_wbm_params()]). One value or one per day. Required.
#' @param tmean Numeric vector. The daily mean temperature, in degrees C.
#'   Required.
#'
#' @return A numeric vector of freeze factors (0-1), one per day, returned
#'   invisibly (assign it or wrap the call in parentheses to print it).
#' @references
#' Jennings, K.S., Winchell, T.S., Livneh, B., and Molotch, N.P. (2018).
#'   Spatial variation of the rain-snow temperature threshold across the
#'   Northern Hemisphere. Nature Communications, 9, 1148.
#' @export
#' @seealso [get_rain()] and [get_snow()], which use it.
#' @examples
#' freeze <- get_freeze(jtemp = 1.5, tmean = c(-5, -1, 1.5, 4, 8))
#' freeze

get_freeze = function(jtemp, tmean){
  freeze=ifelse(tmean<= (jtemp-3),0,
                ifelse(tmean>=(jtemp+3),1,
                       (1/((jtemp+3) - (jtemp-3)))*(tmean-(jtemp-3))))
}
