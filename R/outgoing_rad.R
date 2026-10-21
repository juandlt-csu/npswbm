#' Net outgoing longwave radiation (MJ m-2 day-1) from daily Tmax, Tmin,
#' incoming and clear-sky radiation and actual vapor pressure. Internal helper
#' of [ET_PenmanMonteith_daily()]. Note: the Tmin term is not raised to the
#' 4th power as in FAO-56 equation 39.
#' @param tmax,tmin Numeric vectors. Daily maximum and minimum temperature, degrees C.
#' @param R.s Numeric vector. Incoming solar radiation, MJ m-2 day-1.
#' @param e.a Numeric vector. Actual vapor pressure, kPa.
#' @param R.so Numeric vector. Clear-sky radiation, MJ m-2 day-1.
#' @return Numeric vector. Net outgoing longwave radiation, MJ m-2 day-1.
#' @noRd

outgoing_rad = function(tmax, tmin, R.s, e.a, R.so){
  R.nl = 4.903e-09*(((tmax + 273.16)^4 + (tmin + 273.16))/2)*(0.34-0.14*sqrt(e.a))*(1.35*(R.s/R.so) - 0.35)
  return(R.nl)
}
