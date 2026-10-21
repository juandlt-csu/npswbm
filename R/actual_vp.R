#' Actual vapor pressure (kPa) from daily maximum and minimum relative
#' humidity and temperature. Internal helper of [ET_PenmanMonteith_daily()].
#' @param rhmax,rhmin Numeric vectors. Daily maximum and minimum relative humidity, percent.
#' @param tmax,tmin Numeric vectors. Daily maximum and minimum temperature, degrees C.
#' @return Numeric vector. Actual vapor pressure, kPa.
#' @noRd

actual_vp = function(rhmax, rhmin, tmax, tmin) {
  e.tmax = get_svp(tmax)
  e.tmin = get_svp(tmin)
  e.a = (e.tmin*(rhmax/100) + e.tmax*(rhmin/100))/2
  return(e.a)
}
