#' Saturation vapor pressure (kPa) at a temperature (FAO-56 equation 11,
#' http://www.climasouth.eu/sites/default/files/FAO%2056.pdf). Internal helper
#' of [ET_PenmanMonteith_daily()].
#' @param temp Numeric vector. Temperature, in degrees C.
#' @return Numeric vector. Saturation vapor pressure, in kPa.
#' @noRd

get_svp = function(temp){
  svp = 0.6108*exp((17.27*temp)/(temp + 237.3))
  return(svp)
}
