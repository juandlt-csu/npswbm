#' Slope of the saturation vapor pressure curve (kPa per degree C) at a
#' temperature. Internal helper of [ET_PenmanMonteith_daily()].
#' @param temp Numeric vector. Temperature, in degrees C.
#' @return Numeric vector. Slope, in kPa per degree C.
#' @noRd

vapor_curve = function(temp){
  vap.curve = 4098*(0.6108*exp((17.27*temp/(temp+237.3)))/(temp+237.3)^2)
  return(vap.curve)
}
