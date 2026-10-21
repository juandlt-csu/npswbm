#' Psychrometric constant (kPa per degree C) at an elevation, from the
#' atmospheric pressure of atm_press(). Internal helper of
#' [ET_PenmanMonteith_daily()].
#' @param elev Numeric. Elevation, in meters.
#' @return Numeric. Psychrometric constant, in kPa per degree C.
#' @noRd

psyc_constant = function(elev){
  atm.press = atm_press(elev)
  psyc.const = 0.000665*atm.press
  return(psyc.const)
}
