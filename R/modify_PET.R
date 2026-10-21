#' Adjusts PET for heat load (Lutz et al. 2010) from slope, aspect and
#' latitude, sets it to 0 on days with a freeze factor of 0, and multiplies
#' by a shade coefficient. Not used by [nps_wbm()] at the moment.
#' @param pet Numeric vector. Daily PET.
#' @param slope,aspect Numeric. Slope and aspect of the site, degrees.
#' @param lat Numeric. Latitude, degrees.
#' @param freeze Numeric vector. Freeze factor (0-1) from [get_freeze()].
#' @param shade.coeff Numeric (0-1) or NULL. Shade coefficient; NULL uses 1.
#' @return Numeric vector. Adjusted PET.
#' @noRd

modify_PET = function(pet, slope, aspect, lat, freeze, shade.coeff=NULL){
  f.aspect = abs(180 - abs(aspect - 225))
  lat.rad = ifelse(lat > 66.7, (66.7/180)*pi, (lat/180)*pi)
  slope.rad = (slope/180)*pi
  aspect.rad = (f.aspect/180)*pi
  heat.load = 0.339+0.808*cos(lat.rad)*cos(slope.rad) - 0.196*sin(lat.rad)*sin(slope.rad) - 0.482*cos(aspect.rad)*sin(slope.rad)
  sc = ifelse(!is.null(shade.coeff), shade.coeff, 1)
  freeze = ifelse(freeze == 0,0,1)
  PET.Lutz = pet*heat.load*sc*freeze
  return(PET.Lutz)
}
