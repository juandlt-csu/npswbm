#' Clear-sky solar radiation (MJ m-2 day-1) from day of year, latitude and
#' elevation. Internal helper of [ET_PenmanMonteith_daily()].
#' @param doy Numeric vector. Day of year.
#' @param lat Numeric. Latitude, degrees.
#' @param elev Numeric. Elevation, meters.
#' @return Numeric vector. Clear-sky radiation, MJ m-2 day-1.
#' @noRd

clear_sky_rad = function(doy, lat, elev){
  d.r = 1 + 0.033*cos(((2*pi)/365)*doy)
  declin = 0.409*sin((((2*pi)/365)*doy)-1.39)
  lat.rad = (pi/180)*lat
  sunset.ang = acos(-tan(lat.rad)*tan(declin))
  R.a = ((24*60)/pi)*0.0820*d.r*(sunset.ang*sin(lat.rad)*sin(declin) + cos(lat.rad)*cos(declin)*sin(sunset.ang))
  R.so = (0.75 + 2e-5*elev)*R.a
  return(R.so)
}
