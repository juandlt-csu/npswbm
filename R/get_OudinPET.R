#' @title Oudin daily potential evapotranspiration, adjusted for heat load
#'
#' @description
#' Calculates daily potential evapotranspiration (PET, mm) with the Oudin
#' method, from extraterrestrial radiation (from day of year and latitude)
#' and mean temperature, then multiplies it by a heat load factor for the
#' site's slope and aspect (as in Lutz et al. 2010) and by `shade.coeff`.
#' The default PET method of [nps_wbm()] (`PET_Method = "Oudin"`).
#'
#' PET is 0 on days with more than 2 mm of snowpack or a mean temperature of
#' -5 degrees C or less.
#'
#' These model functions are based on Dave Thoma's water balance spreadsheet
#' model; they were first written on 2019-10-30 by ARC and updated on
#' 2021-04-14 by MGS.
#'
#' @param doy Numeric vector. Day of year (1-366). Required.
#' @param lat Numeric. The latitude of the site, in degrees. Required.
#' @param snowpack Numeric vector. The snowpack on each day, in mm (e.g.
#'   from [get_snowpack()]). Required.
#' @param tmean Numeric vector. The daily mean temperature, in degrees C.
#'   Required.
#' @param slope Numeric. The slope of the site, in degrees. Required.
#' @param aspect Numeric. The aspect of the site, in degrees (0-360).
#'   Required.
#' @param shade.coeff Numeric (0-1) or `NULL`. A shading factor
#'   multiplied into PET.
#'   * A number, e.g. `0.8`: PET is reduced to that fraction.
#'   * `NULL`: uses 1 (no shading).
#'
#'   Default: `NULL`.
#'
#' @return A numeric vector of daily PET in mm, the same length as `doy`.
#' @export
#' @seealso [ET_Hamon_daily()] and [ET_PenmanMonteith_daily()], the other
#'   PET methods; [nps_wbm()].
#' @examples
#' get_OudinPET(doy = c(15, 105, 196, 288), lat = 37.2,
#'              snowpack = c(20, 0, 0, 0), tmean = c(-2, 10, 25, 12),
#'              slope = 10, aspect = 180)

get_OudinPET = function(doy, lat, snowpack, tmean, slope, aspect, shade.coeff=NULL){
  d.r = 1 + 0.033*cos((2*pi/365)*doy)
  declin = 0.409*sin((((2*pi)/365)*doy)-1.39)
  lat.rad = (pi/180)*lat
  sunset.ang = acos(-tan(lat.rad)*tan(declin))
  R.a = ((24*60)/pi)*0.082*d.r*((sunset.ang*sin(lat.rad)*sin(declin)) + (cos(lat.rad)*cos(declin)*sin(sunset.ang)))
  Oudin = ifelse(snowpack>2,0,ifelse(tmean>-5,(R.a*(tmean+5)*0.408)/100,0))
  Folded_aspect = abs(180-abs((aspect)-225))
  Heatload = (0.339+0.808*cos(lat*(pi/180))*cos(slope*(pi/180)))-(0.196*sin(lat.rad)*sin(slope*(pi/180)))-(0.482*cos(Folded_aspect*(pi/180))*sin(slope*(pi/180)))
  sc = ifelse(!is.null(shade.coeff), shade.coeff, 1)
  OudinPET = Oudin * Heatload * sc
  return(OudinPET)
}
