#' @title Penman-Monteith daily reference evapotranspiration
#'
#' @description
#' Calculates daily reference evapotranspiration (mm) with the FAO
#' Penman-Monteith method, from daily maximum and minimum temperature,
#' elevation, latitude and wind speed. One of the PET methods of [nps_wbm()]
#' (`PET_Method = "Penman-Monteith"`).
#'
#' Incoming solar radiation is taken as the clear-sky radiation for the day
#' of year, latitude and elevation; an `srad` column in `x` is not used.
#' Actual vapor pressure comes from `x$vp` if present, otherwise from the
#' saturation vapor pressure at the minimum temperature (relative humidity
#' columns currently cause an error, so leave `RHmax` and `RHmin` out).
#'
#' These model functions are based on Dave Thoma's water balance spreadsheet
#' model; they were first written on 2019-10-30 by ARC and updated on
#' 2021-04-14 by MGS.
#'
#' @param x Data frame. A daily time series with `tmax_C` and `tmin_C`
#'   (degrees C) and `doy` (day of year). Optional: `vp` (actual vapor
#'   pressure, kPa) and `wind` (m/s, used only when `wind = NULL`).
#'   Required.
#' @param elev Numeric. The elevation of the site, in meters. Required.
#' @param lat Numeric. The latitude of the site, in degrees. Required.
#' @param wind Numeric or `NULL`. The daily mean wind speed, in m/s.
#'   * A number: used for every day.
#'   * `NULL`: uses `x$wind`.
#'
#'   Default: `2.2` (about 5 mph).
#'
#' @return A numeric vector of daily reference evapotranspiration in mm, one
#'   value per row of `x`.
#' @export
#' @seealso [ET_Hamon_daily()] and [get_OudinPET()], the other PET methods;
#'   [nps_wbm()].
#' @examples
#' days <- data.frame(doy = c(15, 105, 196, 288),
#'                    tmax_C = c(5, 18, 33, 20),
#'                    tmin_C = c(-8, 2, 16, 4))
#' ET_PenmanMonteith_daily(days, elev = 1300, lat = 37.2)

ET_PenmanMonteith_daily = function(x, elev, lat, wind=2.2){
  #Inputs
  tmax = x$tmax_C
  tmin = x$tmin_C
  tmean = (tmax + tmin)/2
  doy = x$doy
  rh.max = x$RHmax
  rh.min = x$RHmin
  vp = x$vp
  #R.s = x$srad
  R.s = clear_sky_rad(doy, lat, elev)
  u = ifelse(is.null(wind) == TRUE, x$wind, wind)
  psyc.const = psyc_constant(elev)
  vap.curve = vapor_curve(tmean)
  
  #Auxilary calculations for wind terms
  DT = vap.curve/(vap.curve + psyc.const*(1+0.34*u))
  PT = psyc.const/(vap.curve + (psyc.const*(1+0.34*u)))
  TT = (900/(tmean + 273))*u
  
  #Saturation vapor pressure
  e.tmax = get_svp(tmax)
  e.tmin = get_svp(tmin)
  e.s = (e.tmax + e.tmin)/2
  
  #Actual vapor pressure
  if(is.null(vp) == TRUE){
    if(is.null(rh.max) == TRUE){
      e.a = e.tmin
    } else {
      e.a = actual_vp(rh.max, rh.min)
    }
  } else {
    e.a = vp
  }
  
  #Solar angle and radiation calculations
  R.ns = (1 - 0.23)*R.s
  R.so = clear_sky_rad(doy, lat, elev)
  R.nl = outgoing_rad(tmax, tmin, R.s, e.a, R.so)
  R.n = R.ns - R.nl
  R.ng = 0.408*R.n
  
  #ET from radiation
  ET.rad = DT*R.ng
  #ET from wind
  ET.wind = PT*TT*(e.s - e.a)
  #Total ET
  ET.o = ET.rad + ET.wind
  return(ET.o)
}
