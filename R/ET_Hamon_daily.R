#' @title Hamon daily potential evapotranspiration
#'
#' @description
#' Calculates daily potential evapotranspiration (PET, mm) with the Hamon
#' method, from mean temperature and day length. One of the PET methods of
#' [nps_wbm()] (`PET_Method = "Hamon"`).
#'
#' These model functions are based on Dave Thoma's water balance spreadsheet
#' model; they were first written on 2019-10-30 by ARC and updated on
#' 2021-04-14 by MGS.
#'
#' @param x Data frame. A daily time series with `tmean_C` (mean air
#'   temperature, degrees C) and `daylength` (hours, e.g. from
#'   [get_daylength()]). Required.
#'
#' @return A numeric vector of daily PET in mm, one value per row of `x`.
#' @export
#' @seealso [get_OudinPET()] and [ET_PenmanMonteith_daily()], the other PET
#'   methods; [nps_wbm()].
#' @examples
#' days <- data.frame(tmean_C = c(-5, 5, 15, 25), daylength = c(9.5, 11, 13, 14.5))
#' ET_Hamon_daily(days)

ET_Hamon_daily = function(x){
  et.hamon = 0.1651*(x$daylength/12)*(216.7*(6.108*exp((17.26*x$tmean_C)/(x$tmean_C+273.3))))/(x$tmean_C+273.3)
  return(et.hamon)
}
