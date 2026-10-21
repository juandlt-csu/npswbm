#' @title Snowfall from precipitation and the freeze factor
#'
#' @description
#' Snow is the part of precipitation that isn't rain:
#' `(1 - freeze) * ppt`.
#'
#' These model functions are based on Dave Thoma's water balance spreadsheet
#' model; they were first written on 2019-10-30 by ARC and updated on
#' 2021-04-14 by MGS.
#'
#' @param ppt Numeric vector. Daily precipitation, in mm. Required.
#' @param freeze Numeric vector. The freeze factor (0-1) from
#'   [get_freeze()]: the fraction that falls as rain. Required.
#'
#' @return A numeric vector of daily snowfall (water equivalent), in the
#'   units of `ppt`.
#' @export
#' @seealso [get_rain()] for the rain part; [get_melt()] and
#'   [get_snowpack()] for what happens to the snow.
#' @examples
#' freeze <- get_freeze(jtemp = 1.5, tmean = c(-5, 1.5, 8))
#' get_snow(ppt = c(10, 10, 10), freeze = freeze)

get_snow = function(ppt, freeze){
  snow = (1 - freeze)*ppt
  return(snow)
}
