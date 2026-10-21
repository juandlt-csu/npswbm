#' @title Rainfall from precipitation and the freeze factor
#'
#' @description
#' Rain is precipitation times the freeze factor (`ppt * freeze`).
#'
#' These model functions are based on Dave Thoma's water balance spreadsheet
#' model; they were first written on 2019-10-30 by ARC and updated on
#' 2021-04-14 by MGS.
#'
#' @param ppt Numeric vector. Daily precipitation, in mm. Required.
#' @param freeze Numeric vector. The freeze factor (0-1) from
#'   [get_freeze()]: the fraction that falls as rain. Required.
#'
#' @return A numeric vector of daily rain, in the units of `ppt`.
#' @export
#' @seealso [get_snow()] for the snow part; [get_freeze()].
#' @examples
#' freeze <- get_freeze(jtemp = 1.5, tmean = c(-5, 1.5, 8))
#' get_rain(ppt = c(10, 10, 10), freeze = freeze)

get_rain = function(ppt, freeze){
  rain = ppt*freeze
  return(rain)
}
