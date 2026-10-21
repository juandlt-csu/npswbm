#' @title Daily actual evapotranspiration
#'
#' @description
#' Actual evapotranspiration (AET) is PET on days when available water
#' (`w`) exceeds PET; otherwise it is the available water plus the water
#' taken from the soil that day (`w + swc[yesterday] - swc[today]`).
#'
#' These model functions are based on Dave Thoma's water balance spreadsheet
#' model; they were first written on 2019-10-30 by ARC and updated on
#' 2021-04-14 by MGS.
#'
#' @param w Numeric vector. Daily water available to the soil (rain plus
#'   melt), in mm. Required.
#' @param pet Numeric vector. Daily PET, in mm. Required.
#' @param swc Numeric vector. Daily soil water content, in mm (from
#'   [get_soil()]). Required.
#' @param swc.0 Numeric or `NULL`. The soil water before the first day, in
#'   mm. `NULL` uses 0. Default: `NULL`.
#'
#' @return A numeric vector of daily AET, in mm.
#' @export
#' @seealso [get_soil()]; [nps_wbm()].
#' @examples
#' w <- c(10, 0, 0, 25, 0)
#' pet <- c(3, 4, 5, 2, 6)
#' swc <- get_soil(w, swc.0 = 50, pet = pet, w_pet = w - pet, swc.max = 100)
#' get_AET(w, pet, swc, swc.0 = 50)

get_AET = function(w, pet, swc, swc.0=NULL){
  swc.i = ifelse(!is.null(swc.0), swc.0, 0)
  AET = c()
  for(i in 1:length(w)){
    AET[i] = ifelse(w[i] > pet[i], pet[i], w[i]+swc.i-swc[i])
    swc.i = swc[i]
  }
  return(AET)
}
