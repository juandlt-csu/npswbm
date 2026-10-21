#' @title Daily soil water content
#'
#' @description
#' Updates soil water content day by day. On days when available water
#' (`w`) exceeds PET, the soil fills by `w_pet` up to `swc.max`. On
#' other days it dries exponentially:
#' `swc - swc * (1 - exp(-(pet - w) / swc.max))`.
#'
#' These model functions are based on Dave Thoma's water balance spreadsheet
#' model; they were first written on 2019-10-30 by ARC and updated on
#' 2021-04-14 by MGS.
#'
#' @param w Numeric vector. Daily water available to the soil (rain plus
#'   melt), in mm. Required.
#' @param swc.0 Numeric or `NULL`. The soil water before the first day, in
#'   mm. `NULL` uses 0. Default: `NULL`.
#' @param pet Numeric vector. Daily PET, in mm. Required.
#' @param w_pet Numeric vector. `w - pet` for each day, in mm. Required.
#' @param swc.max Numeric. The soil's water holding capacity, in mm.
#'   Required.
#'
#' @return A numeric vector of soil water content at the end of each day, in
#'   mm.
#' @export
#' @seealso [get_AET()], which uses the soil water; [nps_wbm()].
#' @examples
#' w <- c(10, 0, 0, 25, 0)
#' pet <- c(3, 4, 5, 2, 6)
#' get_soil(w, swc.0 = 50, pet = pet, w_pet = w - pet, swc.max = 100)

get_soil = function(w, swc.0=NULL, pet, w_pet, swc.max){
  swc.i = ifelse(!is.null(swc.0), swc.0,0)
  soil=c()
  for(i in 1:length(pet)){
    soil[i] = ifelse(w[i]>pet[i], min((w_pet[i]+swc.i),swc.max), swc.i-swc.i*(1-exp(-(pet[i]-w[i])/swc.max)))
    swc.i=soil[i]
  }
  return(soil)  
}
