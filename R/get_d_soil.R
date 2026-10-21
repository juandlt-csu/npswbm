#' Daily change in soil water content: each day's value minus the day
#' before's (the first day minus swc.0). Internal helper of [nps_wbm()].
#' @param swc Numeric vector. Daily soil water content, in mm.
#' @param swc.0 Numeric or NULL. Soil water before the first day; NULL uses 0.
#' @return Numeric vector. Daily change, in mm.
#' @noRd

get_d_soil=function(swc, swc.0=NULL){
  swc.0 = ifelse(!is.null(swc.0), swc.0, 0)
  d_soil = swc - dplyr::lag(swc, default=swc.0)
  return(d_soil)
}
