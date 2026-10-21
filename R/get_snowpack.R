#' @title Daily snowpack from snowfall and melt
#'
#' @description
#' Accumulates the snowpack day by day: each day's snowpack is the day
#' before's plus snowfall minus melt.
#'
#' These model functions are based on Dave Thoma's water balance spreadsheet
#' model; they were first written on 2019-10-30 by ARC and updated on
#' 2021-04-14 by MGS.
#'
#' @param jtemp Numeric. The Jennings threshold temperature. Not used; kept
#'   for compatibility. Required (any value).
#' @param snow Numeric vector. Daily snowfall, in mm (from [get_snow()]).
#'   Required.
#' @param melt Numeric vector. Daily melt, in mm (from [get_melt()]).
#'   Required.
#' @param sp.0 Numeric or `NULL`. The snowpack before the first day, in mm.
#'   `NULL` uses 0. Default: `NULL`.
#'
#' @return A numeric vector of the snowpack at the end of each day, in mm.
#' @export
#' @seealso [get_melt()]; [get_OudinPET()], which sets PET to 0 under
#'   snowpack.
#' @examples
#' tmean <- c(-6, -4, -1, 2, 5, 8)
#' snow <- c(20, 15, 5, 0, 0, 0)
#' melt <- get_melt(tmean, jtemp = rep(1.5, 6), hock = 4, snow = snow)
#' get_snowpack(jtemp = 1.5, snow = snow, melt = melt)

get_snowpack = function(jtemp, snow, melt, sp.0=NULL){
  low_thresh_temp = jtemp - 3
  sp.i = ifelse(!is.null(sp.0), sp.0, 0)
  snowpack <- vector()
  for(i in 1:length(melt)){
    snowpack[i] = sp.i+snow[i]-melt[i]
    sp.i=snowpack[i]
  }
  return(snowpack)
}
