#' @title Growing degree-days
#'
#' @description
#' Daily growing degree-days: `tmean - tbase` when `tmean` is at least
#' `tbase`, otherwise 0.
#'
#' These model functions are based on Dave Thoma's water balance spreadsheet
#' model; they were first written on 2019-10-30 by ARC and updated on
#' 2021-04-14 by MGS.
#'
#' @param tmean Numeric vector. The daily mean temperature, in degrees C.
#'   Required.
#' @param tbase Numeric or `NULL`. The base temperature, in degrees C.
#'   `NULL` uses 0. Default: `NULL`.
#'
#' @return A numeric vector of growing degree-days, one per day.
#' @export
#' @seealso [nps_wbm()], which calculates them as `GDD`.
#' @examples
#' get_GDD(c(-3, 2, 8, 15))
#' get_GDD(c(-3, 2, 8, 15), tbase = 5)

get_GDD = function(tmean, tbase=NULL){
  tb = ifelse(!is.null(tbase), tbase, 0)
  GDD = ifelse(tmean < tb, 0, tmean - tb)
  return(GDD)
}
