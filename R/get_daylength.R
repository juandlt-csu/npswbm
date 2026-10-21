#' @title Day length for dates at a latitude
#'
#' @description
#' Returns the day length in hours for each date at the given latitude, with
#' [geosphere::daylength()]. [nps_wbm()] uses it for the Hamon PET method.
#'
#' These model functions are based on Dave Thoma's water balance spreadsheet
#' model; they were first written on 2019-10-30 by ARC and updated on
#' 2021-04-14 by MGS.
#'
#' @param dates Date vector (or strings like `"2021-06-21"`). The days.
#'   Required.
#' @param lat Numeric. The latitude in degrees, one value or one per date.
#'   Required.
#'
#' @return A numeric vector of day lengths in hours, one per date.
#' @export
#' @seealso [ET_Hamon_daily()], which uses it.
#' @examples
#' get_daylength(as.Date(c("2021-03-20", "2021-06-21", "2021-12-21")), lat = 37.2)

get_daylength = function(dates, lat) {
  yday <- as.numeric(strftime(dates, "%j"))
  dayl_h <- geosphere::daylength(lat, yday)
  return(dayl_h)
}
