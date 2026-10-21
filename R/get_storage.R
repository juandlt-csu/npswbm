#' @title Daily release from a simple storage reservoir
#'
#' @description
#' Routes water through a linear storage: each day the water added
#' (`storage_add`) joins the water already stored, and `return_rate` of
#' the total is released. Used by [nps_wbm()] to turn surplus water into
#' runoff. Added by CSU to the original model.
#'
#' @param storage_add Numeric vector. Daily water added to storage, in mm
#'   (rain plus melt minus AET minus the change in soil water). Default:
#'   `DailyWB$STORAGE_ADD`, which needs an object called `DailyWB` in the
#'   calling environment; pass the values.
#' @param return_rate Numeric from 0 to 1. The fraction of stored water
#'   released each day.
#'   * `1`: everything is released the day it arrives (no storage).
#'   * Less than 1: release is spread over the following days.
#'
#'   Default: `Return_Rate`, which needs an object of that name in the
#'   calling environment; pass the value.
#'
#' @return A list of two numeric vectors, both in mm: `storage_release`
#'   (water released each day) and `storage_remain` (water left in storage
#'   at the end of each day).
#' @export
#' @seealso [nps_wbm()], whose `Return_Rate` argument is passed here.
#' @examples
#' get_storage(storage_add = c(10, 0, 0, 5, 0), return_rate = 0.5)

get_storage <- function(storage_add = DailyWB$STORAGE_ADD, 
                        return_rate = Return_Rate) {
  storage_release <- 0 * storage_add
  storage_release[1] <- storage_add[1] * return_rate
  
  storage_remain <- 0 * storage_add # Start empty
  storage_remain[1] <- storage_add[1] * (1-return_rate)
  
  for (i in 2:length(storage_release)) {
    storage_release[i] <- (storage_remain[i-1] + storage_add[i])*return_rate
    storage_remain[i] <- (storage_remain[i-1] + storage_add[i]) * (1-return_rate) 
  }
  
  return(list("storage_release" = storage_release,
              "storage_remain" = storage_remain))
}
