#' @title Run the NPS water balance model for one site
#'
#' @description
#' Runs the NPS water balance model (WBM) on a daily climate series for one
#' site. Each day it:
#' 1. splits precipitation into rain and snow with the Jennings threshold
#'    temperature ([get_freeze()], [get_rain()], [get_snow()]);
#' 2. melts snow with a degree-day factor and updates the snowpack
#'    ([get_melt()], [get_snowpack()]);
#' 3. sends `Direct_Frac` of the rain straight to runoff;
#' 4. calculates PET with `PET_Method` and multiplies it by `PET_mult`;
#' 5. updates soil water, with the capacity scaled by `Soil_mult`
#'    ([get_soil()]), and calculates AET ([get_AET()]) and the change in soil
#'    water;
#' 6. puts the surplus (water minus AET minus the soil change) in a storage
#'    reservoir that releases `Return_Rate` of its water each day
#'    ([get_storage()]); runoff is that release plus the direct rain.
#'
#' With the defaults `Direct_Frac = 0`, `Return_Rate = 1`, `PET_mult = 1`
#' and `Soil_mult = 1` (the CSU additions), it matches the original model.
#' AET is calculated from the unadjusted PET, so `PET_mult` changes the soil
#' water but not AET.
#'
#' These model functions are based on Dave Thoma's water balance spreadsheet
#' model; they were first written on 2019-10-30 by ARC and updated on
#' 2021-04-14 by MGS.
#'
#' @param DailyWB data.table. The daily climate for the site, with no gaps
#'   in the dates: `x` (longitude), `y` (latitude, decimal degrees), `date`
#'   (Date), `GCM` (model or source name, kept in the output), `ppt_mm`
#'   (precipitation, mm) and `tmean_C` (mean temperature, degrees C). For
#'   `PET_Method = "Penman-Monteith"` it also needs `tmax_C` and `tmin_C`.
#'   It must be a data.table: with a plain data frame the storage step makes
#'   a list column and the function stops with an error, so convert it with
#'   [data.table::as.data.table()] first. Required.
#' @param point_params Data frame with one row. The site parameters, e.g.
#'   one row of [get_nps_wbm_params()]: `Elev` (m), `Slope` and `Aspect`
#'   (degrees), `SWC.Max` (soil water capacity, mm), `J.Temp` (Jennings
#'   threshold temperature, degrees C) and, for Penman-Monteith, `y`
#'   (latitude). Required.
#' @param Direct_Frac Numeric from 0 to 1. The fraction of rain that goes
#'   straight to runoff, skipping the soil. Default: `0`.
#' @param Return_Rate Numeric from 0 to 1. The fraction of the storage
#'   reservoir released as runoff each day.
#'   * `1`: all surplus becomes runoff the same day.
#'   * Less than 1: runoff is spread over the following days.
#'
#'   Default: `1`.
#' @param PET_mult Numeric. A factor that PET is multiplied by before the
#'   soil water step, to correct a known bias. Default: `1`.
#' @param Soil_mult Numeric. A factor that the soil water capacity
#'   (`SWC.Max`) is multiplied by. Default: `1`.
#' @param PET_Method String. The PET method, one of:
#'   * `"Oudin"`: [get_OudinPET()], with the site's slope,
#'     aspect and `Shade.Coeff`; 0 under snowpack.
#'   * `"Hamon"`: [ET_Hamon_daily()], from temperature and day length.
#'   * `"Penman-Monteith"`: [ET_PenmanMonteith_daily()], from maximum and
#'     minimum temperature, with a wind speed of 2.2 m/s.
#'
#'   Pass one value. The default, `c("Oudin", "Hamon", "Penman-Monteith")`,
#'   stops with an error.
#' @param hock_coef Numeric. The degree-day melt factor, in mm per degree C
#'   per day. Hock (2003) reports about 2.5 for Gooseberry Creek, Utah.
#'   Default: `4`.
#' @param Snowpack.Init Numeric. The snowpack before the first day, in mm.
#'   Default: `0`.
#' @param Soil.Init Numeric. The soil water before the first day, in mm.
#'   Default: `0` (empty).
#' @param Shade.Coeff Numeric from 0 to 1. The shading factor for the Oudin
#'   method. Default: `1` (no shade).
#' @param T.Base Numeric. The base temperature for growing degree-days, in
#'   degrees C. Default: `0`.
#'
#' @return A long data.table, returned invisibly (assign it), with one row
#'   per day and variable: `date`, `x`, `y`, `GCM`, `vars` and `vals`. The
#'   variables are `ppt_mm`, `RUNOFF`, `RAIN`, `SNOW`, `MELT`, `AET`,
#'   `PET_mod`, `tmean_C`, `excess_wat` (water added to storage), `PACK`
#'   (snowpack) and `SOIL`. All are in inches (despite the `ppt_mm` name),
#'   except `tmean_C` in degrees C.
#' @references
#' Hock, R. (2003). Temperature index melt modelling in mountain areas.
#'   Journal of Hydrology, 282(1-4), 104-115.
#'   <https://doi.org/10.1016/S0022-1694(03)00257-9>
#' @export
#' @seealso [run_NPS_WBM_points()] to run it for many points and models;
#'   [get_nps_wbm_params()] for `point_params`; [runoff_routing_gw()] and
#'   [runoff_routing_snow_fun()] to route the runoff.
#' @examples
#' # One made-up water year at one site
#' dates <- seq(as.Date("2020-10-01"), as.Date("2021-09-30"), by = "day")
#' doy <- as.numeric(format(dates, "%j"))
#' set.seed(1)
#' daily <- data.table::data.table(
#'   x = -112.98, y = 37.2, GCM = "example", date = dates,
#'   ppt_mm = ifelse(runif(length(dates)) < 0.3, rexp(length(dates), 0.08), 0),
#'   tmean_C = 12 - 12 * cos(2 * pi * (doy - 20) / 365)
#' )
#' site <- data.frame(Elev = 1300, Slope = 5, Aspect = 180, SWC.Max = 50,
#'                    J.Temp = 1.5, y = 37.2)
#'
#' wb <- nps_wbm(daily, site, PET_Method = "Oudin")
#'
#' # Water year totals, in inches
#' totals <- wb[wb$vars %in% c("ppt_mm", "RUNOFF", "AET", "SNOW"), ]
#' tapply(totals$vals, totals$vars, sum)

nps_wbm <- function(DailyWB, 
                    point_params, 
                    Direct_Frac = 0, # CSU ADDITION
                    Return_Rate = 1, # CSU ADDITION
                    PET_mult = 1,  # CSU ADDITION
                    Soil_mult = 1, # CSU ADDITION
                    PET_Method = c("Oudin", "Hamon", "Penman-Monteith"), 
                    hock_coef = 4, 
                    Snowpack.Init = 0,   # according to Mike's code
                    Soil.Init = 0, # Soil.Init is full (1) from Mike's code, but empty (0) in Amber's
                    Shade.Coeff = 1,
                    T.Base = 0) {
  
  DailyWB$doy <- lubridate::yday(DailyWB$date)
  DailyWB$daylength <- get_daylength(DailyWB$date, DailyWB$y)
  DailyWB$jtemp <- as.numeric(point_params$J.Temp) #as.numeric(get_jtemp(DailyWB$y, DailyWB$x)) 
  DailyWB$F <- get_freeze(DailyWB$jtemp, DailyWB$tmean_C)
  DailyWB$RAIN <- get_rain(DailyWB$ppt_mm, DailyWB$F)
  DailyWB$SNOW <- get_snow(DailyWB$ppt_mm, DailyWB$F)
  DailyWB$MELT <- get_melt(DailyWB$tmean_C, DailyWB$jtemp, hock=hock_coef, DailyWB$SNOW, Snowpack.Init)
  DailyWB$PACK <-  get_snowpack(DailyWB$jtemp, DailyWB$SNOW, DailyWB$MELT)
  DailyWB$DIRECT <- DailyWB$RAIN * Direct_Frac # CSU ADD
  #DailyWB$W = DailyWB$MELT + DailyWB$RAIN
  DailyWB$W = DailyWB$MELT + DailyWB$RAIN - DailyWB$DIRECT # CSU MOD
  if(PET_Method == "Hamon"){
    DailyWB$PET <- ET_Hamon_daily(DailyWB)
  } else {
    if(PET_Method == "Penman-Monteith"){
      DailyWB$PET <- ET_PenmanMonteith_daily(DailyWB, point_params$Elev, point_params$y, wind = 2.2)
    } else {
      if(PET_Method == "Oudin"){
        DailyWB$PET <- get_OudinPET(DailyWB$doy, DailyWB$y, DailyWB$PACK, 
                                    DailyWB$tmean_C, point_params$Slope, 
                                    point_params$Aspect, Shade.Coeff)
      } else {
        print("Error - PET method not found")
      }
    }
  }
  DailyWB$PET_mod <- DailyWB$PET * PET_mult # CSU ADD
  #DailyWB$PET = modify_PET(DailyWB$PET, Slope, Aspect, Lat, Shade.Coeff)
  DailyWB$W_PET <- DailyWB$W - DailyWB$PET
  DailyWB$SOIL <- get_soil(DailyWB$W, Soil.Init, DailyWB$PET_mod, DailyWB$W_PET, Soil_mult * point_params$SWC.Max)
  #DailyWB$DSOIL <- diff(c(Soil.Init, DailyWB$SOIL)) # What is the supposed to be doing?
  DailyWB$DSOIL <- get_d_soil(DailyWB$SOIL, swc.0 = Soil.Init)
  DailyWB$AET <- get_AET(DailyWB$W, DailyWB$PET, DailyWB$SOIL, Soil.Init)
  #DailyWB$W_ET_DSOIL = DailyWB$W - DailyWB$AET - DailyWB$DSOIL
  DailyWB$STORAGE_ADD <- DailyWB$W - DailyWB$AET - DailyWB$DSOIL # CSU ADD
  DailyWB$STORAGE_RELEASE <- get_storage(DailyWB$STORAGE_ADD, Return_Rate)["storage_release"] # CSU ADD
  DailyWB$W_ET_DSOIL =  DailyWB$STORAGE_RELEASE + DailyWB$DIRECT
  DailyWB$STORAGE_REMAIN = get_storage(DailyWB$STORAGE_ADD, Return_Rate)["storage_remain"] # CSU ADD
  DailyWB$D = DailyWB$PET - DailyWB$AET
  DailyWB$GDD = get_GDD(DailyWB$tmean_C, T.Base)
  
  # Prepare output dataframe
  DailyWB_out <- DailyWB %>%
    data.table::data.table() %>%
    dplyr::ungroup() %>%
    dplyr::select(date, x, y, GCM, ppt_mm, "RUNOFF" = W_ET_DSOIL, RAIN, SNOW, MELT,AET,PET_mod, tmean_C,"excess_wat" = STORAGE_ADD, PACK, SOIL) %>%
    tidyr::pivot_longer(-c(date,x,y, GCM), 
                 values_to = "vals", 
                 names_to = "vars") %>%
    dplyr::mutate(vals = ifelse(vars == "tmean_C", vals, vals /25.4)) # mm to in for all but temp
}
