#' @title Run the water balance model for many points and climate models
#'
#' @description
#' Runs [nps_wbm()] for every point and every climate model (`GCM`) in
#' `climate_data`, and combines the results:
#' 1. Gets the site parameters of each point with [get_nps_wbm_params()]
#'    (unless `use_final = TRUE`, which uses `param_list`).
#' 2. For each model and point, picks the point's rows of `climate_data` by
#'    matching `x` and `y` and runs [nps_wbm()].
#' 3. Averages the points for each day, variable and model: a plain mean, or
#'    an area-weighted mean when `area_weighted = TRUE` (skipped when
#'    `aggregate = FALSE`).
#'
#' It prints each model and point as it goes. Run time grows with points x
#' models x days.
#'
#' @param points sf object. The model points with `x` and `y` columns that
#'   match `climate_data`, e.g. GridMET cell centers. Required.
#' @param climate_data Data frame. Daily climate for every point and model:
#'   `x`, `y`, `date`, `GCM`, `ppt_mm` and `tmean_C` (and `tmax_C`/`tmin_C`
#'   for Penman-Monteith), e.g. from `get_climate_historic()` or
#'   `get_climate_future()` in wavadeepdive with a `GCM` column. Required.
#' @param Direct_Frac Numeric from 0 to 1. The fraction of rain that goes
#'   straight to runoff; passed to [nps_wbm()]. Default: `0`.
#' @param Return_Rate Numeric from 0 to 1. The daily release fraction of the
#'   storage reservoir; passed to [nps_wbm()]. Default: `1`.
#' @param PET_mult Numeric. The PET multiplier; passed to [nps_wbm()].
#'   Default: `1`.
#' @param Soil_mult Numeric. The soil water capacity multiplier; passed to
#'   [nps_wbm()]. Default: `1`.
#' @param PET_Method String. `"Oudin"`, `"Hamon"` or `"Penman-Monteith"`,
#'   passed to [nps_wbm()]. Pass one value: the default, all three, stops
#'   with an error.
#' @param hock_coef Numeric. The degree-day melt factor, in mm per degree C
#'   per day; passed to [nps_wbm()]. Default: `4`.
#' @param Snowpack.Init Numeric. The snowpack before the first day, in mm,
#'   for every point (replaced by the saved value when `use_final = TRUE`).
#'   Default: `0`.
#' @param Soil.Init Numeric. The soil water before the first day, in mm, for
#'   every point (replaced by the saved value when `use_final = TRUE`).
#'   Default: `0`.
#' @param Shade.Coeff Numeric from 0 to 1. The shading factor for the Oudin
#'   method; passed to [nps_wbm()]. Default: `1`.
#' @param T.Base Numeric. The base temperature for growing degree-days;
#'   passed to [nps_wbm()]. Default: `0`.
#' @param area_weighted Logical. Whether the points stand for parts of grid
#'   cells.
#'   * `TRUE`: parameters are area-weighted over `grid` within `aoi` (see
#'     [get_nps_wbm_params()]) and the daily results are area-weighted means.
#'     Needs `aoi` and `grid`.
#'   * `FALSE`: parameters at the points and plain means.
#'
#'   Default: `FALSE`.
#' @param aoi sf object or `NULL`. The area of interest polygon; required
#'   when `area_weighted = TRUE`. Default: `NULL`.
#' @param grid sf object or `NULL`. The grid cell polygons; required when
#'   `area_weighted = TRUE`. Default: `NULL`.
#' @param ret_final_cond Logical. Whether to also return each point's last
#'   soil water and snowpack, to start a later run from them.
#'   * `TRUE`: returns a list of the results and the point parameters with
#'     `final_soil` and `final_snowpack` columns. These come from the
#'     [nps_wbm()] output, so they are in inches.
#'   * `FALSE`: returns only the results.
#'
#'   Default: `FALSE`.
#' @param use_final Logical. Whether to start each point from saved
#'   conditions.
#'   * `TRUE`: uses `param_list` as the point parameters and its
#'     `final_soil` and `final_snowpack` as `Soil.Init` and `Snowpack.Init`.
#'     Note that those are in inches while the model expects mm.
#'   * `FALSE`: uses `Soil.Init` and `Snowpack.Init` for every point.
#'
#'   Default: `FALSE`.
#' @param param_list Data frame or `NULL`. Point parameters with
#'   `final_soil` and `final_snowpack`, from an earlier run with
#'   `ret_final_cond = TRUE`. Required when `use_final = TRUE`; ignored
#'   otherwise. Default: `NULL`.
#' @param aggregate Logical. Whether to combine the points.
#'   * `TRUE`: one value per day, variable and model.
#'   * `FALSE`: one value per point, day, variable and model, keeping `x` and
#'     `y`.
#'
#'   Default: `TRUE`.
#'
#' @return A data.table with one row per day and model (and point when
#'   `aggregate = FALSE`), with `date`, `GCM` and the variables in inches
#'   (degrees C for temperature): `precip_in_wbm`, `runoff_in_wbm`,
#'   `rain_in_wbm`, `snow_in_wbm`, `melt_in_wbm`, `aet_in_wbm`,
#'   `pet_in_wbm`, `tmean_C`, `excess_water_in_wbm`, `snowpack_in_wbm` and
#'   `soil_in_wbm`. With `ret_final_cond = TRUE`, a list of that table and
#'   the point parameters.
#' @export
#' @seealso [nps_wbm()], which runs each point; [get_nps_wbm_params()];
#'   [runoff_routing_gw()] and [runoff_routing_snow_fun()] to route
#'   `runoff_in_wbm`.
#' @examplesIf file.exists(wavacore::wava_data_path("all", "elevation_cropped.tif"))
#' # Two points with the same made-up climate for one water year
#' pts <- data.frame(x = c(-112.98, -112.95), y = c(37.20, 37.25))
#' pts <- sf::st_as_sf(pts, coords = c("x", "y"), crs = 4326, remove = FALSE)
#'
#' dates <- seq(as.Date("2020-10-01"), as.Date("2021-09-30"), by = "day")
#' doy <- as.numeric(format(dates, "%j"))
#' set.seed(1)
#' ppt <- ifelse(runif(length(dates)) < 0.3, rexp(length(dates), 0.08), 0)
#' climate <- do.call(rbind, lapply(seq_len(nrow(pts)), function(i) {
#'   data.frame(x = pts$x[i], y = pts$y[i], GCM = "example", date = dates,
#'              ppt_mm = ppt, tmean_C = 12 - 12 * cos(2 * pi * (doy - 20) / 365))
#' }))
#'
#' wb <- run_NPS_WBM_points(pts, climate, PET_Method = "Oudin")
#' colSums(wb[, c("precip_in_wbm", "runoff_in_wbm", "aet_in_wbm")])

run_NPS_WBM_points <- function(points, 
                               climate_data,
                               Direct_Frac = 0, 
                               Return_Rate = 1, 
                               PET_mult = 1,  
                               Soil_mult = 1, 
                               PET_Method = c("Oudin", "Hamon", "Penman-Monteith"), 
                               hock_coef = 4, 
                               Snowpack.Init = 0,   # according to Mike's code
                               Soil.Init = 0, # Soil.Init is full (1) from Mike's code, but empty (0) in Amber's
                               Shade.Coeff = 1,
                               T.Base = 0, 
                               area_weighted = FALSE, 
                               aoi = NULL, 
                               grid = NULL,
                               ret_final_cond = FALSE,
                               use_final = FALSE,
                               param_list = NULL,
                               aggregate = TRUE) {
  
 if(use_final == TRUE){
   
   points_params <- param_list
   
   
 } else if (ret_final_cond == TRUE) {
  # Get parameters at each point (slope, aspect, elevation)
  points_params <- get_nps_wbm_params(points, area_weighted = area_weighted,
                                      aoi = aoi, grid = grid) 
  
  
    points_params <- points_params %>%
      dplyr::mutate(final_soil = rep(0, nrow(points_params)),
                    final_snowpack = rep(0, nrow(points_params)))
    
 } else{
    
   points_params <- get_nps_wbm_params(points, area_weighted = area_weighted,
                                       aoi = aoi, grid = grid) 
  } 
  
  # get array of all GCMs for projections
  all_gcms <- climate_data %>%
    dplyr::pull(GCM) %>%
    unique()
  
  # Now, initiate output data storage as lists
  DailyWB_all <- vector(mode = "list",
                        length = length(all_gcms))
  
  DailyWB_pts <- vector(mode = "list",
                        length = nrow(points_params))
  
  print("Running wbm...")
  # First loop through each gcm
  for (j in 1:length(all_gcms)) {
    print(all_gcms[j])
    # Now loop through each point in each GCM
    for (i in 1:nrow(points_params)) {
      print(paste0("point: ", as.character(i)))
      
      # If using final soil storage or snowmelt from a previous run,
      # assign it here.
      if (use_final == TRUE) {
        Soil.Init <- points_params[i,]$final_soil
        Snowpack.Init <- points_params[i,]$final_snowpack
      }
      
      # Subset DailYWB for select GCM and Point
      DailyWB <- climate_data %>%
        data.table::data.table() %>%
        dplyr::filter(x == points_params[i,x],
                      y == points_params[i,y],
                      GCM == all_gcms[j]) 
      
      # Run water balance model for each point
      DailyWB_pts[[i]] <- nps_wbm(DailyWB = DailyWB,
                                  point_params = points_params[i,],
                                  Direct_Frac = Direct_Frac, 
                                  Return_Rate = Return_Rate, 
                                  PET_mult = PET_mult,  
                                  Soil_mult = Soil_mult, 
                                  PET_Method = PET_Method, 
                                  hock_coef = hock_coef, 
                                  Snowpack.Init = Snowpack.Init,  
                                  Soil.Init = Soil.Init, 
                                  Shade.Coeff = Shade.Coeff,
                                  T.Base = T.Base) %>% 
        data.table::data.table() 
      
      if(area_weighted == TRUE){
        
        DailyWB_pts[[i]] <- DailyWB_pts[[i]] %>%
          dplyr::mutate(area_weight = as.numeric(points_params[i,"area_weight"]))
        
      }
      
      # Save soil storage and snowpack from final timestep
      if (ret_final_cond == TRUE) {
        points_params[i,]$final_soil <- DailyWB_pts[[i]] %>% dplyr::filter(vars == "SOIL") %>% utils::tail(n = 1) %>% dplyr::pull(vals)
        points_params[i,]$final_snowpack <- DailyWB_pts[[i]] %>% dplyr::filter(vars == "PACK") %>% utils::tail(n = 1) %>% dplyr::pull(vals)
      }
      
    }
    
    if (area_weighted == TRUE) {
    # Summarize daily values for each point    
    DailyWB_all[[j]] <- DailyWB_pts %>%
      dplyr::bind_rows() %>%
      dplyr::group_by(date, vars, GCM) %>%
      dplyr::mutate(area_weight_norm = area_weight / sum(area_weight, na.rm = TRUE)) 
    
      if (aggregate == TRUE) {
        DailyWB_all[[j]] <- DailyWB_all[[j]] %>%
          dplyr::summarize(vals = stats::weighted.mean(vals, w = area_weight_norm, na.rm = TRUE), .groups = "drop")

      }
      
      } else {
    
      # Summarize daily values for each point    
      DailyWB_all[[j]] <- DailyWB_pts %>%
        dplyr::bind_rows() 
    
      if (aggregate == TRUE) {
        DailyWB_all[[j]] <- DailyWB_all[[j]] %>%
          dplyr::group_by(date, vars, GCM) %>%
          dplyr::select(-c(x, y)) %>%
          dplyr::summarize(vals = mean(vals, na.rm = TRUE), .groups = "drop") 
        }
    
    }
    
  }
  
  # Bind together all results for all GCMs
  DailyWB_ret <- DailyWB_all %>% 
    dplyr::bind_rows() %>%
    data.table::data.table() %>%
    tidyr::pivot_wider(names_from = vars, values_from = vals) %>%
    dplyr::rename(runoff_in_wbm = RUNOFF,
                  rain_in_wbm = RAIN,
                  snow_in_wbm = SNOW,
                  melt_in_wbm = MELT,
                  aet_in_wbm = AET,
                  precip_in_wbm = ppt_mm,
                  pet_in_wbm = PET_mod,
                  excess_water_in_wbm = excess_wat,
                  soil_in_wbm = SOIL,
                  snowpack_in_wbm = PACK
    )
  
  # Return either list containing WB results and points param or
  # just dataframe containing results depending on whether returning final 
  # conditions.
  if (ret_final_cond == TRUE) {
    return(list(DailyWB_ret, points_params))
  } else {
    return(DailyWB_ret)
  }
}



#test <- run_NPS_WBM_points(points, climate_data, PET_Method = "Oudin", ret_final_cond = "TRUE")
