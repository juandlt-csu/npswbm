# Cases for the golden comparison between the old code in
# nps_water_vulnerability (before the split) and npswbm.
#
# Used by make_golden.R, which writes golden/<id>.rds from the old code, and by
# test-golden.R, which checks that npswbm returns the identical object.
# Each case has these fields:
# `id`: the golden file name.
# `fun`: the npswbm function to call, by name.
# `old_fun`: its name in the old code, if it was renamed (snake case, issue #3).
#   Filled in from golden_old_names below; make_golden.R calls that one.
# `args`: a list of arguments, or a function that returns the list. The
#   function gets `fn`, which turns an npswbm function name into the function
#   for the run in progress, so `fn("nse")` is the old NSE() in make_golden.R.
#   The arguments must work with both the old and the new function.
# `needs`: "data" = the shared WaVA data folder, "internet" = a live download.
# `reads`: for a "data" case, the files under the data folder it reads; the
#   case skips without them, and their checksums go in golden_env.csv.
# `downloads`: what an "internet" case fetches, named in its skip reason.
# `keep`: optional function applied to the result before it is saved or
#   compared, for results identical() can't compare as they are.
# `baseline`: "pr165" for code that isn't in the pinned playground commit. These
#   cases come from make_golden_pr165.R, and make_golden.R leaves them out.
#
# Not covered, because they can't run in either version right now (see
# README.md): get_future_wbm, get_wbm_grids, get_historic_wbm_daymet, and
# get_wbm_aoi(download = TRUE), which calls get_wbm_grids.

# npswbm's names and the old code's, for every function it renamed
golden_old_names <- c(
  nse = "NSE", rsr = "RSR", pbias = "PBIAS", cod = "COD", sse = "SSE",
  rmse = "RMSE", mae = "MAE",
  et_hamon_daily = "ET_Hamon_daily",
  et_penman_monteith_daily = "ET_PenmanMonteith_daily",
  get_30_year_wbm_gridmet = "get30yearWBMGridMET",
  get_historic_wbm_daymet = "getHistoricWBMDayMET",
  get_aet = "get_AET", get_gdd = "get_GDD", get_oudin_pet = "get_OudinPET",
  modify_pet = "modify_PET",
  run_nps_wbm_points = "run_NPS_WBM_points"
)

# The objects load_wbm_rasters() assigns in the global environment
golden_raster_names <- c("dem", "soil", "slope", "aspect", "jtemp")

golden_clear_rasters <- function() {
  rm(list = intersect(golden_raster_names, ls(globalenv())), envir = globalenv())
}

# What to compare for a SpatRaster, which is a pointer and can't be saved: its
# shape, CRS, layer names, a sum per layer, and the values themselves (all of
# them for a small raster, the middle two rows of a large one).
golden_keep_raster <- function(r) {
  small <- terra::ncell(r) * terra::nlyr(r) <= 1e6
  list(
    dim = dim(r),
    crs = terra::crs(r),
    ext = as.vector(terra::ext(r)),
    names = names(r),
    sum = terra::global(r, "sum", na.rm = TRUE),
    values = if (small) {
      terra::values(r)
    } else {
      terra::values(r, row = floor(nrow(r) / 2), nrows = 2)
    }
  )
}

# Inputs, built here so both runs get the same objects without a download. No
# random numbers: the same water year every time, cold enough to have snow.
golden_climate <- function(x = -112.98, y = 37.20, GCM = "gcm_a", warmer = 0) {
  date <- seq(as.Date("2020-10-01"), as.Date("2021-09-30"), by = "day")
  i <- seq_along(date)
  doy <- as.numeric(format(date, "%j"))
  tmean_C <- 6 - 14 * cos(2 * pi * (doy - 20) / 365) + warmer
  data.table::data.table(
    x = x, y = y, GCM = GCM, date = date,
    ppt_mm = pmax(0, 14 * sin(i / 3) * cos(i / 17) - 2),
    tmean_C = tmean_C, tmax_C = tmean_C + 6, tmin_C = tmean_C - 6
  )
}

# What get_nps_wbm_params() returns for a point, for the cases that don't read
# the rasters
golden_params <- function(x = -112.98, y = 37.20) {
  data.table::data.table(x = x, y = y, Elev = 1300, Aspect = 180, Slope = 10,
                         SWC.Max = 150, J.Temp = 1.5)
}

# Two points in Zion Canyon, with their climate
golden_points <- function() {
  sf::st_as_sf(
    data.frame(x = c(-112.98, -112.95), y = c(37.20, 37.25),
               area_weight = c(0.6, 0.4)),
    coords = c("x", "y"), crs = 4326, remove = FALSE
  )
}

golden_points_climate <- function(warmer = 0, GCM = "gcm_a") {
  rbind(golden_climate(-112.98, 37.20, GCM, warmer),
        golden_climate(-112.95, 37.25, GCM, warmer + 1))
}

# The two points' climate with the small coordinate differences that separate
# downloads can have. b62f366 finds no rows for these; PR #165 rounds them.
golden_noisy_climate <- function() {
  clim <- golden_points_climate()
  clim$x <- clim$x + 1e-9
  clim$y <- clim$y - 1e-9
  clim
}

golden_box <- function(xmin, ymin, xmax, ymax) {
  sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(
    c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
    crs = sf::st_crs(4326)
  )))
}

# A grid cell around each point, and an area that cuts through them
golden_grid <- function() {
  rbind(golden_box(-113.00, 37.18, -112.96, 37.22),
        golden_box(-112.97, 37.23, -112.93, 37.27))
}

golden_aoi <- function() golden_box(-112.99, 37.19, -112.94, 37.26)

# The model's daily steps, for the cases that test one step at a time. `fn` is
# the one from `args`, so each run uses its own version of the earlier steps.
golden_steps <- function(fn) {
  clim <- golden_climate()
  jtemp <- 1.5
  freeze <- fn("get_freeze")(jtemp, clim$tmean_C)
  rain <- fn("get_rain")(clim$ppt_mm, freeze)
  snow <- fn("get_snow")(clim$ppt_mm, freeze)
  melt <- fn("get_melt")(clim$tmean_C, rep(jtemp, nrow(clim)), hock = 4,
                         snow = snow)
  w <- melt + rain
  pet <- 1 + 3 * (clim$tmean_C + 8) / 28
  list(tmean = clim$tmean_C, ppt = clim$ppt_mm, jtemp = jtemp, freeze = freeze,
       rain = rain, snow = snow, melt = melt, w = w, pet = pet, date = clim$date,
       swc = fn("get_soil")(w, 50, pet, w - pet, 100))
}

# Daily runoff with the columns the routing functions read
golden_routing_input <- function() {
  date <- seq(as.Date("2020-10-01"), as.Date("2021-09-30"), by = "day")
  i <- seq_along(date)
  rain <- pmax(0, 0.4 * sin(i / 3) * cos(i / 17))
  melt <- pmax(0, 0.3 * sin(pi * (i - 150) / 120)) * (i > 150 & i < 270)
  data.frame(date = date, runoff_in_wbm = 0.7 * (rain + melt),
             rain_in_wbm = rain, melt_in_wbm = melt)
}

# Observations for test_model_fit(), as long as the routing input
golden_routing_obs <- function() {
  i <- seq_len(nrow(golden_routing_input()))
  obs <- 0.1 + 0.05 * sin(i / 30) + 0.02 * cos(i / 5)
  obs[c(20, 21, 200)] <- NA
  obs
}

# The arguments for one test_model_fit() run, routing with runoff_routing_gw()
golden_fit_args <- function(fn, ...) {
  utils::modifyList(
    list(calibration_function = fn("runoff_routing_gw"),
         calibration_function_args = list(
           df = golden_routing_input(), tau_q = 2, tau_s = 20, vs = 0.4
         ),
         result_col = "Q_tot", obs_vals = golden_routing_obs(),
         obj_fun = fn("nse"), silence = TRUE),
    list(...)
  )
}

# The cases. Built in local() so the shorthands below don't land in the old
# code's global environment, where make_golden.R sources this file.
golden_cases <- local({
  # Shorthands for `reads`
  rasters <- file.path("all", c("elevation_cropped.tif", "water_storage.tif",
                                "merged_jennings2.tif"))
  wbm_dir <- file.path("park", "BRCA", "wbm_daily_rasters")
  brca_prefix <- "BRCA_watersupply_watershed_runoff"
  brca_historic <- file.path(
    wbm_dir, paste0(brca_prefix, "_historic_gridmet_1980_2023.tif")
  )
  brca_future <- file.path(
    wbm_dir, paste0(brca_prefix, "_selected_cfs_2024_2070.tif")
  )
  summaries <- "annual runoff rasters from the WBM summaries site (S3)"

  # The rasters the model reads, then cleared, so the cases don't depend on
  # each other's CRS
  keep_globals <- function(value) {
    kept <- lapply(golden_raster_names, function(name) {
      golden_keep_raster(get(name, envir = globalenv()))
    })
    golden_clear_rasters()
    stats::setNames(kept, golden_raster_names)
  }

  metric_cases <- unlist(lapply(
    c("nse", "rsr", "pbias", "cod", "sse", "rmse", "mae"),
    function(metric) {
      list(
        list(id = metric, fun = metric,
             args = list(obs = c(2.1, 3.4, 8.0, 5.2, 3.3, NA, 2.4),
                         sim = c(2.4, 3.1, 7.2, 5.9, 3.0, 2.8, 2.2))),
        list(id = paste0(metric, "_constant_obs"), fun = metric,
             args = list(obs = c(3, 3, 3, 3), sim = c(2, 3, 4, 5)))
      )
    }
  ), recursive = FALSE)

  c(
    # Fit metrics
    metric_cases,

    # The model's daily steps
    list(
      list(id = "get_daylength", fun = "get_daylength",
           args = list(dates = golden_climate()$date, lat = 37.2)),
      list(id = "get_freeze", fun = "get_freeze",
           args = list(jtemp = 1.5, tmean = golden_climate()$tmean_C)),
      list(id = "get_rain", fun = "get_rain",
           args = function(fn) {
             s <- golden_steps(fn)
             list(ppt = s$ppt, freeze = s$freeze)
           }),
      list(id = "get_snow", fun = "get_snow",
           args = function(fn) {
             s <- golden_steps(fn)
             list(ppt = s$ppt, freeze = s$freeze)
           }),
      list(id = "get_melt", fun = "get_melt",
           args = function(fn) {
             s <- golden_steps(fn)
             list(tmean = s$tmean, jtemp = rep(s$jtemp, length(s$tmean)),
                  hock = 4, snow = s$snow)
           }),
      list(id = "get_melt_initial_pack", fun = "get_melt",
           args = function(fn) {
             s <- golden_steps(fn)
             list(tmean = s$tmean, jtemp = rep(s$jtemp, length(s$tmean)),
                  hock = 3, snow = s$snow, sp.0 = 25)
           }),
      list(id = "get_snowpack", fun = "get_snowpack",
           args = function(fn) {
             s <- golden_steps(fn)
             list(jtemp = s$jtemp, snow = s$snow, melt = s$melt)
           }),
      list(id = "get_snowpack_initial_pack", fun = "get_snowpack",
           args = function(fn) {
             s <- golden_steps(fn)
             list(jtemp = s$jtemp, snow = s$snow, melt = s$melt, sp.0 = 25)
           }),
      list(id = "get_soil", fun = "get_soil",
           args = function(fn) {
             s <- golden_steps(fn)
             list(w = s$w, swc.0 = 50, pet = s$pet, w_pet = s$w - s$pet,
                  swc.max = 100)
           }),
      list(id = "get_soil_no_initial", fun = "get_soil",
           args = function(fn) {
             s <- golden_steps(fn)
             list(w = s$w, pet = s$pet, w_pet = s$w - s$pet, swc.max = 100)
           }),
      list(id = "get_aet", fun = "get_aet",
           args = function(fn) {
             s <- golden_steps(fn)
             list(w = s$w, pet = s$pet, swc = s$swc, swc.0 = 50)
           }),
      list(id = "get_aet_no_initial", fun = "get_aet",
           args = function(fn) {
             s <- golden_steps(fn)
             list(w = s$w, pet = s$pet, swc = s$swc)
           }),
      list(id = "get_storage", fun = "get_storage",
           args = function(fn) {
             s <- golden_steps(fn)
             list(storage_add = s$w - s$pet, return_rate = 0.5)
           }),
      list(id = "get_gdd", fun = "get_gdd",
           args = list(tmean = golden_climate()$tmean_C)),
      list(id = "get_gdd_tbase", fun = "get_gdd",
           args = list(tmean = golden_climate()$tmean_C, tbase = 5)),

      # PET methods
      list(id = "get_oudin_pet", fun = "get_oudin_pet",
           args = function(fn) {
             s <- golden_steps(fn)
             list(doy = as.numeric(format(s$date, "%j")), lat = 37.2,
                  snowpack = fn("get_snowpack")(s$jtemp, s$snow, s$melt),
                  tmean = s$tmean, slope = 10, aspect = 180)
           }),
      list(id = "get_oudin_pet_shade", fun = "get_oudin_pet",
           args = function(fn) {
             s <- golden_steps(fn)
             list(doy = as.numeric(format(s$date, "%j")), lat = 37.2,
                  snowpack = fn("get_snowpack")(s$jtemp, s$snow, s$melt),
                  tmean = s$tmean, slope = 25, aspect = 90, shade.coeff = 0.8)
           }),
      list(id = "et_hamon_daily", fun = "et_hamon_daily",
           args = function(fn) {
             clim <- golden_climate()
             list(x = data.frame(
               tmean_C = clim$tmean_C,
               daylength = fn("get_daylength")(clim$date, 37.2)
             ))
           }),
      list(id = "et_penman_monteith_daily", fun = "et_penman_monteith_daily",
           args = function(fn) {
             clim <- golden_climate()
             list(x = data.frame(
               doy = as.numeric(format(clim$date, "%j")), tmean_C = clim$tmean_C,
               tmax_C = clim$tmax_C, tmin_C = clim$tmin_C
             ), elev = 1300, lat = 37.2)
           }),

      # Internal helpers. Most are also covered through the functions above;
      # modify_pet() is called by nothing.
      list(id = "atm_press", fun = "atm_press",
           args = list(elev = c(0, 1300, 3000))),
      list(id = "get_svp", fun = "get_svp",
           args = list(temp = c(-10, 5, 25))),
      list(id = "vapor_curve", fun = "vapor_curve",
           args = list(temp = c(-10, 5, 25))),
      list(id = "psyc_constant", fun = "psyc_constant",
           args = list(elev = c(0, 1300, 3000))),
      list(id = "clear_sky_rad", fun = "clear_sky_rad",
           args = list(doy = c(15, 105, 196, 288), lat = 37.2, elev = 1300)),
      list(id = "actual_vp", fun = "actual_vp",
           args = list(rhmax = c(80, 90), rhmin = c(20, 30), tmax = c(25, 30),
                       tmin = c(5, 10))),
      list(id = "outgoing_rad", fun = "outgoing_rad",
           args = list(tmax = c(25, 30), tmin = c(5, 10), R.s = c(20, 25),
                       e.a = c(1, 1.5), R.so = c(26, 30))),
      list(id = "get_d_soil", fun = "get_d_soil",
           args = list(swc = c(50, 60, 40, 45), swc.0 = 10)),
      list(id = "get_d_soil_no_initial", fun = "get_d_soil",
           args = list(swc = c(50, 60, 40, 45))),
      list(id = "modify_pet", fun = "modify_pet",
           args = list(pet = c(1, 2, 3), slope = 10, aspect = 180, lat = 37.2,
                       freeze = c(0, 0.5, 1), shade.coeff = 0.8)),

      # The model for one site, on made-up site parameters
      list(id = "nps_wbm_oudin", fun = "nps_wbm",
           args = list(DailyWB = golden_climate(), point_params = golden_params(),
                       PET_Method = "Oudin")),
      list(id = "nps_wbm_hamon", fun = "nps_wbm",
           args = list(DailyWB = golden_climate(), point_params = golden_params(),
                       PET_Method = "Hamon")),
      list(id = "nps_wbm_penman_monteith", fun = "nps_wbm",
           args = list(DailyWB = golden_climate(), point_params = golden_params(),
                       PET_Method = "Penman-Monteith")),
      list(id = "nps_wbm_every_option", fun = "nps_wbm",
           args = list(DailyWB = golden_climate(), point_params = golden_params(),
                       PET_Method = "Oudin", Direct_Frac = 0.1, Return_Rate = 0.5,
                       PET_mult = 1.1, Soil_mult = 0.9, hock_coef = 3,
                       Snowpack.Init = 10, Soil.Init = 20, Shade.Coeff = 0.8,
                       T.Base = 5)),

      # Runoff routing
      list(id = "runoff_routing_gw", fun = "runoff_routing_gw",
           args = list(df = golden_routing_input(), tau_q = 2, tau_s = 20,
                       vs = 0.4)),
      list(id = "runoff_routing_gw_components", fun = "runoff_routing_gw",
           args = list(df = golden_routing_input(), tau_q = 2, tau_s = 20,
                       vs = 0.4, delay = 3, loss = 0.2, baseflow = 0.05,
                       return_components = TRUE)),
      list(id = "runoff_routing_gw_slow_only", fun = "runoff_routing_gw",
           args = list(df = golden_routing_input(), tau_q = 2, tau_s = 20,
                       vs = 0.4, loss = 0.5, slow_only = TRUE, max_inf = 0.01,
                       return_components = TRUE)),
      list(id = "runoff_routing_snow_fun", fun = "runoff_routing_snow_fun",
           args = list(df = golden_routing_input(), tau_q_rain = 2,
                       tau_s_rain = 20, vs_rain = 0.4, tau_s_melt = 30,
                       delay = 0)),
      list(id = "runoff_routing_snow_fun_components",
           fun = "runoff_routing_snow_fun",
           args = list(df = golden_routing_input(), tau_q_rain = 2,
                       tau_s_rain = 20, vs_rain = 0.4, tau_s_melt = 30,
                       delay = 3, return_components = TRUE)),
      list(id = "runoff_routing_snow_fun_few_missing_dates",
           fun = "runoff_routing_snow_fun",
           args = list(df = golden_routing_input()[-c(40, 41, 90), ],
                       tau_q_rain = 2, tau_s_rain = 20, vs_rain = 0.4,
                       tau_s_melt = 30, delay = 2)),
      list(id = "runoff_routing_snow_fun_many_missing_dates",
           fun = "runoff_routing_snow_fun",
           args = list(df = golden_routing_input()[-(40:50), ],
                       tau_q_rain = 2, tau_s_rain = 20, vs_rain = 0.4,
                       tau_s_melt = 30, delay = 0)),

      # Fitting a routing run. test_model_fit() compares obj_fun with NSE to
      # choose its penalty, so the metric has to come from `fn`.
      list(id = "test_model_fit_nse", fun = "test_model_fit",
           args = function(fn) golden_fit_args(fn)),
      list(id = "test_model_fit_mae", fun = "test_model_fit",
           args = function(fn) golden_fit_args(fn, obj_fun = fn("mae"))),
      list(id = "test_model_fit_last_column", fun = "test_model_fit",
           args = function(fn) {
             args <- golden_fit_args(fn)
             args$result_col <- NULL
             args
           }),
      list(id = "test_model_fit_monthly", fun = "test_model_fit",
           args = function(fn) {
             golden_fit_args(fn, date = golden_routing_input()$date,
                             time_ag = "Monthly")
           }),
      list(id = "test_model_fit_annual", fun = "test_model_fit",
           args = function(fn) {
             golden_fit_args(fn, date = golden_routing_input()$date,
                             time_ag = "Annual")
           }),
      list(id = "test_model_fit_return_dat", fun = "test_model_fit",
           args = function(fn) golden_fit_args(fn, return_dat = TRUE)),
      list(id = "test_model_fit_return_dat_date", fun = "test_model_fit",
           args = function(fn) {
             golden_fit_args(fn, date = golden_routing_input()$date,
                             return_dat = TRUE)
           }),
      list(id = "test_model_fit_penalty_nse", fun = "test_model_fit",
           args = function(fn) golden_fit_args(fn, obs_vals = c(1, 2))),
      list(id = "test_model_fit_penalty_mae", fun = "test_model_fit",
           args = function(fn) {
             golden_fit_args(fn, obs_vals = c(1, 2), obj_fun = fn("mae"))
           }),

      # A later run from final conditions. param_list stands in for the
      # rasters, so it needs no data folder.
      list(id = "run_nps_wbm_points_use_final", fun = "run_nps_wbm_points",
           args = list(
             points = NULL, climate_data = golden_points_climate(),
             PET_Method = "Oudin", use_final = TRUE,
             param_list = rbind(
               cbind(golden_params(-112.98, 37.20),
                     final_soil = 2, final_snowpack = 1),
               cbind(golden_params(-112.95, 37.25),
                     final_soil = 3, final_snowpack = 0)
             )
           )),

      # Live download, early, so an outage stops make_golden.R quickly
      list(id = "get_30_year_wbm_gridmet", fun = "get_30_year_wbm_gridmet",
           args = list(aoi = golden_box(-113.1, 37.1, -112.8, 37.4),
                       variable = "runoff",
                       cf = c("CCSM4.rcp45", "MIROC-ESM.rcp85"),
                       future_years = "2040_2069", save = FALSE),
           needs = "internet", downloads = summaries,
           keep = golden_keep_raster),

      # The rasters. Each load_wbm_rasters() case clears what it loaded, and
      # the cases after it load them again through get_nps_wbm_params().
      list(id = "load_wbm_rasters_wgs84", fun = "load_wbm_rasters",
           args = list(crs = "epsg:4326"),
           needs = "data", reads = rasters, keep = keep_globals),
      list(id = "load_wbm_rasters_albers", fun = "load_wbm_rasters",
           args = list(crs = "epsg:5070"),
           needs = "data", reads = rasters, keep = keep_globals),
      list(id = "get_nps_wbm_params", fun = "get_nps_wbm_params",
           args = list(points = golden_points()),
           needs = "data", reads = rasters),
      list(id = "get_nps_wbm_params_area_weighted", fun = "get_nps_wbm_params",
           args = list(points = golden_points(), area_weighted = TRUE,
                       aoi = golden_aoi(), grid = golden_grid()),
           needs = "data", reads = rasters),

      # The model for many points
      list(id = "run_nps_wbm_points_oudin", fun = "run_nps_wbm_points",
           args = list(points = golden_points(),
                       climate_data = golden_points_climate(),
                       PET_Method = "Oudin"),
           needs = "data", reads = rasters),
      list(id = "run_nps_wbm_points_hamon", fun = "run_nps_wbm_points",
           args = list(points = golden_points(),
                       climate_data = golden_points_climate(),
                       PET_Method = "Hamon"),
           needs = "data", reads = rasters),
      list(id = "run_nps_wbm_points_penman_monteith", fun = "run_nps_wbm_points",
           args = list(points = golden_points(),
                       climate_data = golden_points_climate(),
                       PET_Method = "Penman-Monteith"),
           needs = "data", reads = rasters),
      list(id = "run_nps_wbm_points_not_aggregated", fun = "run_nps_wbm_points",
           args = list(points = golden_points(),
                       climate_data = golden_points_climate(),
                       PET_Method = "Oudin", aggregate = FALSE),
           needs = "data", reads = rasters),
      list(id = "run_nps_wbm_points_two_gcms_every_option",
           fun = "run_nps_wbm_points",
           args = list(points = golden_points(),
                       climate_data = rbind(
                         golden_points_climate(0, "gcm_a"),
                         golden_points_climate(2, "gcm_b")
                       ),
                       PET_Method = "Oudin", Direct_Frac = 0.1, Return_Rate = 0.5,
                       PET_mult = 1.1, Soil_mult = 0.9, hock_coef = 3,
                       Snowpack.Init = 10, Soil.Init = 20, Shade.Coeff = 0.8,
                       T.Base = 5),
           needs = "data", reads = rasters),
      list(id = "run_nps_wbm_points_final_conditions", fun = "run_nps_wbm_points",
           args = list(points = golden_points(),
                       climate_data = golden_points_climate(),
                       PET_Method = "Oudin", ret_final_cond = TRUE),
           needs = "data", reads = rasters),
      list(id = "run_nps_wbm_points_area_weighted", fun = "run_nps_wbm_points",
           args = list(points = golden_points(),
                       climate_data = golden_points_climate(),
                       PET_Method = "Oudin", area_weighted = TRUE,
                       aoi = golden_aoi(), grid = golden_grid()),
           needs = "data", reads = rasters),
      list(id = "run_nps_wbm_points_area_weighted_not_aggregated",
           fun = "run_nps_wbm_points",
           args = list(points = golden_points(),
                       climate_data = golden_points_climate(),
                       PET_Method = "Oudin", area_weighted = TRUE,
                       aoi = golden_aoi(), grid = golden_grid(),
                       aggregate = FALSE),
           needs = "data", reads = rasters),

      # Reading downloaded WBM rasters. Their default path is the shared data
      # folder, and aoi_name picks the files by pattern.
      list(id = "get_wbm_aoi", fun = "get_wbm_aoi",
           args = list(park = "BRCA", aoi = NULL,
                       aoi_name = paste0(brca_prefix, "_historic"), gcm = NULL),
           needs = "data", reads = brca_historic),
      list(id = "get_wbm_aoi_average", fun = "get_wbm_aoi",
           args = list(park = "BRCA", aoi = NULL, aoi_name = brca_prefix,
                       gcm = NULL, aoi_average = TRUE),
           needs = "data", reads = c(brca_historic, brca_future))
    ),

    # Playground PR #165 (issue #2): rain-on-snow melt and rounded coordinates
    # aren't in the pinned commit, so these cases have their own baseline, the
    # head of that PR. The cases above show the defaults didn't change.
    list(
      list(id = "get_melt_ros", fun = "get_melt_ros", baseline = "pr165",
           args = function(fn) {
             s <- golden_steps(fn)
             list(tmean = s$tmean, jtemp = rep(s$jtemp, length(s$tmean)),
                  hock = 4, snow = s$snow, rain = s$rain)
           }),
      list(id = "get_melt_ros_initial_pack", fun = "get_melt_ros",
           baseline = "pr165",
           args = function(fn) {
             s <- golden_steps(fn)
             list(tmean = s$tmean, jtemp = rep(s$jtemp, length(s$tmean)),
                  hock = 3, snow = s$snow, rain = s$rain, sp.0 = 25)
           }),
      list(id = "get_melt_ros_no_rain", fun = "get_melt_ros", baseline = "pr165",
           args = function(fn) {
             s <- golden_steps(fn)
             list(tmean = s$tmean, jtemp = rep(s$jtemp, length(s$tmean)),
                  hock = 4, snow = s$snow, rain = rep(0, length(s$tmean)))
           }),
      list(id = "nps_wbm_ros_false", fun = "nps_wbm", baseline = "pr165",
           args = list(DailyWB = golden_climate(), point_params = golden_params(),
                       PET_Method = "Oudin", ROS = FALSE)),
      list(id = "nps_wbm_ros_oudin", fun = "nps_wbm", baseline = "pr165",
           args = list(DailyWB = golden_climate(), point_params = golden_params(),
                       PET_Method = "Oudin", ROS = TRUE)),
      list(id = "nps_wbm_ros_hamon", fun = "nps_wbm", baseline = "pr165",
           args = list(DailyWB = golden_climate(), point_params = golden_params(),
                       PET_Method = "Hamon", ROS = TRUE)),
      list(id = "nps_wbm_ros_penman_monteith", fun = "nps_wbm",
           baseline = "pr165",
           args = list(DailyWB = golden_climate(), point_params = golden_params(),
                       PET_Method = "Penman-Monteith", ROS = TRUE)),
      list(id = "nps_wbm_ros_every_option", fun = "nps_wbm", baseline = "pr165",
           args = list(DailyWB = golden_climate(), point_params = golden_params(),
                       PET_Method = "Oudin", ROS = TRUE, Direct_Frac = 0.1,
                       Return_Rate = 0.5, PET_mult = 1.1, Soil_mult = 0.9,
                       hock_coef = 3, Snowpack.Init = 10, Soil.Init = 20,
                       Shade.Coeff = 0.8, T.Base = 5)),
      list(id = "run_nps_wbm_points_coordinate_noise",
           fun = "run_nps_wbm_points", baseline = "pr165",
           args = list(
             points = NULL, climate_data = golden_noisy_climate(),
             PET_Method = "Oudin", use_final = TRUE,
             param_list = rbind(
               cbind(golden_params(-112.98, 37.20),
                     final_soil = 2, final_snowpack = 1),
               cbind(golden_params(-112.95, 37.25),
                     final_soil = 3, final_snowpack = 0)
             )
           )),
      list(id = "run_nps_wbm_points_coordinate_noise_rasters",
           fun = "run_nps_wbm_points", baseline = "pr165",
           args = list(points = golden_points(),
                       climate_data = golden_noisy_climate(),
                       PET_Method = "Oudin"),
           needs = "data", reads = rasters),
      list(id = "run_nps_wbm_points_coordinate_noise_not_aggregated",
           fun = "run_nps_wbm_points", baseline = "pr165",
           args = list(points = golden_points(),
                       climate_data = golden_noisy_climate(),
                       PET_Method = "Oudin", aggregate = FALSE),
           needs = "data", reads = rasters)
    )
  )
})

# Fill in old_fun from the table above
golden_cases <- lapply(golden_cases, function(case) {
  if (case$fun %in% names(golden_old_names)) {
    case$old_fun <- golden_old_names[[case$fun]]
  }
  case
})

# Run one case: build its arguments, make sure it can't write files, call it
# and apply its `keep`. `old = TRUE` is for make_golden.R: it calls the old code
# by its old names. The functions print progress, which is left out.
golden_run <- function(case, old = FALSE) {
  lookup <- parent.env(environment())
  fn <- function(name) {
    if (old && name %in% names(golden_old_names)) {
      name <- golden_old_names[[name]]
    }
    get(name, envir = lookup, mode = "function")
  }
  args <- if (is.function(case$args)) case$args(fn) else case$args
  # These write into the shared data folder unless told otherwise, and the
  # three with no such option can't be cases at all
  writes <- isTRUE(args$download) || isTRUE(args$save) ||
    (case$fun == "get_30_year_wbm_gridmet" && !isFALSE(args$save)) ||
    case$fun %in% c("get_future_wbm", "get_wbm_grids", "get_historic_wbm_daymet")
  if (writes) {
    stop("golden case ", case$id, " would write files; cases only compare ",
         "what a function returns", call. = FALSE)
  }
  fun <- if (old && !is.null(case$old_fun)) case$old_fun else case$fun
  utils::capture.output(value <- do.call(fun, args))
  if (!is.null(case$keep)) value <- case$keep(value)
  value
}

# Files in the shared data folder that the "data" cases read. make_golden.R
# records their checksums, and test-golden.R reports it when one has changed.
golden_data_files <- unique(unlist(lapply(golden_cases, function(case) {
  case$reads
})))

# md5 of a file
golden_checksum <- function(path) {
  if (!file.exists(path)) return("missing")
  unname(tools::md5sum(path))
}

# Check the fields of every case, so a typo (e.g. `need = "data"`) stops here
# instead of silently dropping a skip.
local({
  fields <- c("id", "fun", "old_fun", "args", "needs", "reads", "downloads",
              "keep", "baseline")
  for (case in golden_cases) {
    id <- if (is.character(case$id) && length(case$id) == 1) case$id else "?"
    bad <- function(...) stop("golden case ", id, ": ", ..., call. = FALSE)
    extra <- setdiff(names(case), fields)
    if (length(extra) > 0) bad("unknown field(s) ", toString(extra))
    if (id == "?") bad("`id` must be one string")
    if (!is.character(case$fun) || length(case$fun) != 1) {
      bad("`fun` must be one function name")
    }
    if (!is.list(case$args) && !is.function(case$args)) {
      bad("`args` must be a list or a function")
    }
    if (!is.null(case$needs) && !all(case$needs %in% c("data", "internet"))) {
      bad("`needs` must be \"data\", \"internet\" or both")
    }
    if ("data" %in% case$needs &&
        !(is.character(case$reads) && length(case$reads) >= 1)) {
      bad("a \"data\" case needs `reads` (what it reads in the data folder)")
    }
    if (!"data" %in% case$needs && !is.null(case$reads)) {
      bad("`reads` is only for \"data\" cases")
    }
    if ("internet" %in% case$needs &&
        !(is.character(case$downloads) && length(case$downloads) == 1)) {
      bad("an \"internet\" case needs `downloads` (what it fetches)")
    }
    if (!is.null(case$keep) && !is.function(case$keep)) {
      bad("`keep` must be a function")
    }
    if (!is.null(case$baseline) && !identical(case$baseline, "pr165")) {
      bad("`baseline` must be \"pr165\" or left out")
    }
  }
  ids <- vapply(golden_cases, function(case) case$id, character(1))
  if (anyDuplicated(ids)) {
    stop("golden case ids must be unique: ", toString(ids[duplicated(ids)]),
         call. = FALSE)
  }
})
