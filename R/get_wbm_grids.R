#' @title Download daily future and historical WBM rasters for an area
#'
#' @description
#' Reads the daily rasters of the gridded NPS water balance model from the
#' WBM THREDDS server (`http://www.yellowstone.solutions/thredds/`) for one
#' variable: the future runs for each model in `macas` and year in
#' `future_years`, and the GridMET historical run for each year in
#' `historic_years`. Each set is stacked, given the server's CRS (Lambert
#' conformal conic) and extent, cropped to `aoi`, and written to a GeoTIFF.
#' It needs internet; downloads run with [furrr::future_map()], so they are
#' parallel if a [future::plan()] is set.
#'
#' @param park String. The four-letter park code; names the subfolder and
#'   the files. Required.
#' @param aoi sf object. The area of interest to crop to. Required.
#' @param aoi_name String. A short name for the area, used in the file names
#'   so [get_wbm_aoi()] can find them. Default: `"AOI"`.
#' @param future_years Numeric vector. The future years to download.
#'   Default: `2024:2070`.
#' @param historic_years Numeric vector. The historical years to download.
#'   Default: `1980:2023`.
#' @param macas Character vector. The future climate models, as
#'   `"<model>.<scenario>"`, e.g. `"CCSM4.rcp45"`. Required.
#' @param wb_var String. One water balance variable: `"runoff"`,
#'   `"soil_water"`, `"rain"`, `"accumswe"`, `"PET"`, `"Deficit"` or
#'   `"AET"`. Required.
#' @param path String. The folder the park subfolder is made in. The files
#'   go to `<path>/<park>/wbm_daily_rasters/`, named
#'   `<park>_<aoi_name>_<wb_var>_selected_cfs_<years>.tif` and
#'   `..._historic_gridmet_<years>.tif`, replacing existing files; the
#'   folders are created if needed. Default: `park/` in the shared data
#'   folder, see [wavacore::wava_data_path()].
#'
#' @return A list of two SpatRasters, `future` and `historic`, with one layer
#'   per day, named `<model>_<wb_var>_<date>` and `<wb_var>_<date>`.
#' @export
#' @seealso [get_wbm_aoi()], which reads the files back as a table;
#'   [get_future_wbm()] for future rasters only.
#' @examplesIf interactive()
#' # Reads from the WBM THREDDS server, so it needs internet
#' aoi <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(
#'   c(xmin = -113.1, ymin = 37.1, xmax = -112.8, ymax = 37.4), crs = 4326
#' )))
#' grids <- get_wbm_grids("ZION", aoi, aoi_name = "example",
#'                        future_years = 2040, historic_years = 2000,
#'                        macas = "CCSM4.rcp45", wb_var = "runoff",
#'                        path = tempdir())
#' grids$historic

get_wbm_grids <- function(park, 
                          aoi,
                          aoi_name = "AOI",
                          future_years = 2024:2070, 
                          historic_years = 1980:2023,
                          macas, #= selected_cfs_rapid[c(1,4),]$GCM, 
                          wb_var, #= "runoff", 
                          path = wavacore::wava_data_path("park/")) {

    # check that path to save files to exists
  if (!dir.exists(paste0(path, "/", park))) {
    dir.create(file.path(paste0(
      path, "/", park
    )), recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(paste0(path, "/", park, "/wbm_daily_rasters/"))) {
    dir.create(file.path(paste0(
      path, "/", park, "/wbm_daily_rasters/"
    )), recursive = TRUE, showWarnings = FALSE)
  }
  
  # create df of all variable, year, scenario combinations
  map_dat <- tidyr::crossing(future_years, wb_var, macas) %>%
    tidyr::separate_wider_delim(macas, delim = ".", names = c("maca", "rcp"))
  
  
  # transform aoi to crs of WBM data
  aoi <- sf::st_transform(aoi, crs = "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  
  # download each raster file to temp directory
  future_rasters <- furrr::future_map(1:nrow(map_dat), function(x) {
    call <- paste0(
      "/vsicurl/",
      "http://www.yellowstone.solutions/thredds/fileServer/daily_or_monthly/gcm/",
      map_dat$rcp[x],
      "/",
      map_dat$maca[x],
      "/V_1_5_",
      map_dat$future_years[x],
      "_",
      map_dat$maca[x],
      "_",
      map_dat$rcp[x],
      "_",
      map_dat$wb_var[x],
      ".nc4"
    )
    
    r <- terra::rast(call)
    
    # assign informative names to each raster
    names(r) <- paste(map_dat$maca[x], wb_var, as.Date(paste0(map_dat$future_years[x], "-", sub(".*_", "", names(r))), format = "%Y-%j"), sep = "_")
    
    return(r)
    
  })
  
  ## combine all to single stack
  final_future_stack <- terra::rast(future_rasters)
  
  ## saved CRS and extent from raw downloaded file, that info does not attach to files pulled directly from URL
  terra::crs(final_future_stack) <- "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  
  terra::ext(final_future_stack) <- c(-2060750, 2639250, -1915500, 1384500)
  
  # crop to aoi
  final_future_stack <- terra::crop(final_future_stack, aoi)
  
  # save final raster stack to file
  terra::writeRaster(
    final_future_stack,
    filename = paste0(
      path,
      "/",
      park,
      "/wbm_daily_rasters/",
      park,
      "_",
      aoi_name,
      "_",
      wb_var,
      "_selected_cfs_",
      future_years[1],
      "_",
      future_years[length(future_years)],
      ".tif"
    ),
    overwrite = TRUE
  )
  
  # create df of all variable, year, scenario combinations
  map_dat <- tidyr::crossing(historic_years, wb_var)
  
  # transform aoi to crs of WBM data
  aoi <- sf::st_transform(aoi, crs = "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  
  # download each raster file to temp directory
  historic_rasters <- furrr::future_map(1:nrow(map_dat), function(x) {
    call <- paste0(
      "/vsicurl/",
      "http://www.yellowstone.solutions/thredds/fileServer/daily_or_monthly/v2_historical/gridmet_v_1_5_historical/V_1_5_",
      map_dat$historic_years[x],
      "_gridmet_historical_",
      map_dat$wb_var[x],
      ".nc4"
    )
    
    r <- terra::rast(call)
    
    # assign informative names to each raster
    names(r) <- paste(wb_var, as.Date(paste0(map_dat$historic_years[x], "-", sub(".*_", "", names(r))), format = "%Y-%j"), sep = "_")
    
    
    return(r)
    
  })
  
  ## combine all to single stack
  final_historic_stack <- terra::rast(historic_rasters)
  
  ## saved CRS and extent from raw downloaded file, that info does not attach to files pulled directly from URL
  terra::crs(final_historic_stack) <- "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  
  terra::ext(final_historic_stack) <- c(-2060750, 2639250, -1915500, 1384500)
  
  # crop to aoi
  final_historic_stack <- terra::crop(final_historic_stack, aoi)
  
  # save final raster stack to file
  terra::writeRaster(
    final_historic_stack,
    filename = paste0(
      path,
      "/",
      park,
      "/wbm_daily_rasters/",
      park,
      "_",
      aoi_name,
      "_",
      wb_var,
      "_historic_gridmet_",
      historic_years[1],
      "_",
      historic_years[length(historic_years)],
      ".tif"
    ),
    overwrite = TRUE
  )
  

  return(list("future" = final_future_stack, "historic" = final_historic_stack))
  
}

