#' @title Download daily future WBM rasters for an area
#'
#' @description
#' Reads the daily rasters of the gridded NPS water balance model for future
#' climate (MACA models) from the WBM THREDDS server
#' (`http://www.yellowstone.solutions/thredds/`) for one variable, the given
#' years and models, so it needs internet. The rasters are stacked, given
#' the server's CRS (Lambert conformal conic) and extent, cropped to `aoi`,
#' and written to a GeoTIFF.
#'
#' @param park String. The four-letter park code; names the subfolder and
#'   the file. Required.
#' @param aoi sf object. The area of interest to crop to. Required.
#' @param years Numeric vector. The years to download. Default: `2023:2070`.
#' @param macas Character vector. The climate models, as
#'   `"<model>.<scenario>"`, e.g. `"CCSM4.rcp45"`. Required.
#' @param wb_var String. One water balance variable: `"runoff"`,
#'   `"soil_water"`, `"rain"`, `"accumswe"`, `"PET"`, `"Deficit"` or
#'   `"AET"`. Required.
#' @param path String. The folder the park subfolder is made in. The file is
#'   written to `<path>/<park>/wbm_daily_rasters/<park>_<wb_var>_selected_cfs_<first year>_<last year>.tif`,
#'   replacing an existing file; the folders are created if needed. Default:
#'   `park/` in the shared data folder, see [wavacore::wava_data_path()].
#'
#' @return A SpatRaster with one layer per day, named
#'   `<model>_<year>_<layer>`.
#' @export
#' @seealso [get_wbm_grids()], which also downloads the historical rasters;
#'   [get_wbm_aoi()] to summarize them.
#' @examplesIf interactive()
#' # Reads from the WBM THREDDS server, so it needs internet
#' aoi <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(
#'   c(xmin = -113.1, ymin = 37.1, xmax = -112.8, ymax = 37.4), crs = 4326
#' )))
#' runoff <- get_future_wbm("ZION", aoi, years = 2040, macas = "CCSM4.rcp45",
#'                          wb_var = "runoff", path = tempdir())
#' runoff

get_future_wbm <- function(park, aoi, years = 2023:2070, macas, wb_var, path = wavacore::wava_data_path("park/")) {
  # check that path to save files to exists
  if (!dir.exists(paste0(path, "/", park, "/wbm_daily_rasters/"))) {
    dir.create(file.path(paste0(
      path, "/", park, "/wbm_daily_rasters/"
    )), recursive = TRUE, showWarnings = FALSE)
  }
  
  # create df of all variable, year, scenario combinations
  map_dat <- tidyr::crossing(years, wb_var, macas) %>%
    tidyr::separate_wider_delim(macas, delim = ".", names = c("maca", "rcp"))
  
  
  # transform aoi to crs of WBM data
  aoi <- sf::st_transform(aoi, crs = "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  
  # download each raster file to temp directory
  rasters <- purrr::map(1:nrow(map_dat), function(x) {
    call <- paste0(
      "/vsicurl/",
      "http://www.yellowstone.solutions/thredds/fileServer/daily_or_monthly/gcm/",
      map_dat$rcp[x],
      "/",
      map_dat$maca[x],
      "/V_1_5_",
      map_dat$years[x],
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
    names(r) <- paste(map_dat$maca[x], map_dat$years[x], names(r), sep = "_")
    
    return(r)
    
  })
  
  ## combine all to single stack
  final_stack <- terra::rast(rasters)
  
  ## saved CRS and extent from raw downloaded file, that info does not attached to files pulled directly from URL
  terra::crs(final_stack) <- "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  
  terra::ext(final_stack) <- c(-2060750, 2639250, -1915500, 1384500)
  
  # crop to aoi
  final_stack <- terra::crop(final_stack, aoi)
  
  # save final raster stack to file
  terra::writeRaster(
    final_stack,
    filename = paste0(
      path,
      "/",
      park,
      "/wbm_daily_rasters/",
      park,
      "_",
      wb_var,
      "_selected_cfs_",
      years[1],
      "_",
      years[length(years)],
      ".tif"
    ),
    overwrite = TRUE
  )
  
  return(final_stack)
  
}
