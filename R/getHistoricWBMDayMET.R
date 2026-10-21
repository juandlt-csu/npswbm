#' @title Download monthly historical WBM values (DayMET version) for an area
#'
#' @description
#' Downloads the monthly NetCDF files of the DayMET-driven historical NPS
#' water balance model (the most recent version of the WBM) from
#' <http://www.yellowstone.solutions/thredds/catalog/daily_or_monthly/v2_historical/monthly/catalog.html>
#' for every year from 1980 to 2022 and each variable in `wb_vars`, crops
#' each to `aoi`, and writes it as a CSV. It needs internet and makes
#' 43 downloads per variable, so it takes a long time.
#'
#' It sets `options(timeout = 900)` for the session, and downloads into a
#' temporary `templeod_dataall` folder in the working directory, which it
#' deletes at the end.
#'
#' @param park String. The four-letter park code; names the subfolder and
#'   the files. Required.
#' @param aoi sf object. The area of interest to crop to. Required.
#' @param path String. The folder the park subfolder is made in. The CSVs go
#'   to `<path>/<park>/wbm_daymet_hist/<park>_daymet_<variable>_<year>.csv`,
#'   one row per grid cell and month with the value in `val` and the
#'   variable name in `var`, replacing existing files; the folders are
#'   created if needed. Default: `park` in
#'   the shared data folder, see [wavacore::wava_data_path()].
#' @param wb_vars Character vector. The variables to download. Default:
#'   `c("soil_water", "runoff", "rain", "agdd", "accumswe", "PET", "Deficit",
#'   "AET")`.
#'
#' @return Called for the CSV files it writes; the return value is not
#'   meant to be used.
#' @export
#' @seealso [get_wbm_grids()] for the daily GridMET and MACA versions.
#' @examples
#' \dontrun{
#' # Not run: downloads 43 files per variable from the WBM server
#' zion <- wavacore::getParkBoundary("ZION")
#' getHistoricWBMDayMET("ZION", zion, path = tempdir(), wb_vars = "runoff")
#' }

getHistoricWBMDayMET <- function(park, aoi, path = wavacore::wava_data_path("park"), 
                                 wb_vars = c("soil_water", "runoff", "rain", "agdd", "accumswe", "PET", "Deficit", "AET")
) {
  
  options(timeout = 900)

  if(!dir.exists(paste0(getwd(),"/templeod_dataall/"))){dir.create(file.path(getwd(),"/templeod_dataall/"), recursive = TRUE, showWarnings = FALSE)}
  if(!dir.exists(paste0(path, "/", park, "/wbm_daymet_hist/"))){dir.create(file.path(paste0(path, "/", park, "/wbm_daymet_hist/")), recursive = TRUE, showWarnings = FALSE)}
  
  years <- 1980:2022
  
  map_dat <- tidyr::crossing(years, wb_vars)
  
  for(i in 1:nrow(map_dat)){
    #http://www.yellowstone.solutions/thredds/fileServer/daily_or_monthly/v2_historical/gridmet_v_1_5_historical/V_1_5_2022_gridmet_historical_rain.nc4
    call <- paste0("http://www.yellowstone.solutions/thredds/fileServer/daily_or_monthly/v2_historical/monthly/v2_", 
                   map_dat$years[i], "_", map_dat$wb_vars[i], "_monthly.nc4")
    
    temp1 <- tempfile()
    utils::download.file(paste0(call), destfile = "templeod_dataall/data.nc4")#, method = "curl")
    
    #wb_crop <- ncdf4::nc_open("templeod_dataall/data.nc4")
    #wb_crop <- terra::rast("templeod_dataall/data.nc4")
    wb_wb <- stars::read_ncdf("templeod_dataall/data.nc4")
    # Set the CRS for the stars object. For whatever reason, a handful are unknown CRS.
    # Assuming they are the same as the others in the pull:
    sf::st_set_crs(wb_wb, 4326)
    wb_lat <- stars::read_ncdf("templeod_dataall/data.nc4", var = "lat") %>% as.data.frame()
    wb_lon <- stars::read_ncdf("templeod_dataall/data.nc4", var = "lon") %>% as.data.frame() 
    
    aoi <- aoi %>%
      sf::st_transform(sf::st_crs(wb_wb))
    
    wb_wb_crop <- sf::st_crop(wb_wb, aoi, crop = TRUE) %>% raster::as.data.frame(., xy = TRUE)
    
    table <- wb_wb_crop %>%
      dplyr::left_join(wb_lat, by = c("x", "y")) %>%
      dplyr::left_join(wb_lon, by = c("x", "y")) %>%
      # cant get the projections to line up -_-
      #st_as_sf(., coords = c("lon", "lat"), crs = 4326) %>%
      dplyr::mutate(var = names(.)[4]) %>%
      dplyr::rename(val = 4) %>%
      dplyr::filter(!is.na(val))
    
    #mapview(table) + mapview (aoi)
    
    readr::write_csv(table, file = paste0(path, "/", park, "/wbm_daymet_hist/", park, "_daymet_", map_dat$wb_vars[i], "_", map_dat$years[i], ".csv"))
    

  }
  
  
  unlink(paste0(getwd(),"/templeod_dataall"), recursive = TRUE)
  
  
}


# scratch code:
#   wb_crop <- wb_crop %>%
#     terra::crop(aoi, mask = TRUE)
#   
#   terra::writeRaster(wb_crop, filename = paste0(path, "/", park, "/wbm_hist/",
#                                                 park, "_", map_dat$wb_vars[i], "_", map_dat$years[i], ".nc4"),
#                      overwrite = TRUE)
# }
# 
