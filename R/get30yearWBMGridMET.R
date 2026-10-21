#' @title 30-year annual mean rasters of the gridded NPS water balance model
#'
#' @description
#' Downloads the 30-year annual mean rasters of the gridded NPS water balance
#' model (GridMET historical and MACA future runs, in mm) from the WBM
#' summaries site
#' (<http://screenedcleanedsummaries.s3-website-us-west-2.amazonaws.com/>),
#' crops each to `aoi`, and stacks them, so it needs internet. One file is
#' downloaded per variable, model and period, plus the historical ones.
#'
#' Models whose name contains MIROC-ESM are downloaded as
#' `MIROC-ESM-CHEM_rcp85`, because the WBM has no MIROC-ESM RCP4.5 run.
#'
#' @param aoi sf object. The area of interest to crop to, e.g. a buffered
#'   park boundary. Required.
#' @param variable Character vector. The WBM variables, e.g. `"runoff"`,
#'   `"AET"`, `"soil_water"`, `"Deficit"`. Default: `"runoff"`.
#' @param cf Character vector. The future climate models, as
#'   `"<model>.<scenario>"`, e.g. `"CCSM4.rcp45"` (as picked by
#'   `select_cfs_deep_dive()` in wavadeepdive). Required.
#' @param historic_years Character vector. The historical periods: any of
#'   `"1980_1999"`, `"1980_2019"`, `"1981_2010"` and `"2000_2019"`. Default:
#'   `"1981_2010"`.
#' @param future_years Character vector. The future periods: `"2040_2069"`
#'   and/or `"2070_2099"`. Default: `c("2040_2069", "2070_2099")`.
#' @param save Logical. Whether to write the stack to a GeoTIFF.
#'   * `TRUE`: writes `<filename_prefix>_wbm_30yr_annual_rasters.tif` to
#'     `<path>/wbm_gridmet_30_year/`, creating the folders if needed and
#'     replacing an existing file.
#'   * `FALSE`: writes nothing.
#'
#'   Default: `TRUE`.
#' @param path String. The folder that gets the `wbm_gridmet_30_year`
#'   subfolder when `save = TRUE`. Default: `all` in the shared data folder,
#'   see [wavacore::wava_data_path()], so a default call writes there.
#' @param filename_prefix String. The start of the file name, describing the
#'   area (e.g. `"BRCA"`). Required when `save = TRUE`; not used otherwise.
#'
#' @return A SpatRaster with one layer per file, named after the file
#'   (variable, model, period); values are annual means in mm.
#' @export
#' @seealso [get_future_wbm()] and [get_wbm_grids()] for daily WBM rasters.
#' @examplesIf interactive()
#' # Downloads from the WBM summaries site, so it needs internet
#' aoi <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(
#'   c(xmin = -113.1, ymin = 37.1, xmax = -112.8, ymax = 37.4), crs = 4326
#' )))
#' runoff_30yr <- get30yearWBMGridMET(aoi, variable = "runoff",
#'                                    cf = c("CCSM4.rcp45", "CanESM2.rcp85"),
#'                                    save = FALSE)
#' names(runoff_30yr)

get30yearWBMGridMET <- function(aoi,
                                variable = c("runoff"),
                                cf,
                                historic_years = c("1981_2010"),
                                future_years = c("2040_2069", "2070_2099"),
                                save = TRUE,
                                path = wavacore::wava_data_path("all"),
                                filename_prefix) {
  # Set lookup vars
  scenarios <- c('rcp45', 'rcp85', "historical")
  rcp <- gsub("^.*\\.", "", cf)
  cfs <- gsub("\\.", "_", cf)
  call_vars <- tidyr::crossing(variable, cfs, years = future_years) %>%
    dplyr::mutate(rcp = gsub("^.*\\_", "", cfs)) %>%
    dplyr::bind_rows(
      tidyr::crossing(
        variable,
        rcp = "historical",
        cfs = "gridmet_historical",
        years = historic_years
      )
    ) %>% 
    # fix name for "MIROC-ESM-CHEM" to make sure it matches what WBM link says
    ## also note, for now since WBM does not have MIROC-ESM for rcp 4.5, this will
    ## instead use rcp 8.5 if that was a selected model
    dplyr::mutate(cfs = dplyr::if_else(stringr::str_detect(cfs, "MIROC-ESM"), "MIROC-ESM-CHEM_rcp85", cfs))
  
  # Define web call URLs, only pulls annual means
  base_call <- "http://screenedcleanedsummaries.s3-website-us-west-2.amazonaws.com/"
  all_calls <- paste0(
    base_call,
    call_vars$variable,
    "/",
    call_vars$rcp,
    "/V_1_5_annual_",
    call_vars$cfs,
    "_",
    call_vars$variable,
    "_",
    call_vars$years,
    "_annual_means_cropped_units_mm.tif"
  )
  
  #download and crop each individual .tif and save as list
  tif_list <- purrr::map(all_calls, function(x) {
    #download a single .tif
    temp1 <- tempfile()
    utils::download.file(x, destfile = temp1, method = "curl")
    
    # read and crop raster to AOI
    wb_dat <- terra::rast(temp1) %>%
      terra::crop(sf::st_transform(aoi, sf::st_crs(.)))
    
    # assign name to raster layer
    names(wb_dat) <- gsub(".*V_1_5_annual_", "", x)
    
    return(wb_dat)
    
  })
  
  # bind all rasters into raster stack
  tif_stack <- terra::rast(tif_list)
  
  # save raster stack as single .tif (with named layers)
  if (save == TRUE) {
    # Set output directory
    out_dir <- paste0(path, "/wbm_gridmet_30_year/")
    
    if (!dir.exists(path)) {
      dir.create(path)
    }
    
    if (!dir.exists(out_dir)) {
      dir.create(out_dir)
    }
    
    terra::writeRaster(
      tif_stack,
      filename = paste0(out_dir, filename_prefix, "_wbm_30yr_annual_rasters.tif"),
      overwrite = TRUE
    )
  }
  
  return(tif_stack)
  
}

# # Test function -----------------
# source("setup.R")
# ## get aoi
# brca <- getParkBoundary("BRCA")
# aoi <- brca %>% st_buffer(20000)
# 
# ## variable
# variable <- c("runoff")
# 
# ## get cfs
# brca_cfs <- select_GCMs("BRCA", region = "IMR")
# cf <- brca_cfs %>% filter(centroid == "BRCA_future") %>% pull(GCM)
# 
# brca_wbm_30y <- get30yearWBMGridMET(aoi = aoi,
#                                     variable = variable,
#                                     cf = cf,
#                                     save = TRUE,
#                                     path = wava_data_path("all"),
#                                     filename_prefix = "BRCA")
# 
# # inspect maps
# library(leaflet)
# leaflet() %>% addTiles() %>% addRasterImage(brca_wbm_30y[[9]])

