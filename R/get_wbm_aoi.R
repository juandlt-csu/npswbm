#' @title Daily WBM values for an area from saved rasters
#'
#' @description
#' Reads the daily WBM rasters saved by [get_wbm_grids()] for an area
#' (the files in `<path><park>/wbm_daily_rasters/` whose names contain
#' `aoi_name`) and returns them as a long table, converted from the files'
#' units (tenths of mm) to inches. Historical layers get `gcm =
#' "gridmet.historical"`. Files are read with [furrr::future_map_dfr()].
#'
#' @param park String. The four-letter park code (the subfolder name).
#'   Required.
#' @param aoi sf object. The area of interest; only used when
#'   `download = TRUE`. Required.
#' @param aoi_name String. The area name used in the file names by
#'   [get_wbm_grids()]. Required.
#' @param gcm Character vector. The future climate models to download when
#'   `download = TRUE`. Default: `select_cfs$GCM`, which needs an object
#'   called `select_cfs` in the global environment.
#' @param download Logical. Whether to download the rasters first.
#'   * `TRUE`: meant to run [get_wbm_grids()] for soil_water, runoff, rain,
#'     accumswe, PET, Deficit and AET first (minutes to hours), but it
#'     currently stops with an error (`object 'gcms' not found`); run
#'     [get_wbm_grids()] yourself instead.
#'   * `FALSE`: reads the files already saved. Stops with an error if there
#'     are none.
#'
#'   Default: `FALSE`.
#' @param aoi_average Logical. Whether to average over the area.
#'   * `TRUE`: one row per date and model, with one column per variable
#'     (the mean over all cells).
#'   * `FALSE`: one row per cell, date, model and variable, keeping `x` and
#'     `y`.
#'
#'   Default: `FALSE`.
#' @param path String. The folder that holds the park subfolders. It must
#'   end with `/`, because the park code is pasted straight on. Default:
#'   `park/` in the shared data folder, see [wavacore::wava_data_path()].
#'
#' @return A data frame. With `aoi_average = FALSE`: `x`, `y`, `gcm`,
#'   `wbm_var` (`runoff_in`, `rain_in`, `aet_in`, `pet_in`, `accumswe_in`,
#'   `soil_water_in`, `deficit_in`), `date` and `val` (inches). With
#'   `aoi_average = TRUE`: `date`, `gcm` and one column per variable.
#' @export
#' @seealso [get_wbm_grids()], which saves the rasters.
#' @examplesIf interactive()
#' # Downloads one variable for two years into a temporary folder, then
#' # reads it back; needs internet
#' aoi <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(
#'   c(xmin = -113.1, ymin = 37.1, xmax = -112.8, ymax = 37.4), crs = 4326
#' )))
#' out <- paste0(tempdir(), "/")
#' get_wbm_grids("ZION", aoi, aoi_name = "example", future_years = 2040,
#'               historic_years = 2000, macas = "CCSM4.rcp45",
#'               wb_var = "runoff", path = out)
#' runoff <- get_wbm_aoi("ZION", aoi, aoi_name = "example", gcm = "CCSM4.rcp45",
#'                       aoi_average = TRUE, path = out)
#' head(runoff)

get_wbm_aoi <- function(park,
                        aoi,
                        aoi_name,
                        gcm = select_cfs$GCM,
                        download = FALSE,
                        aoi_average = FALSE,
                        path = wavacore::wava_data_path("park/")) {
  
  if (download == TRUE) {
    print("Downloading gridded WBM. This may take several minutes to hours...\n\n")
    
    furrr::future_walk(
      c(
        "soil_water",
        "runoff",
        "rain",
        "accumswe",
        "PET",
        "Deficit",
        "AET"
      ),
      ~ get_wbm_grids(
        park = park,
        aoi_name = aoi_name,
        aoi = aoi,
        wb_var = .x,
        macas = gcms,
        path = path
      )
    )
    
    print("Download complete!\n\n")
  }
  
  # set up directory with wbm rasters
  wbm_aoi_dir <- paste0(path,park,"/wbm_daily_rasters/")
  
  # create function to clean and convert wbm rasters to data frame
  rast_read <- function(rast_list) {
    
    terra::rast(rast_list) %>%
      terra::as.data.frame(xy = TRUE) %>%
      # convert to data.table for speed
      data.table::data.table() %>% 
      # remove column names that have NA for the data
      dplyr::select(-dplyr::contains("_NA")) %>%
      tidyr::pivot_longer(-(x:y), names_to = "var", values_to = "val") %>%
      # Add historic to name
      dplyr::mutate(var =
                      dplyr::if_else(
                        grepl(
                          '^accumswe|^runoff|^rain|^PET|^AET|^soil_water|^Deficit',
                          var
                        ),
                        paste0("gridmet.historical_", var),
                        var
                      ),
                    val = val / (25.4 * 10)) %>% # 10 = conversion factor, 25.4 = mm to in
      dplyr::mutate(var = gsub("soil_water", "soilwater", var)) %>%
      tidyr::separate(var, 
               into = c("gcm", "wbm_var", "date"), 
               sep = "_") %>%
      dplyr::mutate(wbm_var = 
                      stringr::str_replace_all(
                        wbm_var, 
                        c("runoff" = "runoff_in", 
                          "rain" = "rain_in", 
                          "AET" = "aet_in",
                          "PET" = "pet_in",
                          "accumswe" = "accumswe_in",
                          "soilwater" = "soil_water_in",
                          "Deficit" = "deficit_in")),
                    date = as.Date(date)) 
  }
  
  # list all aoi wbm rasters
  wbm_aoi <- list.files(wbm_aoi_dir,full.names = TRUE) %>% 
            grep(aoi_name, ., value=TRUE) 
  
  
  if (length(wbm_aoi) < 1) {
    
    stop("Must download wbm files first! Try function argument: download = TRUE.")
  
    }
    
  if(aoi_average == TRUE) {
    
    wbm_aoi_cleaned <- wbm_aoi %>%
      furrr::future_map_dfr(~ rast_read(.)) %>%
      dplyr::select(-c(x,y)) %>%
      dplyr::group_by(date, wbm_var, gcm) %>%
      dplyr::summarize_all(.funs = 
                      list(val = ~ mean(val, na.rm = TRUE)), 
                    .groups = "keep") %>%
      tidyr::pivot_wider(names_from = "wbm_var", values_from = "val") %>% 
      dplyr::ungroup()
    
  } else {
    
    wbm_aoi_cleaned <- wbm_aoi %>%
      furrr::future_map_dfr(~ rast_read(.))
    
  }
  
  
    return(wbm_aoi_cleaned)
    
    
  
}
