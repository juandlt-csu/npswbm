#' @title Load the WBM site rasters into the global environment
#'
#' @description
#' Reads the rasters the water balance model needs for its site parameters
#' from the shared data folder and projects them to `crs`:
#' * `dem`: elevation (m), `all/elevation_cropped.tif`;
#' * `soil`: soil water storage capacity (mm, converted from cm),
#'   `all/water_storage.tif`;
#' * `slope` and `aspect` (degrees), calculated from `dem`;
#' * `jtemp`: the Jennings et al. (2018) rain-snow threshold temperature
#'   (degrees C), `all/merged_jennings2.tif`.
#'
#' The five rasters are assigned in the global environment (with `<<-`),
#' where [get_nps_wbm_params()] looks for them; existing objects with those
#' names are replaced. Reading and projecting takes a minute or so.
#'
#' @param crs String. The coordinate reference system to project the
#'   rasters to, in a form [terra::project()] accepts: `"epsg:4326"`, a WKT
#'   string (e.g. `terra::crs(points)`) or a PROJ string. A bare number such
#'   as `4326` stops with an error. Use the CRS of the points the model will
#'   run at. Rasters already in that CRS are not projected. Required.
#'
#' @return Called for its side effect: creates `dem`, `soil`, `slope`,
#'   `aspect` and `jtemp` (SpatRasters) in the global environment. The
#'   return value itself (invisible) is not meant to be used.
#' @export
#' @seealso [get_nps_wbm_params()], which extracts the parameters from these
#'   rasters.
#' @examplesIf file.exists(wavacore::wava_data_path("all", "elevation_cropped.tif"))
#' load_wbm_rasters(crs = "epsg:4326")
#' dem
#' jtemp

load_wbm_rasters <- function(crs) {
  
  # Load DEM (from NPS WBM group) -> in meters
  dem <<- terra::rast(wavacore::wava_data_path("all", "elevation_cropped.tif"))
  
  if(is.na(sf::st_crs(dem)) || crs != sf::st_crs(dem)){
    
    dem <<- dem %>%
      terra::project(crs)
    
  }
  
  # Load storage properties raster (from NPS WBM group)
  soil <<- terra::rast(wavacore::wava_data_path("all", "water_storage.tif")) * 10 #cm to mm
    
  if(is.na(sf::st_crs(soil)) || crs != sf::st_crs(soil)){
    
    soil <<- soil %>%
      terra::project(crs)
    
  }
  
  # Calculate slope & aspect
  # For slope, 4 better for "smooth" surfaces, 8 better for rough See:
  # https://www.rdocumentation.org/packages/raster/versions/3.1-5/topics/terrain
  slope <<- terra::terrain(dem, v = "slope", 
                           unit = "degrees", 
                           neighbors = 8) 
  
  aspect <<- terra::terrain(dem, 
                            v = "aspect", 
                            unit = "degrees")
  
  jtemp <<- terra::rast(wavacore::wava_data_path("all", "merged_jennings2.tif"))
  sf::st_crs(jtemp, 4326)
  
  
  if(is.na(sf::st_crs(jtemp)) || crs != sf::st_crs(jtemp)){
    
    jtemp <<- jtemp %>%
      terra::project(crs)
    
  }
  
  
}
