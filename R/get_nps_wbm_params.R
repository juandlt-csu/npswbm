#' @title Site parameters for the water balance model
#'
#' @description
#' Gets the water balance model's site parameters (elevation, slope, aspect,
#' soil water capacity and Jennings threshold temperature) from the rasters
#' loaded by [load_wbm_rasters()]. If `dem`, `soil` and `jtemp` aren't in the
#' session yet, it calls `load_wbm_rasters(crs = terra::crs(points))` first,
#' which creates them in the global environment.
#'
#' @param points sf object. The model points, e.g. GridMET cell centers,
#'   with `x` and `y` columns (longitude and latitude) that match the climate
#'   data, as [run_NPS_WBM_points()] needs. Required.
#' @param area_weighted Logical. How the values are taken.
#'   * `FALSE`: the raster values at each point.
#'   * `TRUE`: area-weighted means over each `grid` cell that contains a point,
#'     clipped to `aoi`, with [terra::extract()] (`weights = TRUE, exact =
#'     TRUE`). Aspect is a weighted circular mean. Needs `aoi` and `grid`.
#'
#'   Default: `FALSE`.
#' @param aoi sf object or `NULL`. The area of interest polygon the grid
#'   cells are clipped to. Required when `area_weighted = TRUE` (stops with an
#'   error otherwise); ignored when `FALSE`. Default: `NULL`.
#' @param grid sf object or `NULL`. The grid cell polygons (e.g. the GridMET
#'   cells from `get_climate_historic(return_grid = TRUE)` in wavadeepdive),
#'   reprojected to the CRS of `points` if needed. Required when
#'   `area_weighted = TRUE` (stops with an error otherwise); ignored when
#'   `FALSE`. Default: `NULL`.
#'
#' @return A data.table with one row per point (or per grid cell piece when
#'   `area_weighted = TRUE`) and columns `Elev` (m), `Aspect` (degrees, 0-360),
#'   `Slope` (degrees), `SWC.Max` (mm) and `J.Temp` (degrees C), plus the
#'   columns of `points` (or of `grid` and `points` when area-weighted, with
#'   the cell pieces' `geometry`).
#' @export
#' @seealso [load_wbm_rasters()]; [nps_wbm()] and [run_NPS_WBM_points()],
#'   which use the parameters.
#' @examplesIf file.exists(wavacore::wava_data_path("all", "elevation_cropped.tif"))
#' pts <- data.frame(x = c(-112.98, -112.95), y = c(37.20, 37.25))
#' pts <- sf::st_as_sf(pts, coords = c("x", "y"), crs = 4326, remove = FALSE)
#' get_nps_wbm_params(pts)

get_nps_wbm_params <- function(points, area_weighted = FALSE, aoi = NULL, grid = NULL) {
  
  if (!(exists("dem") && exists("soil")  && exists("jtemp"))) {
    load_wbm_rasters(crs = terra::crs(points))
  }
  
  if(area_weighted == FALSE){
    
    # Extract params at each gridMET centroid point
    point_params <- points %>% 
      data.table::data.table() %>%
      dplyr::mutate(Elev = terra::extract(dem, points)$elevation_cropped,
                    Aspect = terra::extract(aspect, points)$aspect,
                    Slope = terra::extract(slope, points)$slope,
                    SWC.Max =  terra::extract(soil, points)$water_storage,
                    J.Temp = terra::extract(jtemp, points)$merged_jennings2_1)
    
    return(point_params)
    
  } else if(area_weighted == TRUE){
    
    if (is.null(grid)) {
      stop("A grid for `grid` argument must be provided when area_weighted = TRUE")
    }
    
    if (is.null(aoi)) {
      stop("A polygon for `aoi` argument must be provided when area_weighted = TRUE")
    }
    
    # check CRS:
    if (sf::st_crs(grid) != sf::st_crs(points)) {
      grid <- sf::st_transform(grid, crs = sf::st_crs(points))
    }
    
    # find which grid cells intersect with user-supplied GridMET points:
    grid_point_intersect <- grid %>%
      sf::st_join(., points, left = FALSE) %>%
      sf::st_intersection(., dplyr::select(aoi, geometry)) %>%
      dplyr::mutate(grid_id = dplyr::row_number())  
    
    # extract area-weighted means for non-aspect variables separately
    # (purposefully avoided stacking due to extent mismatches between layers)
    elev_extracts <- terra::extract(dem, 
                                    terra::vect(grid_point_intersect), 
                                    fun = "mean",
                                    weights = TRUE,
                                    exact = TRUE) %>%
      dplyr::select(ID, Elev = names(dem)[1])
    
    slope_extracts <- terra::extract(slope, 
                                     terra::vect(grid_point_intersect), 
                                     fun = "mean",
                                     weights = TRUE,
                                     exact = TRUE) %>% 
      dplyr::select(ID, Slope = names(slope)[1])
    
    soil_extracts <- terra::extract(soil, 
                                    terra::vect(grid_point_intersect), 
                                    fun = "mean",
                                    weights = TRUE,
                                    exact = TRUE) %>% 
      dplyr::select(ID, SWC.Max = names(soil)[1])
    
    jtemp_extracts <- terra::extract(jtemp, 
                                     terra::vect(grid_point_intersect), 
                                     fun = "mean",
                                     weights = TRUE,
                                     exact = TRUE) %>% 
      dplyr::select(ID, J.Temp = names(jtemp)[1])
    
    # handle aspect differently (since aspect is radial...)
    aspect_extracts <- terra::extract(aspect, 
                                      terra::vect(grid_point_intersect), 
                                      weights = TRUE,
                                      exact = TRUE)
    
    # get circular mean from degreees in raw aspect
    aspect_means <- aspect_extracts %>%
      data.table::data.table() %>%
      dplyr::group_by(ID) %>%
      dplyr::summarize(Aspect = {
        # degrees to radians
        angles_rad <- aspect * pi / 180  # Convert to radians
        
        # normalize weights
        weights_norm <- weight / sum(weight)  # Normalize weights
        
        # calculate weighted circular mean
        x_sum <- sum(cos(angles_rad) * weights_norm, na.rm = TRUE)
        y_sum <- sum(sin(angles_rad) * weights_norm, na.rm = TRUE)
        
        # convert back to degrees
        mean_angle <- atan2(y_sum, x_sum) * 180 / pi
        
        # ensure positive angle (0-360) a la Claude
        if (mean_angle < 0) mean_angle + 360 else mean_angle
      }
      )
    
    # combine all the extractions -
    point_grid_combo <- elev_extracts %>%
      data.table::data.table() %>%
      dplyr::left_join(slope_extracts, by = "ID") %>%
      dplyr::left_join(soil_extracts, by = "ID") %>%
      dplyr::left_join(aspect_means, by = "ID") %>%
      dplyr::left_join(jtemp_extracts, by = "ID") %>%
      # join back with original grid attributes
      cbind(grid_point_intersect %>% 
              sf::st_drop_geometry()) %>%
      dplyr::select(-c(grid_id, ID)) 
    
    return(point_grid_combo)
  }
}
