test_that("the model entry points and pieces are exported", {
  exports <- getNamespaceExports("npswbm")
  expected <- c("run_nps_wbm_points", "nps_wbm", "get_nps_wbm_params",
                "load_wbm_rasters", "runoff_routing_gw",
                "runoff_routing_snow_fun", "test_model_fit", "get_aet",
                "get_melt_ros", "get_snowpack", "get_storage", "et_hamon_daily",
                "get_wbm_aoi", "get_wbm_grids")
  expect_true(all(expected %in% exports))
  # helpers only the model itself calls stay internal
  expect_false("get_svp" %in% exports)
  expect_false("vapor_curve" %in% exports)
})

test_that("data-puller defaults sit under the shared data folder", {
  withr::local_options(wava.data_dir = NULL)
  withr::local_envvar(WAVA_DATA_DIR = NA)
  expect_equal(eval(formals(get_wbm_aoi)$path), "data/park/")
  expect_equal(eval(formals(get_30_year_wbm_gridmet)$path), "data/all")
  withr::local_options(wava.data_dir = "/shared")
  expect_equal(eval(formals(get_wbm_aoi)$path), "/shared/park/")
})

test_that("physical helpers move in the right direction", {
  expect_lt(atm_press(3000), atm_press(0))
  expect_gt(get_svp(25), get_svp(5))
})
