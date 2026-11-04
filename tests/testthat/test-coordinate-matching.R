# run_nps_wbm_points() with param_list, so it needs no rasters. The golden tests
# compare the same code with playground PR #165.

match_params <- function() {
  rbind(
    data.table::data.table(x = -113, y = 37.2, Elev = 1300, Aspect = 180,
                           Slope = 10, SWC.Max = 150, J.Temp = 1.5,
                           final_soil = 0, final_snowpack = 0),
    data.table::data.table(x = -112.9, y = 37.3, Elev = 1400, Aspect = 90,
                           Slope = 5, SWC.Max = 100, J.Temp = 1.5,
                           final_soil = 0, final_snowpack = 0)
  )
}

match_climate <- function(x, y, GCM = "a", warmer = 0) {
  date <- seq(as.Date("2021-01-01"), as.Date("2021-03-31"), by = "day")
  i <- seq_along(date)
  data.table::data.table(x = x, y = y, GCM = GCM, date = date,
                         ppt_mm = (i %% 5) * 2, tmean_C = -5 + i / 6 + warmer)
}

run_points <- function(climate, ...) {
  utils::capture.output(
    result <- run_nps_wbm_points(
      points = NULL, climate_data = climate, PET_Method = "Hamon",
      use_final = TRUE, param_list = match_params(), ...
    )
  )
  result
}

exact_climate <- function() {
  rbind(match_climate(-113, 37.2), match_climate(-112.9, 37.3, warmer = 1))
}

test_that("coordinates that differ past 6 decimals still match", {
  noisy <- exact_climate()
  noisy$x <- noisy$x + 1e-9
  noisy$y <- noisy$y - 1e-9
  # Not identical: the model takes the latitude from climate_data, so the noise
  # reaches the day length and PET, at around 1e-13.
  expect_equal(run_points(noisy), run_points(exact_climate()))
})

test_that("each point gets its own climate, not its neighbour's", {
  result <- run_points(exact_climate(), aggregate = FALSE)
  first <- result[result$x == -113, ]
  second <- result[result$x == -112.9, ]
  expect_equal(nrow(first), 90)
  expect_equal(nrow(second), 90)
  expect_equal(second$tmean_C - first$tmean_C, rep(1, 90))
})

test_that("coordinates that differ at 6 decimals don't match", {
  climate <- rbind(match_climate(-113 + 1e-5, 37.2),
                   match_climate(-112.9, 37.3))
  expect_error(run_points(climate), "no climate_data rows found for point 1")
})

test_that("a missing point names the point, its coordinates and the GCM", {
  expect_error(
    run_points(match_climate(-112.9, 37.3)),
    "run_nps_wbm_points: no climate_data rows found for point 1 \\(x = -113, y = 37.2\\) and GCM = 'a'"
  )
})

test_that("a point missing from one GCM stops, naming that GCM", {
  climate <- rbind(exact_climate(), match_climate(-113, 37.2, GCM = "b"))
  expect_error(run_points(climate),
               "point 2 \\(x = -112.9, y = 37.3\\) and GCM = 'b'")
})
