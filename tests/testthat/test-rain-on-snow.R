# Made-up data with values worked out by hand. The golden tests compare the same
# code with playground PR #165.

test_that("rain raises the melt factor on a melting day", {
  # The threshold is 1.5 - 3 = -1.5, so day 2 is 3.5 degrees above it and its
  # factor is 4 + 0.0125 * 10 = 4.125: melt = 14.4375, leaving 5.5625. Day 3 has
  # no rain and could melt 5.5 * 4 = 22, but only 5.5625 is left. Day 4 has none.
  melt <- get_melt_ros(tmean = c(-6, 2, 4, 5), jtemp = rep(1.5, 4), hock = 4,
                       snow = c(20, 0, 0, 0), rain = c(0, 10, 0, 2))
  expect_equal(melt, c(0, 14.4375, 5.5625, 0))
})

test_that("rain changes nothing on a day too cold to melt", {
  melt <- get_melt_ros(tmean = c(-6, -3), jtemp = rep(1.5, 2), hock = 4,
                       snow = c(20, 0), rain = c(30, 30), sp.0 = 10)
  expect_equal(melt, c(0, 0))
})

test_that("the first day uses the rain term and the initial snowpack", {
  # 1.5 degrees above the threshold, with a factor of 4 + 0.0125 * 8 = 4.1
  args <- list(tmean = c(0, -9), jtemp = rep(1.5, 2), hock = 4, snow = c(0, 0),
               rain = c(8, 0))
  expect_equal(do.call(get_melt_ros, c(args, sp.0 = 100)), c(6.15, 0))
  # never more than the snowpack
  expect_equal(do.call(get_melt_ros, c(args, sp.0 = 2)), c(2, 0))
  # no initial snowpack, no melt
  expect_equal(do.call(get_melt_ros, args), c(0, 0))
})

test_that("with no rain it returns what get_melt() returns", {
  tmean <- c(-6, -4, -1, 2, 5, 8)
  snow <- c(20, 15, 5, 0, 0, 0)
  jtemp <- rep(1.5, 6)
  expect_identical(
    get_melt_ros(tmean, jtemp, hock = 4, snow = snow, rain = rep(0, 6)),
    get_melt(tmean, jtemp, hock = 4, snow = snow)
  )
  expect_identical(
    get_melt_ros(tmean, jtemp, hock = 4, snow = snow, rain = rep(0, 6), sp.0 = 25),
    get_melt(tmean, jtemp, hock = 4, snow = snow, sp.0 = 25)
  )
})

# Six days: snow, then rain on the melting pack
ros_climate <- function() {
  data.table::data.table(
    x = -113, y = 37.2, GCM = "a",
    date = seq(as.Date("2021-03-01"), by = "day", length.out = 6),
    ppt_mm = c(40, 20, 0, 25.4, 0, 0),
    tmean_C = c(-8, -8, 1, 3, 2, 2)
  )
}

ros_params <- function() {
  data.table::data.table(x = -113, y = 37.2, Elev = 1300, Aspect = 180,
                         Slope = 10, SWC.Max = 150, J.Temp = 1.5)
}

wbm_var <- function(result, var) result$vals[result$vars == var]

test_that("nps_wbm() defaults to ROS = FALSE, which uses get_melt()", {
  expect_false(formals(nps_wbm)$ROS)
  default <- nps_wbm(ros_climate(), ros_params(), PET_Method = "Hamon")
  expect_identical(
    default,
    nps_wbm(ros_climate(), ros_params(), PET_Method = "Hamon", ROS = FALSE)
  )
  clim <- ros_climate()
  freeze <- get_freeze(1.5, clim$tmean_C)
  expect_equal(
    wbm_var(default, "MELT"),
    get_melt(clim$tmean_C, rep(1.5, 6), 4, get_snow(clim$ppt_mm, freeze), 0) / 25.4
  )
})

test_that("nps_wbm(ROS = TRUE) uses get_melt_ros() and the model's own rain", {
  clim <- ros_climate()
  freeze <- get_freeze(1.5, clim$tmean_C)
  rain <- get_rain(clim$ppt_mm, freeze)
  snow <- get_snow(clim$ppt_mm, freeze)
  # the test is only useful if there is rain on melting snow
  expect_gt(rain[4], 0)

  with_ros <- nps_wbm(clim, ros_params(), PET_Method = "Hamon", ROS = TRUE)
  without <- nps_wbm(clim, ros_params(), PET_Method = "Hamon")

  expect_equal(wbm_var(with_ros, "MELT"),
               get_melt_ros(clim$tmean_C, rep(1.5, 6), 4, snow, rain, 0) / 25.4)
  # day 4 melts more; rain and snow don't change, and day 3 has no rain
  expect_gt(wbm_var(with_ros, "MELT")[4], wbm_var(without, "MELT")[4])
  expect_equal(wbm_var(with_ros, "MELT")[3], wbm_var(without, "MELT")[3])
  expect_equal(wbm_var(with_ros, "RAIN"), wbm_var(without, "RAIN"))
  expect_equal(wbm_var(with_ros, "SNOW"), wbm_var(without, "SNOW"))
})

test_that("the initial snowpack reaches get_melt_ros()", {
  clim <- ros_climate()
  clim$ppt_mm <- 0
  clim$ppt_mm[3] <- 25.4
  result <- nps_wbm(clim, ros_params(), PET_Method = "Hamon", ROS = TRUE,
                    Snowpack.Init = 100)
  rain <- get_rain(clim$ppt_mm, get_freeze(1.5, clim$tmean_C))
  expect_equal(
    wbm_var(result, "MELT"),
    get_melt_ros(clim$tmean_C, rep(1.5, 6), 4, rep(0, 6), rain, 100) / 25.4
  )
  expect_gt(sum(wbm_var(result, "MELT")), 0)
})

test_that("T.Base still works by name after ROS was added before it", {
  expect_identical(names(formals(nps_wbm))[12:13], c("ROS", "T.Base"))
  # T.Base only reaches a column nps_wbm() doesn't return, so it can't change
  # the output
  expect_identical(
    nps_wbm(ros_climate(), ros_params(), PET_Method = "Hamon", T.Base = 5),
    nps_wbm(ros_climate(), ros_params(), PET_Method = "Hamon")
  )
})
