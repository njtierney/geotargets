# test_that() #Included to make RStudio recognize this file as a test

skip_if_not_installed("stars")

targets::tar_test("tar_stars() works", {
  geotargets::geotargets_option_set("gdal.raster.creation_options", c("COMPRESS=DEFLATE", "TFW=YES"))
  targets::tar_script({
    list(geotargets::tar_stars(
      test_stars,
      stars::read_stars(system.file("tif", "olinda_dem_utm25s.tif", package = "stars"))
    ))
  })
  targets::tar_make()
  x <- targets::tar_read(test_stars)
  expect_s3_class(x, "stars")
  expect_snapshot(
      x
  )
})


targets::tar_test("tar_stars_proxy() works", {
  geotargets::geotargets_option_set("gdal.raster.creation_options", c("COMPRESS=DEFLATE", "TFW=YES"))
  geotargets::geotargets_option_set("stars.proxy", TRUE) # needed for {covr} only
  targets::tar_script({
    list(geotargets::tar_stars_proxy(
      test_stars_proxy,
      stars::read_stars(system.file("tif", "olinda_dem_utm25s.tif", package = "stars"), proxy = TRUE)
    ))
  })
  targets::tar_make()
  x <- targets::tar_read(test_stars_proxy)
  expect_s3_class(x, "stars_proxy")
  expect_snapshot(
    x
  )
  geotargets::geotargets_option_set("stars.proxy", FALSE) # go back to default
})
