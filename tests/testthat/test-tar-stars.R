# test_that() #Included to make RStudio recognize this file as a test

skip_if_not_installed("sf")

skip_if_not_installed("stars")

targets::tar_test("tar_stars() works", {
  geotargets::geotargets_option_set("raster_gdal_creation_options", c("COMPRESS=DEFLATE", "TFW=YES"))
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
