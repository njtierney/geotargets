targets::tar_test("geotargets_options_get() retrieves options in correct priority", {
  # options takes precedent over env var
  withr::with_envvar(
    c("GEOTARGETS_GDAL_RASTER_DRIVER" = "COG"),
    withr::with_options(list("geotargets.gdal.raster.driver" = "GIF"), {
      targets::tar_script({
        list(
          targets::tar_target(
            opt,
            geotargets::geotargets_option_get("gdal.raster.driver")
          )
        )
      })
      targets::tar_make()
      expect_identical(
        geotargets::geotargets_option_get("gdal.raster.driver"),
        "GIF"
      )
    })
  )
})


test_that("geotargets_option_set() works", {
  op <- getOption("geotargets.gdal.raster.driver")
  withr::defer(options("geotargets.gdal.raster.driver" = op))

  geotargets_option_set(gdal_raster_driver = "COG")
  expect_identical(getOption("geotargets.gdal.raster.driver"), "COG")
  expect_identical(geotargets_option_get("gdal.raster.driver"), "COG")
  expect_identical(geotargets_option_get("gdal_raster_driver"), "COG")
})

test_that("options aren't reset with multiple calls to geotargets_option_set()", {
  op_rast <- getOption("geotargets.gdal.raster.driver")
  withr::defer(options("geotargets.gdal.raster.driver" = op_rast))
  op_vect <- getOption("geotargets.gdal.vector.driver")
  withr::defer(options("geotargets.gdal.vector.driver" = op_vect))

  geotargets_option_set(gdal_raster_driver = "GPKG")
  geotargets_option_set(gdal_vector_driver = "GPKG")
  expect_identical(geotargets_option_get("gdal_vector_driver"), "GPKG")
  expect_identical(geotargets_option_get("gdal_raster_driver"), "GPKG")
})
