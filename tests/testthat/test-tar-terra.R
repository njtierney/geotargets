# test_that() #Included to make RStudio recognize this file as a test
targets::tar_test("tar_terra_rast() works", {
  targets::tar_script({
    list(
      geotargets::tar_terra_rast(
        test_terra_rast,
        terra::rast(system.file("ex/elev.tif", package = "terra"))
      )
    )
  })
  targets::tar_make()
  x <- targets::tar_read(test_terra_rast)
  expect_s4_class(x, "SpatRaster")
  expect_snapshot(
      x
  )
})

targets::tar_test("tar_terra_vect() works", {
  targets::tar_script({
    lux_area <- function(projection = "EPSG:4326") {
      terra::project(
        terra::vect(system.file("ex", "lux.shp",
          package = "terra"
        )),
        projection
      )
    }
    list(
      geotargets::tar_terra_vect(
        test_terra_vect,
        lux_area()
      ),
      geotargets::tar_terra_vect(
          test_terra_vect2,
          lux_area(),
          filetype = "ESRI Shapefile"
      )
    )
  })
  targets::tar_make()
  x <- targets::tar_read(test_terra_vect)
  y <- targets::tar_read(test_terra_vect2)
  expect_s4_class(x, "SpatVector")
  expect_s4_class(y, "SpatVector")
  expect_snapshot(x)
  expect_snapshot(y)
  expect_equivalent(x, y)
})
