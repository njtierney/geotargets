# test_that() # nolint
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
        test_terra_vect_shz,
        lux_area(),
        filetype = "ESRI Shapefile"
      )
    )
  })
  targets::tar_make()
  x <- targets::tar_read(test_terra_vect)
  y <- targets::tar_read(test_terra_vect_shz)
  expect_s4_class(x, "SpatVector")
  expect_s4_class(y, "SpatVector")
  expect_snapshot(x)
  expect_snapshot(y)
  expect_identical(terra::values(x), terra::values(y))
})


targets::tar_test("tar_terra_vect() works with dynamic branching", {
  targets::tar_script({
    list(
      geotargets::tar_terra_vect(
        my_vect,
        terra::vect(system.file("ex", "lux.shp", package = "terra"))
      ),
      targets::tar_target(
        to_sub,
        c("Clervaux", "Redange")
      ),
      geotargets::tar_terra_vect(
        my_vect_subs,
        my_vect[my_vect$NAME_2 == to_sub],
        pattern = to_sub
      )
    )
  })
  targets::tar_make()
  expect_length(targets::tar_read(my_vect_subs), 2)
})

targets::tar_test(
    "tar_terra_vect() works with multiple workers (tests un/marshaling)", {
  targets::tar_script({
    targets::tar_option_set(
        controller = crew::crew_controller_local(workers = 2)
        )
    list(
      geotargets::tar_terra_vect(
        vect1,
        terra::vect(system.file("ex", "lux.shp", package = "terra"))
      ),
      geotargets::tar_terra_vect(
        vect2,
        terra::vect(system.file("ex", "lux.shp", package = "terra"))
      )
    )
  })
  targets::tar_make()
  expect_true(all(is.na(targets::tar_meta()$error)))
  expect_s4_class(targets::tar_read(vect1), "SpatVector")
})
