# test_that() #Included to make RStudio recognize this file as a test # nolint
library(targets)
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

targets::tar_test(
    "tar_terra_rast() works with multiple workers (tests un/marshaling)", {
  targets::tar_script({
    targets::tar_option_set(
        controller = crew::crew_controller_local(workers = 2)
        )
    list(
      geotargets::tar_terra_rast(
        rast1,
        terra::rast(system.file("ex/elev.tif", package = "terra"))
      ),
      geotargets::tar_terra_rast(
        rast2,
        terra::rast(system.file("ex/elev.tif", package = "terra"))
      )
    )
  })
  targets::tar_make()
  expect_true(all(is.na(targets::tar_meta()$error)))
  expect_s4_class(targets::tar_read(rast1), "SpatRaster")
})

targets::tar_test("tar_terra_rast() works with dynamic branching", {
  targets::tar_script({
    list(
      targets::tar_target(
        to_add,
        c(1, 2)
      ),
      geotargets::tar_terra_rast(
        my_map,
        terra::rast(system.file("ex/elev.tif", package = "terra"))
      ),
      geotargets::tar_terra_rast(
        my_map_plus,
        my_map + to_add,
        pattern = to_add
      )
    )
  })
  targets::tar_make()
  expect_length(targets::tar_read(my_map_plus), 2)
})


targets::tar_test("user resources are passed correctly", {
  library(crew)
  persistent <- crew::crew_controller_local(name = "persistent")
  transient <- crew::crew_controller_local(name = "transient", tasks_max = 1L)
  targets::tar_option_set(
    controller = crew::crew_controller_group(persistent, transient),
    resources = tar_resources(
      crew = tar_resources_crew(controller = "transient")
    )
  )
  testthat::expect_equal(
    tar_terra_rast(x, 1)$settings$resources$crew,
    tar_resources_crew(controller = "transient")
  )
  testthat::expect_equal(
    tar_terra_rast(
      x, 1,
      resources = tar_resources(
          crew = tar_resources_crew(controller = "persistent")
          )
    )$settings$resources$crew,
    tar_resources_crew(controller = "persistent")
  )
  testthat::expect_equal(
    tar_terra_vect(
      x, 1,
      resources = tar_resources(
          crew = tar_resources_crew(controller = "persistent")
          )
    )$settings$resources$crew,
    tar_resources_crew(controller = "persistent")
  )
  testthat::expect_equal(
    tar_terra_sprc(
      x, 1,
      resources = tar_resources(
          crew = tar_resources_crew(controller = "persistent")
          )
    )$settings$resources$crew,
    tar_resources_crew(controller = "persistent")
  )
})

tar_test("That changing filetype invalidates a target", {
  targets::tar_script({
    library(targets) # nolint
    library(geotargets)
    library(terra)

    list(
      tar_terra_rast(
        r,
        rast(system.file("ex/elev.tif", package = "terra")),
        filetype = "COG"
      )
    )
  })
  tar_make()

  targets::tar_script({
    library(targets)
    library(geotargets)
    library(terra)

    list(
      tar_terra_rast(
        r,
        rast(system.file("ex/elev.tif", package = "terra")),
        filetype = "GTiff"
      )
    )
  })
  expect_identical(tar_outdated(), "r")
})

tar_test("metadata is maintained for GTiff", {
  tar_script({
    library(targets)
    library(geotargets)
    library(terra)
    geotargets_option_set(
        terra_preserve_metadata = "zip",
        gdal_raster_driver = "GTiff" # default
        )
    make_rast <- function() {
      f <- system.file("ex/elev.tif", package = "terra")
      r <- terra::rast(f)
      r <- c(r, r + 10, r / 2)
      terra::units(r) <- rep("m", 3)
      terra::time(r) <- as.Date("2024-10-01") + c(0, 1, 2)
      r
    }
    list(
      tar_terra_rast(r, make_rast()),
      tar_terra_rast(r2, make_rast(), preserve_metadata = "drop")
    )
  })
  tar_make()
  x <- tar_read(r)
  expect_identical(terra::units(x), rep("m", 3))
  expect_identical(terra::time(x), as.Date("2024-10-01") + c(0, 1, 2))
  expect_identical(head(terra::values(x)), head(terra::values(tar_read(r2))))
})

tar_test("metadata is maintained for COG", {
  tar_script({
    library(targets)
    library(geotargets)
    library(terra)
    geotargets_option_set(
        terra_preserve_metadata = "zip",
        gdal_raster_driver = "COG"
        )
    make_rast <- function() {
      f <- system.file("ex/elev.tif", package = "terra")
      r <- terra::rast(f)
      r <- c(r, r + 10, r / 2)
      terra::units(r) <- rep("m", 3)
      terra::time(r) <- as.Date("2024-10-01") + c(0, 1, 2)
      r
    }
    list(
      tar_terra_rast(r, make_rast()),
      tar_terra_rast(r2, make_rast(), preserve_metadata = "drop")
    )
  })
  tar_make()
  x <- tar_read(r)
  expect_identical(terra::units(x), rep("m", 3))
  expect_identical(terra::time(x), as.Date("2024-10-01") + c(0, 1, 2))
  expect_identical(head(terra::values(x)), head(terra::values(tar_read(r2))))
})
