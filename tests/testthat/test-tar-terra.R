# test_that() #Included to make RStudio recognize this file as a test
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

targets::tar_test("tar_terra_rast() works with multiple workers (tests marshaling/unmarshaling)", {
    targets::tar_script({
        targets::tar_option_set(controller = crew::crew_controller_local(workers = 2))
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
    expect_equal(terra::values(x), terra::values(y))
})

targets::tar_test("tar_terra_vect() works with multiple workers (tests marshaling/unmarshaling)", {
    targets::tar_script({
        targets::tar_option_set(controller = crew::crew_controller_local(workers = 2))
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

targets::tar_test("user resources are passed correctly", {
    library(crew)
    persistent <- crew::crew_controller_local(name = "persistent")
    transient  <- crew::crew_controller_local(name = "transient", tasks_max = 1L)
    targets::tar_option_set(
        controller = crew::crew_controller_group(persistent, transient),
        resources = tar_resources(
            crew = tar_resources_crew(controller = "transient")
        ))
    testthat::expect_equal(
        tar_terra_rast(x, 1)$settings$resources$crew,
        tar_resources_crew(controller = "transient")
    )
    testthat::expect_equal(
        tar_terra_rast(
            x, 1,
            resources = tar_resources(crew = tar_resources_crew(controller = "persistent"))
        )$settings$resources$crew,
        tar_resources_crew(controller = "persistent")
    )
    testthat::expect_equal(
        tar_terra_vect(
            x, 1,
            resources = tar_resources(crew = tar_resources_crew(controller = "persistent"))
        )$settings$resources$crew,
        tar_resources_crew(controller = "persistent")
    )
    testthat::expect_equal(
        tar_terra_sprc(
            x, 1,
            resources = tar_resources(crew = tar_resources_crew(controller = "persistent"))
        )$settings$resources$crew,
        tar_resources_crew(controller = "persistent")
    )
})

