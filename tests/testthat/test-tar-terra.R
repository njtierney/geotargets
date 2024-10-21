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

targets::tar_test("tar_terra_rast() works with dynamic branching", {
    targets::tar_script({
        list(
            targets::tar_target(
                to_add,
                c(1,2)
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

targets::tar_test("tar_terra_rast_wrap works", {
    targets::tar_script({

        make_rast1 <- function() {
            x <- terra::rast(system.file("ex/elev.tif", package = "terra"))
            terra::units(x) <- "m"
            terra::varnames(x) <- "elev"
            x
        }

        make_rast2 <- function() {
            x <- terra::rast(system.file("ex/elev.tif", package = "terra"))
            y <- terra::classify(x, cbind(c(0, 300, 500),
                                          c(300, 500, 1000),
                                          1:3))
            levels(y) <- data.frame(value = 1:3,
                                    category = c("low", "med", "hi"))
            y
        }

        list(
            geotargets::tar_terra_rast_wrap(
                rast1,
                make_rast1(),
                filetype = "GPKG"
            ),
            geotargets::tar_terra_rast_wrap(
                rast2,
                make_rast2(),
                filetype = "GTiff"
            )
        )
    })

    targets::tar_make()

    x <- targets::tar_read(rast1)
    y <- targets::tar_read(rast2)
    z <- targets::tar_read(rast1_cache_files)
    x_raw <- readRDS("_targets/objects/rast1")

    expect_s4_class(terra::rast(z[1]), "SpatRaster")

    expect_snapshot(x)
    expect_snapshot(y)
    expect_snapshot(z)

    expect_true("units" %in% names(x_raw@attributes))
    expect_equal(terra::units(x), "m")
    expect_true(!is.null(terra::levels(y)))
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

targets::tar_test("tar_terra_vect() works with dynamic branching", {
    targets::tar_script({
        list(
            geotargets::tar_terra_vect(
                my_vect,
                terra::vect(system.file("ex", "lux.shp",package = "terra"))
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
