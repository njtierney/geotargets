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

targets::tar_test("tar_terra_rast() works with `use_cache = TRUE`", {
    targets::tar_script({
        library(targets)
        library(geotargets)
        library(terra)
        rast_with_metadata <- function(file) {
            r <- terra::rast(file)
            terra::time(r) <- as.Date("2024-10-22")
            units(r) <- "m"
            metags(r) <- c(tag = "custom tag")
            r
        }

        list(
            tar_target(f, system.file("ex/elev.tif", package="terra"), format = "file"),
            tar_terra_rast(
                name = r,
                command = rast_with_metadata(f),
                filetype = "GTiff",
                use_cache = TRUE
            )
        )
    })
    targets::tar_make()
    expect_true(all(is.na(targets::tar_meta()$error)))
    expect_true(file.exists("_geotargets/r"))
    expect_true(file.exists("_geotargets/r.aux.json"))
    r <- targets::tar_read(r)
    expect_snapshot(r)
    #need to load terra for SpatRaster method for units()
    withr::with_package("terra", {
        expect_equal(units(r), "m")
    })
    expect_equal(terra::time(r), as.Date("2024-10-22"))
    # expect_equal(terra::metags(r)["tag"], "custom tag") #Doesn't work with wrapCache(), so won't work here. Possible `terra` bug.

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

tar_test("That changing filetype invalidates a target", {
    targets::tar_script({
        library(targets)
        library(geotargets)
        library(terra)

        list(
            tar_terra_rast(
                r,
                rast(system.file("ex/elev.tif", package="terra")),
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
                rast(system.file("ex/elev.tif", package="terra")),
                filetype = "GTiff"
            )
        )
    })
    expect_equal(tar_outdated(), "r")
})
