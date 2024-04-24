# test_that() #Included to make RStudio recognize this file as a test
targets::tar_test("tar_terra_rast() works", {
    # geotargets::geotargets_option_set(gdal_raster_creation_options = c("COMPRESS=DEFLATE", "TFW=YES"))
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

targets::tar_test("tar_terra_rast() works with multiple workers", {
    targets::tar_script({
        targets::tar_option_set(controller = crew::crew_controller_local(workers = 2))
        list(
            geotargets::tar_terra_rast(
                rast_raw,
                terra::rast(system.file("ex/elev.tif", package = "terra"))
            ),
            geotargets::tar_terra_rast(
                rast_plus,
                rast_raw + 1
            ),
            geotargets::tar_terra_rast(
                rast_minus,
                rast_raw - 1
            ),
            geotargets::tar_terra_rast(
                combined,
                rast(unname(list(rast_plus, rast_minus)))
            )
        )
    })
    targets::tar_make()
    expect_true(all(is.na(tar_meta()$error)))
    expect_snapshot(tar_read(combined))
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

targets::tar_test("tar_terra_vect() works with multiple workers", {
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
    expect_s4_class(targets::tar_read(vect1), "SpatVector")
    expect_s4_class(targets::tar_read(vect2), "SpatVector")
})

