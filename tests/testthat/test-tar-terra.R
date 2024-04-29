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

targets::tar_test("tar_terra_rast(zipfile=TRUE) works", {
    targets::tar_script({
        list(
            geotargets::tar_terra_rast(
                test_terra_rast2,
                terra::rast(system.file("ex/elev.tif", package = "terra")),
                gdal = c("STREAMABLE_OUTPUT=YES", "COMPRESS=NONE"),
                zipfile = TRUE
            ),
            geotargets::tar_terra_rast(
                test_terra_rast3,
                terra::rast(system.file("ex/elev.tif", package = "terra")),
                filetype = "GPKG",
                zipfile = TRUE
            )
         )
    })
    targets::tar_make()
    expect_true(all(is.na(targets::tar_meta()$error)))
    x <- targets::tar_read(test_terra_rast2)
    expect_s4_class(x, "SpatRaster")
    expect_snapshot(x)
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
            ),
            geotargets::tar_terra_vect(
                test_terra_vect_parquet_zip,
                lux_area(),
                filetype = "Parquet",
                zipfile = TRUE
            )
        )
    })
    targets::tar_make()
    x <- targets::tar_read(test_terra_vect)
    y <- targets::tar_read(test_terra_vect_shz)
    z <- targets::tar_read(test_terra_vect_parquet_zip)
    expect_s4_class(x, "SpatVector")
    expect_s4_class(y, "SpatVector")
    expect_s4_class(z, "SpatVector")
    expect_snapshot(x)
    expect_snapshot(y)
    expect_snapshot(z)
    expect_equal(terra::values(x), terra::values(y))
    expect_equal(terra::values(y), terra::values(z))
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

