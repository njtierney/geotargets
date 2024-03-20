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
