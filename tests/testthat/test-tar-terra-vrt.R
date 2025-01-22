targets::tar_test("tar_terra_vrt() works", {

    targets::tar_script({
        library(targets)
        library(geotargets)
        list(
            tar_terra_rast(r, terra::rast(system.file("ex", "elev.tif", package = "terra"))),
            tar_terra_rast(r2, r * 2),
            tar_terra_tiles(
                rt, c(r, r2),
                function(x) tile_grid(x, ncol = 2, nrow = 2)
            ),
            tar_terra_vrt(r3, rt)
        )
    })

    targets::tar_make()

    expect_true(all(is.na(targets::tar_meta()$error)))

    # check VRT output
    vrt_xml <- readLines(targets::tar_path_target(r3))

    # 4 tiles, 2 bands each = 8 sources from target store
    expect_true(length(grep("<VRTRasterBand", vrt_xml)) == 2 &&
                    length(grep("<ComplexSource", vrt_xml)) == 8 &&
                    length(grep(targets::tar_path_store(), vrt_xml)) == 8)

})
