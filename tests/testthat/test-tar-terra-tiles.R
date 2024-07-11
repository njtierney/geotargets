# test_that() #Included to make RStudio recognize this file as a test
targets::tar_test("tar_terra_tiles() works", {
    targets::tar_script({
        library(targets)
        library(geotargets)
        library(terra)
        list(
            tar_target(
                my_file,
                system.file("ex/elev.tif", package="terra"),
                format = "file"
            ),
            tar_terra_rast(
                my_map,
                terra::rast(my_file)
            ),
            tar_terra_tiles(
                name = rast_split,
                raster = my_map,
                ncol = 2,
                nrow = 2
            )
        )
    })
    manifest <- targets::tar_manifest()
    #check that the command is correct
    expect_equal(manifest[manifest$name == "rast_split_exts", ][["command"]],
                 "create_tile_exts(my_map, ncol = 2, nrow = 2)")
    expect_equal(manifest[manifest$name == "rast_split",][["command"]],
                 "set_window(my_map, terra::ext(rast_split_exts))")
    targets::tar_make()
    expect_true(all(is.na(targets::tar_meta()$error)))
})

targets::tar_test("recombined tiles are equal to original", {
    targets::tar_script({
        library(targets)
        library(geotargets)
        library(terra)
        list(
            tar_target(
                my_file,
                system.file("ex/elev.tif", package="terra"),
                format = "file"
            ),
            tar_terra_rast(
                my_map,
                terra::rast(my_file)
            ),
            tar_terra_tiles(
                name = rast_split,
                raster = my_map,
                ncol = 2,
                nrow = 2
            )
        )
    })
    targets::tar_make()
    targets::tar_load(c(my_map, rast_split))
    recombined <- terra::merge(terra::sprc(rast_split))
    expect_equal(terra::values(my_map), terra::values(recombined))
})
