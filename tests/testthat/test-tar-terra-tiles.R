# test_that() #Included to make RStudio recognize this file as a test
targets::tar_test("tar_terra_tiles() works", {
    targets::tar_script({
        library(targets)
        library(geotargets)
        library(terra)
        list(
            tar_target(
                my_file,
                # system.file("ex/elev.tif", package="terra"),
                system.file("ex/logo.tif", package = "terra"),
                format = "file"
            ),
            tar_terra_rast(
                my_map,
                terra::rast(my_file)
            ),
            tar_terra_tiles(
                name = rast_split,
                raster = my_map,
                tile_fun = tile_blocksize
            ),
            tar_terra_tiles(
                name = rast_split_grid,
                raster = my_map,
                tile_fun = \(x) tile_grid(x, ncol = 2, nrow = 2)
            ),
            tar_terra_tiles(
                name = rast_split_n,
                raster = my_map,
                tile_fun = \(x) tile_n(x, n = 6)
            )
        )
    })
    manifest <- targets::tar_manifest()
    #check that the command is correct
    expect_equal(manifest[manifest$name == "rast_split_exts", ][["command"]],
                 "tile_blocksize(my_map)")
    # expect_equal(manifest[manifest$name == "rast_split_n_exts", ][["command"]],
    #              "tile_grid(my_map, ncol = 2, nrow = 2)") #TODO: haven't figured out how to make the manifest look nice with anonymous funs yet
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
                system.file("ex/elev.tif", package = "terra"),
                format = "file"
            ),
            tar_terra_rast(
                my_map,
                #create multi-layer raster for testing
                rast(c(my_file, my_file))
            ),
            tar_terra_tiles(
                name = rast_split,
                raster = my_map,
                tile_fun = tile_blocksize
            )
        )
    })
    targets::tar_make()
    targets::tar_load(c(my_map, rast_split))
    recombined <- terra::merge(terra::sprc(rast_split))
    expect_equal(terra::values(my_map),
                 terra::values(recombined))
})
