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
                template = terra::rast(ncols = 2, nrows = 2, ext = ext(my_map)),
                tiles_dir = tempdir()
            )
        )
    })
    manifest <- targets::tar_manifest()
    #check that the command is correct
    expect_match(manifest[manifest$name == "rast_split_tile", ][["command"]], "terra::makeTiles\\(my_map, terra::rast\\(ncols = 2, nrows = 2")
    expect_equal(manifest[manifest$name == "rast_split_files",][["command"]], "rast_split_tile")
    expect_equal(manifest[manifest$name == "rast_split",][["command"]], "rast(rast_split_files)")
    targets::tar_make()
    expect_true(all(is.na(targets::tar_meta()$error)))
})
