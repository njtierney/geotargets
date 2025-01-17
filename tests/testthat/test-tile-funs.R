test_that("tile_n fails with non integer", {
    f <- system.file("ex/elev.tif", package = "terra")
    r <- terra::rast(f)
    expect_snapshot(
        error = TRUE,
        tile_n(r, n = 3.14)
    )
    skip_on_ci()
    expect_snapshot(
        tile_n(r, n = 4)
    )
})

# expected number of tiles are being output
test_that("tile_n gives the appropriate number of outputs", {
    f <- system.file("ex/elev.tif", package = "terra")
    r <- terra::rast(f)
    skip_on_ci()
    tiled_1 <- tile_n(r, n = 1)
    tiled_2 <- tile_n(r, n = 2)
    tiled_3 <- tile_n(r, n = 3)
    tiled_4 <- tile_n(r, n = 4)
    tiled_5 <- tile_n(r, n = 5)

    expect_length(tiled_1, 1)
    expect_length(tiled_2, 2)
    expect_length(tiled_3, 3)
    expect_length(tiled_4, 4)
    expect_length(tiled_5, 6)
})

# sum of number of cells is equal to number of cells in input raster

# In `test-tile-funs.R` add tests for `tile_blocksize` and `tile_grid`.
# Additional tests could include that the expected number of tiles are being
# output and the sum of the number of cells is equal to the number of cells
# in the input raster.

test_that("tile_grid fails with non integer", {
    f <- system.file("ex/elev.tif", package = "terra")
    r <- terra::rast(f)
    expect_snapshot(
        error = TRUE,
        tile_grid(r,
                  ncol = 1.5,
                  nrow = 2.5)
    )
    expect_snapshot(
        error = TRUE,
        tile_grid(r,
                  ncol = 1,
                  nrow = 2.5)
    )
    expect_snapshot(
        error = TRUE,
        tile_grid(r,
                  ncol = 1.5,
                  nrow = 2)
    )
    skip_on_ci()
    expect_snapshot(
        tile_grid(r,
                  ncol = 2,
                  nrow = 3)
    )
})
