test_that("tile_n fails with non integer", {
    f <- system.file("ex/elev.tif", package = "terra")
    r <- terra::rast(f)
    expect_snapshot(
        error = TRUE,
        tile_n(r, n = 3.14)
    )
    expect_snapshot(
        tile_n(r, n = 4)
    )
})
