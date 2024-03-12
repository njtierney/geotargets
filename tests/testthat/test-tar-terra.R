# test_that() #Included to make RStudio recognize this file as a test
targets::tar_test("tar_terra_rast() works", {
    targets::tar_script({
        list(
           geotargets::tar_terra_rast(
                "test",
                system.file("ex/elev.tif", package="terra") |> terra::rast()
            )
        )
    })
    targets::tar_make()
    x <- targets::tar_read(test)
    expect_s4_class(x, "SpatRaster")
})
