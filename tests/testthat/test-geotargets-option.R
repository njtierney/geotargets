# test_that("geotargets_options_get() retrieves options in correct priority", {
#     withr::with_envvar(
#
#     )
# })

targets::tar_test("options are distributed to workers", {
    targets::tar_script({
        options("geotargets.gdal.raster.driver" = "GeoJSON")
        targets::tar_option_set(
            controller = crew::crew_controller_local(workers = 2)
        )
        list(
            targets::tar_target(
                opt,
                getOption("geotargets.gdal.raster.driver")
            )
        )
    })
    targets::tar_make()
    expect_equal(targets::tar_read(opt), "GeoJSON")
})

targets::tar_test("env vars are distributed to workers", {
    targets::tar_script({
        Sys.setenv("GEOTARGETS_GDAL_RASTER_DRIVER" = "GeoJSON")
        targets::tar_option_set(
            controller = crew::crew_controller_local(workers = 2)
        )
        list(
            targets::tar_target(
                opt,
                Sys.getenv("GEOTARGETS_GDAL_RASTER_DRIVER")
            )
        )
    })
    targets::tar_make()
    expect_equal(targets::tar_read(opt), "GeoJSON")
})
