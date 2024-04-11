targets::tar_test("geotargets_options_get() retrieves options in correct priority", {
    #options takes precedent over env var
    withr::with_envvar(
        c("GEOTARGETS_GDAL_RASTER_DRIVER" = "COG"),
        withr::with_options(list("geotargets.gdal.raster.driver" = "GIF"), {
            targets::tar_script({
                list(
                    targets::tar_target(
                        opt,
                        geotargets::geotargets_option_get("gdal.raster.driver")
                    )
                )
            })
            targets::tar_make()
            expect_equal(geotargets::geotargets_option_get("gdal.raster.driver"), "GIF")
        })

    )
})



# targets::tar_test("Options are distributed to workers", {
#     targets::tar_script({
#         Sys.setenv("GEOTARGETS_GDAL_RASTER_DRIVER" = "COG")
#         targets::tar_option_set(
#             controller = crew::crew_controller_local(workers = 2)
#         )
#         list(
#             targets::tar_target(
#                 opt,
#                 Sys.getenv("GEOTARGETS_GDAL_RASTER_DRIVER")
#             )
#         )
#     })
#     targets::tar_make()
#     expect_equal(targets::tar_read(opt), "COG")
# })
