targets::tar_test("tar_terra_rasts() works", {
  geotargets::geotargets_option_set(
    "raster_gdal_creation_options",
    c("COMPRESS=DEFLATE", "TFW=YES")
  )
  targets::tar_script({
    elev_scale <- function(z = 1, projection = "EPSG:4326") {
      terra::project(
        terra::rast(
          system.file(
            "ex",
            "elev.tif",
            package = "terra"
          )
        ) * z,
        projection
      )
    }
    list(
      geotargets::tar_terra_rasts(
        raster_elevs,
        # two rasters, one unaltered, one scaled by factor of 2 and
        # reprojected to interrupted good homolosine
        command = terra::sprc(list(
          elev_scale(1),
          elev_scale(2, "+proj=igh")
        ))
      )
    )
  })
  targets::tar_make()
  x <- targets::tar_read(raster_elevs)
  expect_s4_class(x, "SpatRasterCollection")
  expect_snapshot(
    x
  )
})
