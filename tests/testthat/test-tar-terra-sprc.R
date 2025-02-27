# test_that() #Included to make RStudio recognize this file as a test # nolint
targets::tar_test("tar_terra_sprc() works", {
  geotargets::geotargets_option_set(
    gdal_raster_creation_options = c("COMPRESS=DEFLATE", "TFW=YES")
  )
  targets::tar_script({
    elev_scale <- function(z = 1, projection = "EPSG:4326") {
      rast_elev_scale <- terra::project(
        terra::rast(
          system.file(
            "ex",
            "elev.tif",
            package = "terra"
          )
        ) *
          z,
        projection
      )
      # test to ensure metadata comes along.
      # e.g., layer names, units, time, variables
      # The snapshots for these tests do not include those metadata
      # so this may be as easy as assigning some of these attributes in the
      # test SPRC/SDS and adding those fields in the snapshot.
      terra::units(rast_elev_scale) <- "m"
      terra::time(rast_elev_scale) <- as.Date("2025-01-15")
      rast_elev_scale
    }
    list(
      geotargets::tar_terra_sprc(
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
  expect_snapshot(x)
  expect_snapshot(x[1])
  expect_snapshot(x[2])
})

targets::tar_test("tar_terra_sds() works", {
  geotargets::geotargets_option_set(
    gdal_raster_creation_options = c("COMPRESS=DEFLATE", "TFW=YES")
  )
  targets::tar_script({
    elev_scale <- function(z = 1) {
      rast_elev_scale <- terra::rast(
        system.file(
          "ex",
          "elev.tif",
          package = "terra"
        )
      ) *
        z

      # test to ensure metadata comes along.
      # e.g., layer names, units, time, variables
      # The snapshots for these tests do not include those metadata
      # so this may be as easy as assigning some of these attributes in the
      # test SPRC/SDS and adding those fields in the snapshot.
      terra::units(rast_elev_scale) <- "m"
      terra::varnames(rast_elev_scale) <- "elev"
      terra::longnames(rast_elev_scale) <- "really-long-name"
      terra::time(rast_elev_scale) <- as.Date("2025-01-15")
      rast_elev_scale
    }
    list(
      geotargets::tar_terra_sds(
        raster_elevs,
        # two rasters, one unaltered, one scaled by factor of 2
        command = terra::sds(list(
          elev_scale(1),
          elev_scale(2)
        ))
      )
    )
  })
  targets::tar_make()
  x <- targets::tar_read(raster_elevs)
  expect_s4_class(x, "SpatRasterDataset")
  expect_snapshot(x)
  expect_snapshot(x[1])
  expect_identical(terra::units(x[1]), "m")
  expect_identical(terra::time(x[1]), as.Date("2025-01-15"))
  expect_snapshot(x[2])
  expect_identical(terra::units(x[2]), "m")
  expect_identical(terra::time(x[2]), as.Date("2025-01-15"))
})

# difficult to test for this warning from tar_terra_sprc() because it doesn't
# end up in tar_meta() in current version of `targets`.  Added in dev version:
# https://github.com/ropensci/targets/discussions/1345
# Once this is released, this test can be replaced with a targets pipeline and a
# check on `tar_meta(target, warnings)`

test_that("wrapped write function doesn't print warning", {
  f <- system.file("ex/elev.tif", package = "terra")
  r <- terra::rast(f)
  object <- terra::sprc(r, terra::project(r, "+proj=igh"))
  path <- withr::local_tempfile()
  terra::writeRaster(
    x = object[1],
    filename = path,
    filetype = "GTiff",
    overwrite = TRUE,
    gdal = ""
  )

  expect_warning(
    terra::writeRaster(
      x = object[2],
      filename = path,
      filetype = "GTiff",
      overwrite = FALSE,
      gdal = "APPEND_SUBDATASET=YES"
    )
  )

  expect_no_warning(
    withCallingHandlers(
      warning = function(cnd) {
        if (grepl("\\[rast\\] skipped sub-datasets", cnd$message)) {
          rlang::cnd_muffle(cnd)
        }
      },
      terra::writeRaster(
        x = object[2],
        filename = path,
        filetype = "GTiff",
        overwrite = FALSE,
        gdal = "APPEND_SUBDATASET=YES"
      )
    )
  )

  # other warnings should still make it through
  expect_warning(
    withCallingHandlers(
      warning = function(cnd) {
        if (grepl("\\[rast\\] skipped sub-datasets", cnd$message)) {
          rlang::cnd_muffle(cnd)
        }
      },
      terra::writeRaster(
        x = object[1],
        filename = withr::local_tempfile(fileext = ".nc")
      )
    ),
    "consider writeCDF to write ncdf files"
  )
})
