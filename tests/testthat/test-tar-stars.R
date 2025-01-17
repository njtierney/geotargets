# test_that() #Included to make RStudio recognize this file as a test # nolint

skip_if_not_installed("stars")

targets::tar_test("tar_stars() works", {
  geotargets::geotargets_option_set(
      gdal_raster_creation_options = c("COMPRESS=DEFLATE", "TFW=YES")
      )
  targets::tar_script({
    list(geotargets::tar_stars(
      test_stars,
      stars::read_stars(
          system.file("tif", "olinda_dem_utm25s.tif", package = "stars")
          )
    ))
  })
  targets::tar_make()
  x <- targets::tar_read(test_stars)
  expect_s3_class(x, "stars")
  expect_snapshot(
    x
  )
})

targets::tar_test("tar_stars_proxy() works", {
  geotargets::geotargets_option_set(
      gdal_raster_creation_options = c("COMPRESS=DEFLATE", "TFW=YES")
      )
  targets::tar_script({
    list(geotargets::tar_stars_proxy(
      test_stars_proxy,
      stars::read_stars(
          system.file("tif", "olinda_dem_utm25s.tif", package = "stars")
          )
    ))
  })
  targets::tar_make()
  x <- targets::tar_read(test_stars_proxy)
  expect_s3_class(x, "stars_proxy")
  expect_snapshot(
    x
  )
})


targets::tar_test("tar_stars(mdim=TRUE) works", {
  targets::tar_script({
    geotargets::geotargets_option_set(gdal_raster_driver = "netCDF")
    list(geotargets::tar_stars(test_stars_mdim,
      {
        set.seed(135)
        m <- matrix(runif(10), 2, 5)
        names(dim(m)) <- c("stations", "time")
        times <- as.Date("2022-05-01") + 1:5
        pts <- sf::st_as_sfc(c("POINT(0 1)", "POINT(3 5)"))
        s <- stars::st_as_stars(list(Precipitation = m)) |>
          stars::st_set_dimensions(1, values = pts) |>
          stars::st_set_dimensions(2, values = times)
      },
      mdim = TRUE
    ))
  })

  targets::tar_make()
  x <- targets::tar_read(test_stars_mdim)
  expect_s3_class(x, "stars")
  expect_snapshot(x)
})


targets::tar_test("tar_stars(mdim=TRUE, ncdf=TRUE) works", {
  targets::tar_script({
    list(geotargets::tar_stars(test_stars_mdim_ncdf,
      {
        set.seed(135)
        m <- matrix(runif(10), 2, 5)
        names(dim(m)) <- c("stations", "time")
        times <- as.Date("2022-05-01") + 1:5
        pts <- sf::st_as_sfc(c("POINT(0 1)", "POINT(3 5)"))
        s <- stars::st_as_stars(list(Precipitation = m)) |>
          stars::st_set_dimensions(1, values = pts) |>
          stars::st_set_dimensions(2, values = times)
        s
      },
      driver = "netCDF",
      mdim = TRUE,
      ncdf = TRUE
    ))
  })

  targets::tar_make()
  # warnings related to no CRS
  suppressWarnings({
    x <- targets::tar_read(test_stars_mdim_ncdf)
  })
  expect_s3_class(x, "stars")
  expect_snapshot(x)
})

targets::tar_test("tar_stars() works with dynamic branching", {
  targets::tar_script({
    list(
      geotargets::tar_stars(
        test_stars,
        stars::read_stars(
            system.file("tif", "olinda_dem_utm25s.tif", package = "stars")
            )
      ),
      targets::tar_target(
        to_add,
        c(1, 2)
      ),
      geotargets::tar_stars(
        test_stars_plus,
        test_stars + to_add,
        pattern = map(to_add)
      )
    )
  })
  targets::tar_make()
  expect_length(targets::tar_read(test_stars_plus), 2)
})
