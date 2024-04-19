
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geotargets

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R
Targetopia](https://img.shields.io/badge/R_Targetopia-member-blue?style=flat&labelColor=gray)](https://wlandau.github.io/targetopia/)
[![R-CMD-check](https://github.com/njtierney/geotargets/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/njtierney/geotargets/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/njtierney/geotargets/branch/master/graph/badge.svg)](https://app.codecov.io/gh/njtierney/geotargets?branch=master)
<!-- badges: end -->

`geotargets` extends targets to work with geospatial data formats, such
as rasters and vectors (e.g., shapefiles).

A relatively common gotcha moment when using popular libraries like
`terra` with targets is running into errors with read and write. Due to
the limitations that come with the underlying C++ implementation in the
`terra` library, there are specific ways to write and read these
objects. See `?terra` for details. `geotargets` helps handle these write
and read steps, so you don’t have to worry about them and can use
targets as you are used to.

In essence, if you’ve ever come across the error:

    Error in .External(list(name = "CppMethod__invoke_notvoid", address = <pointer: 0x0>,  : 
      NULL value passed as symbol address

or

    Error: external pointer is not valid

When trying to read in a geospatial raster or vector in targets, then
this is for you :)

## Installation

You can install the development version of geotargets like so:

``` r
install.packages("geotargets", repos = c("https://njtierney.r-universe.dev", "https://cran.r-project.org"))
```

## A note on development

`geotargets` is still undergoing development, and we would love for
people to use the package to kick the tyres. We are using it in our own
work, but want users to know that the API could change in subtle or
breaking ways.

# Examples

Below we show two examples of target factories:

- `tar_terra_rast()`
- `tar_terra_vect()`

You would use these in place of `tar_target()` in your targets pipeline,
when you are doing work with terra raster or terra vector data.

It is a bit tricky to implement targets workflows in a README, but if
you would like to see and download working examples for yourself, see
the repo,
[demo-geotargets](https://github.com/njtierney/demo-geotargets).

## `tar_terra_rast()`: targets with terra rasters

``` r
library(targets)
tar_dir({ # tar_dir() runs code from a temporary directory.
  tar_script({
    library(targets)
    library(geotargets)
    list(
      tar_terra_rast(
        terra_rast_example,
        system.file("ex/elev.tif", package = "terra") |> terra::rast()
      )
    )
  })
  tar_make()
  x <- tar_read(terra_rast_example)
  x
})
```

## `tar_terra_vect()`: targets with terra vectors

``` r
tar_dir({ # tar_dir() runs code from a temporary directory.
  tar_script({
    library(geotargets)
    lux_area <- function(projection = "EPSG:4326") {
      terra::project(
        terra::vect(system.file("ex", "lux.shp",
          package = "terra"
        )),
        projection
      )
    }
    list(
      tar_terra_vect(
        terra_vect_example,
        lux_area()
      )
    )
  })
  tar_make()
  x <- tar_read(terra_rast_example)
  x
})
```

## Code of Conduct

Please note that the geotargets project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
