
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geotargets <a href="http://geotargets.njtierney.com/"><img src="man/figures/logo.png" alt="geotargets website" align="right" height="139"/></a>

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R
Targetopia](https://img.shields.io/badge/R_Targetopia-member-blue?style=flat&labelColor=gray)](https://wlandau.github.io/targetopia/)
[![R-CMD-check](https://github.com/njtierney/geotargets/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/njtierney/geotargets/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/njtierney/geotargets/branch/master/graph/badge.svg)](https://app.codecov.io/gh/njtierney/geotargets?branch=master)
[![pkgcheck](https://github.com/njtierney/geotargets/workflows/pkgcheck/badge.svg)](https://github.com/njtierney/geotargets/actions?query=workflow%3Apkgcheck)
<!-- badges: end -->

`geotargets` extends [`targets`](https://github.com/ropensci/targets) to
work with geospatial data formats, such as rasters and vectors (e.g.,
shapefiles). Currently we support raster and vector formats for the
[`terra`](https://github.com/rspatial/terra) package

## Installation

You can install the development version of geotargets like so:

``` r
install.packages("geotargets", repos = c("https://njtierney.r-universe.dev", "https://cran.r-project.org"))
```

## Why `geotargets`

If you want to use geospatial data formats (such as `terra`) with the
[`targets`](https://github.com/ropensci/targets) package to build
analytic reproducible pipelines, it involves writing a lot of custom
targets wrappers. We wrote `geotargets` so you can use geospatial data
formats with `targets`.

To provide more detail on this, a common problem when using popular
libraries like `terra` with `targets` is running into errors with read
and write. Due to the limitations that come with the underlying C++
implementation in the `terra` library, there are specific ways to write
and read these objects. See `?terra` for details. `geotargets` helps
handle these write and read steps, so you don’t have to worry about them
and can use targets as you are used to.

In essence, if you’ve ever come across the error:

    Error in .External(list(name = "CppMethod__invoke_notvoid", address = <pointer: 0x0>,  : 
      NULL value passed as symbol address

or

    Error: external pointer is not valid

When trying to read in a geospatial raster or vector in targets, then
`geotargets` for you :)

# Examples

We currently provide support for the `terra` package with `targets`.
Below we show three examples of target factories:

- `tar_terra_rast()`
- `tar_terra_vect()`
- `tar_terra_sprc()`
- `tar_terra_sds()`
- `tar_tera_tiles()`
- `tar_stars()`

You would use these in place of `tar_target()` in your targets pipeline,
e.g., when you are doing work with `terra` raster, vector, or raster
collection data.

If you would like to see and download working examples for yourself, see
the repos:

- [demo-geotargets](https://github.com/njtierney/demo-geotargets)
- [icebreaker](https://github.com/njtierney/icebreaker)

## `tar_terra_rast()`: targets with terra rasters

``` r
library(targets)

tar_dir({ # tar_dir() runs code from a temporary directory.
  tar_script({
    library(geotargets)

    get_elev <- function() {
      terra::rast(system.file("ex", "elev.tif", package = "terra"))
    }

    list(
      tar_terra_rast(
        terra_rast_example,
        get_elev()
      )
    )
  })

  tar_make()
  x <- tar_read(terra_rast_example)
  x
})
#> ▶ dispatched target terra_rast_example
#> ● completed target terra_rast_example [0.008 seconds, 7.992 kilobytes]
#> ▶ ended pipeline [0.064 seconds]
#> class       : SpatRaster 
#> dimensions  : 90, 95, 1  (nrow, ncol, nlyr)
#> resolution  : 0.008333333, 0.008333333  (x, y)
#> extent      : 5.741667, 6.533333, 49.44167, 50.19167  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source      : terra_rast_example 
#> name        : elevation 
#> min value   :       141 
#> max value   :       547
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
  x <- tar_read(terra_vect_example)
  x
})
#> ▶ dispatched target terra_vect_example
#> ● completed target terra_vect_example [0.017 seconds, 117.622 kilobytes]
#> ▶ ended pipeline [0.054 seconds]
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 12, 6  (geometries, attributes)
#>  extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#>  source      : terra_vect_example
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#>  names       :  ID_1   NAME_1  ID_2   NAME_2  AREA   POP
#>  type        : <num>    <chr> <num>    <chr> <num> <int>
#>  values      :     1 Diekirch     1 Clervaux   312 18081
#>                    1 Diekirch     2 Diekirch   218 32543
#>                    1 Diekirch     3  Redange   259 18664
```

## `tar_terra_sprc()`: targets with terra raster collections

``` r
tar_dir({ # tar_dir() runs code from a temporary directory.
  tar_script({
    library(geotargets)

    elev_scale <- function(z = 1, projection = "EPSG:4326") {
      terra::project(
        terra::rast(system.file("ex", "elev.tif", package = "terra")) * z,
        projection
      )
    }

    list(
      tar_terra_sprc(
        raster_elevs,
        # two rasters, one unaltered, one scaled by factor of 2 and
        # reprojected to interrupted goode homolosine
        command = terra::sprc(list(
          elev_scale(1),
          elev_scale(2, "+proj=igh")
        ))
      )
    )
  })

  tar_make()
  x <- tar_read(raster_elevs)
  x
})
#> ▶ dispatched target raster_elevs
#> ● completed target raster_elevs [0.065 seconds, 36.423 kilobytes]
#> ▶ ended pipeline [0.119 seconds]
#> class       : SpatRasterCollection 
#> length      : 2 
#> nrow        : 90, 115 
#> ncol        : 95, 114 
#> nlyr        :  1,   1 
#> extent      : 5.741667, 1558890, 49.44167, 5556741  (xmin, xmax, ymin, ymax)
#> crs (first) : lon/lat WGS 84 (EPSG:4326) 
#> names       : raster_elevs, raster_elevs
```

## `tar_stars()`: targets with stars objects

``` r
tar_dir({ # tar_dir() runs code from a temporary directory.
  tar_script({
    library(geotargets)

    list(
      tar_stars(
        test_stars,
        stars::read_stars(system.file("tif", "olinda_dem_utm25s.tif", package = "stars"))
      )
    )
  })

  tar_make()
  x <- tar_read(test_stars)
  x
})
#> ▶ dispatched target test_stars
#> ● completed target test_stars [0.018 seconds, 49.9 kilobytes]
#> ▶ ended pipeline [0.059 seconds]
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>             Min. 1st Qu. Median     Mean 3rd Qu. Max.
#> test_stars    -1       6     12 21.66521      35   88
#> dimension(s):
#>   from  to  offset  delta                       refsys point x/y
#> x    1 111  288776  89.99 UTM Zone 25, Southern Hem... FALSE [x]
#> y    1 111 9120761 -89.99 UTM Zone 25, Southern Hem... FALSE [y]
```

## Code of Conduct

Please note that the geotargets project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Acknowledgements

Logo design by Hubert Hałun at Appsilon.
