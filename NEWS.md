# geotargets (development version)

* bugfix by @brownag that fixes use of `file.rename()` in `tar_terra_rast(..., preserve_metadata = "zip")`, which does not work when the temporary directory is on a different partition. (#121, PR #122).
* Fixed examples for `tar_terra_tiles()`, `tar_terra_sds()`, and `tar_terra_sprc()` as reported by @amart90 as part of [rOpenSci review](https://github.com/ropensci/software-review/issues/675)

# geotargets 0.2.0 (29 November 2024)

* Created `tar_stars()` and `tar_stars_proxy()` that create `stars` and `stars_proxy` objects, respectively.
* Created `tar_terra_tiles()`, a "target factory" for splitting a raster into multiple tiles with dynamic branching (#69).
* Created two helper functions for use in `tar_terra_tiles()`: `tile_grid()`, `tile_n()`, and `tile_blocksize()` (#69, #86, #87, #89).
* Created utility function `set_window()` mostly for internal use within `tar_terra_tiles()`.
* Removes the `iteration` argument from all `tar_*()` functions.  `iteration` now hard-coded as `"list"` since it is the only option that works (for now at least).
* Added the `description` argument to all `tar_*()` functions which is passed to `tar_target()`.
* Suppressed the warning "[rast] skipped sub-datasets" from `tar_terra_sprc()`, which is misleading in this context (#92, #104).
* Requires GDAL 3.1 or greater to use "ESRI Shapefile" driver in `tar_terra_vect()` (#71, #97)
* `geotargets` now requires `targets` version 1.8.0 or higher
* `tar_terra_rast()` gains a `preserve_metadata` option that when set to `"zip"` reads/writes targets as zip archives that include aux.json "sidecar" files sometimes written by `terra` (#58)
* `terra` (>= 1.7.71), `withr` (>= 3.0.0), and `zip` are now required dependencies of `geotargets` (moved from `Suggests` to `Imports`)

# geotargets 0.1.0 (14 May 2024)

* Created `tar_terra_rast()` and `tar_terra_vect()` for targets that create `SpatRaster` and `SpatVector` objects, respectively
* Created `tar_terra_sprc()` that creates a `SpatRasterCollection` object.
* `geotargets_options_get()` and `geotargets_options_set()` can be used to set and get options specific to `geotargets`.
* `geotargets` now requires `targets` version 1.7.0 or higher
* fixed a bug where `resources` supplied to `tar_terra_*()` were being ignored (#66)
