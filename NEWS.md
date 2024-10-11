# geotargets (development version)

* Created `tar_stars()` and `tar_stars_proxy()` that create `stars` and `stars_proxy` objects, respectively.
* Created `tar_terra_tiles()`, a "target factory" for splitting a raster into multiple tiles with dynamic branching (#69).
* Created two helper functions for use in `tar_terra_tiles()`: `tile_grid()` and `tile_blocksize()` (#69, #87, #89).
* Created utility function `set_window()` mostly for internal use within `tar_terra_tiles()`.
* Removes the `iteration` argument from all `tar_*()` functions.  `iteration` now hard-coded as `"list"` since it is the only option that works (for now at least).
* Added the `description` argument to all `tar_*()` functions which is passed to `tar_target()`.
* Requires GDAL 3.1 or greater to use "ESRI Shapefile" driver in `tar_terra_vect()` (#71, #97)

# geotargets 0.1.0 (14 May 2024)

* Created `tar_terra_rast()` and `tar_terra_vect()` for targets that create `SpatRaster` and `SpatVector` objects, respectively
* Created `tar_terra_sprc()` that creates a `SpatRasterCollection` object.
* `geotargets_options_get()` and `geotargets_options_set()` can be used to set and get options specific to `geotargets`.
* `geotargets` now requires `targets` version 1.7.0 or higher
* fixed a bug where `resources` supplied to `tar_terra_*()` were being ignored (#66)
