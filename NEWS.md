# geotargets (development version)

* Created `tar_stars()` and `tar_stars_proxy()` that create `stars` and `stars_proxy` objects, respectively.
* Created `tar_terra_tiles()`, a "target factory" for splitting a raster into multiple tiles with dynamic branching.
* Created helper functions `set_window()` and `create_tile_exts()` mostly for use within `tar_terra_tiles()`.
* Added the `description` argument to all `tar_*()` functions which is passed to `tar_target()`
* Hard-codes the `iteration` argument for `tar_terra_rast()` and `tar_terra_vect()` to `"lists"` since the other options don't work.

# geotargets 0.1.0 (14 May 2024)

* Created `tar_terra_rast()` and `tar_terra_vect()` for targets that create `SpatRaster` and `SpatVector` objects, respectively
* Created `tar_terra_sprc()` that creates a `SpatRasterCollection` object.
* `geotargets_options_get()` and `geotargets_options_set()` can be used to set and get options specific to `geotargets`.
* `geotargets` now requires `targets` version 1.7.0 or higher
* fixed a bug where `resources` supplied to `tar_terra_*()` were being ignored (#66)
