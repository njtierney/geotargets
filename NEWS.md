# geotargets (development version)

* Created `tar_stars()` and `tar_stars_proxy()` that create `stars` and `stars_proxy` objects, respectively.
* Added the `description` argument to all `tar_*()` functions which is passed to `tar_target()`

# geotargets 0.1.0 (14 May 2024)

* Created `tar_terra_rast()` and `tar_terra_vect()` for targets that create `SpatRaster` and `SpatVector` objects, respectively
* Created `tar_terra_sprc()` that creates a `SpatRasterCollection` object.
* `geotargets_options_get()` and `geotargets_options_set()` can be used to set and get options specific to `geotargets`.
* `geotargets` now requires `targets` version 1.7.0 or higher
* fixed a bug where `resources` supplied to `tar_terra_*()` were being ignored (#66)
