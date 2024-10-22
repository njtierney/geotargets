# tar_terra_rast() works

    Code
      x
    Output
      class       : SpatRaster 
      dimensions  : 90, 95, 1  (nrow, ncol, nlyr)
      resolution  : 0.008333333, 0.008333333  (x, y)
      extent      : 5.741667, 6.533333, 49.44167, 50.19167  (xmin, xmax, ymin, ymax)
      coord. ref. : lon/lat WGS 84 (EPSG:4326) 
      source      : test_terra_rast 
      name        : elevation 
      min value   :       141 
      max value   :       547 

# tar_terra_rast() works with `use_cache = TRUE`

    Code
      r
    Output
      class       : SpatRaster 
      dimensions  : 90, 95, 1  (nrow, ncol, nlyr)
      resolution  : 0.008333333, 0.008333333  (x, y)
      extent      : 5.741667, 6.533333, 49.44167, 50.19167  (xmin, xmax, ymin, ymax)
      coord. ref. : lon/lat WGS 84 (EPSG:4326) 
      source      : r 
      name        : elevation 
      min value   :       141 
      max value   :       547 
      unit        :         m 
      time (days) : 2024-10-22 

