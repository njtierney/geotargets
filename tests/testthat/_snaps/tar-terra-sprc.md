# tar_terra_sprc() works

    Code
      x
    Output
      class       : SpatRasterCollection 
      length      : 2 
      nrow        : 90, 115 
      ncol        : 95, 114 
      nlyr        :  1,   1 
      extent      : 5.741667, 1558890, 49.44167, 5556741  (xmin, xmax, ymin, ymax)
      crs (first) : lon/lat WGS 84 (EPSG:4326) 
      names       : raster_elevs, raster_elevs 

---

    Code
      x[1]
    Output
      class       : SpatRaster 
      dimensions  : 90, 95, 1  (nrow, ncol, nlyr)
      resolution  : 0.008333333, 0.008333333  (x, y)
      extent      : 5.741667, 6.533333, 49.44167, 50.19167  (xmin, xmax, ymin, ymax)
      coord. ref. : lon/lat WGS 84 (EPSG:4326) 
      source      : raster_elevs 
      name        : elevation 
      min value   :       141 
      max value   :       547 
      unit        :         m 
      time (days) : 2025-01-15 

---

    Code
      x[2]
    Output
      class       : SpatRaster 
      dimensions  : 115, 114, 1  (nrow, ncol, nlyr)
      resolution  : 683.4048, 683.4048  (x, y)
      extent      : 1480982, 1558890, 5478149, 5556741  (xmin, xmax, ymin, ymax)
      coord. ref. : +proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 
      source      : raster_elevs 
      name        : elevation 
      min value   :  282.8344 
      max value   : 1087.3800 
      unit        :         m 
      time (days) : 2025-01-15 

# tar_terra_sds() works

    Code
      x
    Output
      class       : SpatRasterDataset 
      subdatasets : 2 
      dimensions  : 90, 95 (nrow, ncol)
      nlyr        : 1, 1 
      resolution  : 0.008333333, 0.008333333  (x, y)
      extent      : 5.741667, 6.533333, 49.44167, 50.19167  (xmin, xmax, ymin, ymax)
      coord. ref. : lon/lat WGS 84 (EPSG:4326) 
      source(s)   : raster_elevs 
      names       : raster_elevs, raster_elevs 

---

    Code
      x[1]
    Output
      class       : SpatRaster 
      dimensions  : 90, 95, 1  (nrow, ncol, nlyr)
      resolution  : 0.008333333, 0.008333333  (x, y)
      extent      : 5.741667, 6.533333, 49.44167, 50.19167  (xmin, xmax, ymin, ymax)
      coord. ref. : lon/lat WGS 84 (EPSG:4326) 
      source      : raster_elevs 
      name        : elevation 
      min value   :       141 
      max value   :       547 
      unit        :         m 
      time (days) : 2025-01-15 

---

    Code
      x[2]
    Output
      class       : SpatRaster 
      dimensions  : 90, 95, 1  (nrow, ncol, nlyr)
      resolution  : 0.008333333, 0.008333333  (x, y)
      extent      : 5.741667, 6.533333, 49.44167, 50.19167  (xmin, xmax, ymin, ymax)
      coord. ref. : lon/lat WGS 84 (EPSG:4326) 
      source      : raster_elevs 
      name        : elevation 
      min value   :       282 
      max value   :      1094 
      unit        :         m 
      time (days) : 2025-01-15 

