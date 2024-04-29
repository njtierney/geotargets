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

# tar_terra_rast(zipfile=TRUE) works

    Code
      x
    Output
      class       : SpatRaster 
      dimensions  : 90, 95, 1  (nrow, ncol, nlyr)
      resolution  : 0.008333333, 0.008333333  (x, y)
      extent      : 5.741667, 6.533333, 49.44167, 50.19167  (xmin, xmax, ymin, ymax)
      coord. ref. : lon/lat WGS 84 (EPSG:4326) 
      source      : test_terra_rast2} 
      name        : elevation 

# tar_terra_vect() works

    Code
      x
    Output
       class       : SpatVector 
       geometry    : polygons 
       dimensions  : 12, 6  (geometries, attributes)
       extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
       source      : test_terra_vect
       coord. ref. : lon/lat WGS 84 (EPSG:4326) 
       names       :  ID_1   NAME_1  ID_2   NAME_2  AREA   POP
       type        : <num>    <chr> <num>    <chr> <num> <int>
       values      :     1 Diekirch     1 Clervaux   312 18081
                         1 Diekirch     2 Diekirch   218 32543
                         1 Diekirch     3  Redange   259 18664

---

    Code
      y
    Output
       class       : SpatVector 
       geometry    : polygons 
       dimensions  : 12, 6  (geometries, attributes)
       extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
       source      : test_terra_vect_shz} (test_terra_vect_shz)
       coord. ref. : lon/lat WGS 84 (EPSG:4326) 
       names       :  ID_1   NAME_1  ID_2   NAME_2  AREA   POP
       type        : <num>    <chr> <num>    <chr> <num> <int>
       values      :     1 Diekirch     1 Clervaux   312 18081
                         1 Diekirch     2 Diekirch   218 32543
                         1 Diekirch     3  Redange   259 18664

---

    Code
      z
    Output
       class       : SpatVector 
       geometry    : polygons 
       dimensions  : 12, 6  (geometries, attributes)
       extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
       source      : test_terra_vect_geobuf_zip} (test_terra_vect_geobuf_zip)
       coord. ref. : lon/lat WGS 84 (EPSG:4326) 
       names       :  ID_1       NAME_1  ID_2     NAME_2  AREA    POP
       type        : <num>        <chr> <num>      <chr> <num>  <int>
       values      :     3   Luxembourg    10 Luxembourg   237 182607
                         2 Grevenmacher     7     Remich   129  22366
                         2 Grevenmacher     6 Echternach   188  18899

