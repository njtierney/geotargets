# tile_n fails with non integer

    Code
      tile_n(r, n = 3.14)
    Condition
      Error in `tile_n()`:
      ! 3.14 must be an integer.
      We see that "n" is: 3.14

---

    Code
      tile_n(r, n = 4)
    Message
      creating 2 * 2 = 4 tile extents
    Output
      [[1]]
           xmin      xmax      ymin      ymax 
       5.741667  6.141667 49.816667 50.191667 
      
      [[2]]
           xmin      xmax      ymin      ymax 
       6.133333  6.533333 49.816667 50.191667 
      
      [[3]]
           xmin      xmax      ymin      ymax 
       5.741667  6.141667 49.441667 49.816667 
      
      [[4]]
           xmin      xmax      ymin      ymax 
       6.133333  6.533333 49.441667 49.816667 
      

