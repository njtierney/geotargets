# tile_n fails with non integer

    Code
      tile_n(r, n = 3.14)
    Condition
      Error in `tile_n()`:
      ! `n` must be an integer.
      We see that `n` is: 3.14

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
       6.141667  6.533333 49.816667 50.191667 
      
      [[3]]
           xmin      xmax      ymin      ymax 
       5.741667  6.141667 49.441667 49.816667 
      
      [[4]]
           xmin      xmax      ymin      ymax 
       6.141667  6.533333 49.441667 49.816667 
      

# tile_grid fails with non integer

    Code
      tile_grid(r, ncol = 1.5, nrow = 2.5)
    Condition
      Error in `tile_grid()`:
      ! `ncol` must be an integer.
      We see that `ncol` is: 1.5

---

    Code
      tile_grid(r, ncol = 1, nrow = 2.5)
    Condition
      Error in `tile_grid()`:
      ! `nrow` must be an integer.
      We see that `nrow` is: 2.5

---

    Code
      tile_grid(r, ncol = 1.5, nrow = 2)
    Condition
      Error in `tile_grid()`:
      ! `ncol` must be an integer.
      We see that `ncol` is: 1.5

---

    Code
      tile_grid(r, ncol = 2, nrow = 3)
    Output
      [[1]]
           xmin      xmax      ymin      ymax 
       5.741667  6.141667 49.941667 50.191667 
      
      [[2]]
           xmin      xmax      ymin      ymax 
       6.141667  6.533333 49.941667 50.191667 
      
      [[3]]
           xmin      xmax      ymin      ymax 
       5.741667  6.141667 49.691667 49.941667 
      
      [[4]]
           xmin      xmax      ymin      ymax 
       6.141667  6.533333 49.691667 49.941667 
      
      [[5]]
           xmin      xmax      ymin      ymax 
       5.741667  6.141667 49.441667 49.691667 
      
      [[6]]
           xmin      xmax      ymin      ymax 
       6.141667  6.533333 49.441667 49.691667 
      

