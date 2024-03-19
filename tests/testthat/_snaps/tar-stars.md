# tar_stars() works

    Code
      x
    Output
      stars object with 2 dimensions and 1 attribute
      attribute(s):
                  Min. 1st Qu. Median     Mean 3rd Qu. Max.
      test_stars    -1       6     12 21.66521      35   88
      dimension(s):
        from  to  offset  delta                       refsys point x/y
      x    1 111  288776  89.99 UTM Zone 25, Southern Hem... FALSE [x]
      y    1 111 9120761 -89.99 UTM Zone 25, Southern Hem... FALSE [y]

# tar_stars_proxy() works

    Code
      x
    Output
      stars_proxy object with 1 attribute in 1 file(s):
      $test_stars_proxy
      [1] "[...]/test_stars_proxy"
      
      dimension(s):
        from  to  offset  delta                       refsys point x/y
      x    1 111  288776  89.99 UTM Zone 25, Southern Hem... FALSE [x]
      y    1 111 9120761 -89.99 UTM Zone 25, Southern Hem... FALSE [y]

# tar_stars(mdim=TRUE) works

    Code
      x
    Output
      stars object with 2 dimensions and 1 attribute
      attribute(s):
                           Min.   1st Qu.    Median      Mean  3rd Qu.      Max.
      Precipitation  0.03524588 0.3224987 0.3772574 0.4289465 0.511113 0.9204841
      dimension(s):
               from to     offset  delta refsys point                   values
      stations    1  2         NA     NA     NA  TRUE POINT (0 1), POINT (3 5)
      time        1  5 2022-05-02 1 days   Date    NA                     NULL

# tar_stars(mdim=TRUE, ncdf=TRUE) works

    Code
      x
    Output
      stars object with 2 dimensions and 1 attribute
      attribute(s):
                           Min.   1st Qu.    Median      Mean  3rd Qu.      Max.
      Precipitation  0.03524588 0.3224987 0.3772574 0.4289465 0.511113 0.9204841
      dimension(s):
               from to         offset  delta x/y
      stations    1  2            0.5      1 [x]
      time        1  5 2022-05-02 UTC 1 days [y]

