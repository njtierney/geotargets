# check_user_resources works

    Code
      check_user_resources(resources = c(custom_format = 1))
    Condition
      Error:
      ! "custom_format" cannot be supplied to targets created with `tar_terra_rast()`
      We see in `names(resources)`:
      "custom_format"

