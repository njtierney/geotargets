test_that("formats return output of tar_format()", {
    expect_type(format_terra_rast, "closure")
    expect_type(format_terra_rast(), "character")
    expect_match(format_terra_rast(), "^format_custom.+")
})
