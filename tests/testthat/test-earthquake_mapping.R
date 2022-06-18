test_that("eq_create_label creates the correct vector",{
  expect_type(eq_create_label(eq_raw_cleaner(tail(eq))),
              "character")
  expect_equal(length(eq_create_label(eq_raw_cleaner(tail(eq)))),6)
})

test_that("eq_map produces the correct output", {
  expect_s3_class(eq_map(eq_raw_cleaner(tail(eq))),
                  class= 'leaflet')
})
