data(eq_data)

test_that("eq_create_label creates the correct vector",{
  expect_type(eq_create_label(eq_raw_cleaner(as.data.frame(tail(eq_data)))),
              "character")
  expect_equal(length(eq_create_label(eq_raw_cleaner(as.data.frame(tail(eq_data))))),6)
})

test_that("eq_map produces the correct output", {
  expect_s3_class(eq_map(eq_raw_cleaner(as.data.frame(tail(eq_data)))),
                  class= 'leaflet')
})
