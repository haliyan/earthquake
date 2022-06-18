test_that("date_paster returns a correctly formatted date",{
  expect_equal(nchar(as.character(date_paster(y="2022"))), 10)
  expect_warning(date_paster(y="eighteen hundred"),
                 "All formats failed to parse. No formats found.")
  expect_match(as.character(date_paster(y="2022",m="February")),
               "2022-02-01")
  expect_type(date_paster(y="2022"), "double")
})

test_that("year_pad returns a correctly formatted date",{
  expect_equal(nchar(as.character(year_pad(y=1))),10)
  expect_match(as.character(year_pad(y=2022)), "2022-01-01")
  expect_match(as.character(year_pad(y=100,m=6)), "0100-06-01")
  expect_type(year_pad(y="181"), "double")
})

test_that("bc_date returns a correctly formatted date", {
  expect_equal(nchar(as.character(bc_date(y=-2000))),11)
  expect_equal(nchar(as.character(bc_date(y=-200))),10)
  expect_match(as.character(bc_date(y=-1850)), "^-.")
  expect_warning(bc_date(y="500 BC"), "NAs introduced by coercion")
  expect_type(bc_date(y=-1888), "double")
})

test_that("date_sorter returns a correctly formatted vector of dates",{
  expect_length(date_sorter(c("2021","2022"),c("2","5"),c("11","12")),2)
  expect_type(date_sorter(c("2021","2022"),c("2","5"),c("11","12")),
              "double")
})

test_that("eq_raw_cleaner returns the correct object",{
  expect_equal(dim(eq_raw_cleaner(eq)), c(6259, 37))
})

test_that("eq_location_clean returns the correct type object",{
  expect_s3_class(eq_location_clean(eq_raw_cleaner(tail(eq,100))),
                  "data.frame")
  expect_s3_class(eq_location_clean(eq_raw_cleaner(tail(eq,100)))$`Location Name`,
                  "factor")
  expect_match(as.character(eq_location_clean(eq[300,])$`Location Name`),
               "Ramala, Gaza, Nablus; Egypt")
})

test_that("country_finder returns the correct type object",{
  expect_s3_class(country_finder(eq_raw_cleaner(eq[200:300,])),
                  "data.frame")
  expect_s3_class(country_finder(eq_raw_cleaner(eq[300:400,]))$Country,
                  "factor")
})
