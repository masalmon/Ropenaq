test_that("Country, city and location are checked for consistency", {
  vcr::use_cassette("buildQueries_locations_consistency", {
    expect_error(aq_measurements(page = 1, country="PANEM"),
                 "not available within the platform")

    expect_error(aq_measurements(page = 1, country="AD", city="Barcelona"),
                 "not available within the platform")

    expect_error(aq_measurements(page = 1, city="Capitole"),
                 "not available within the platform")

    expect_error(aq_measurements(page = 1, country="AD", location="Nirgendwo"),
                 "not available within the platform")

    expect_error(aq_measurements(page = 1, country="AD", city="Escaldes-Engordany", location="Nirgendwo"),
                 "not available within the platform")

    expect_error(aq_measurements(page = 1, city="Escaldes-Engordany", location="Nirgendwo"),
                 "not available within the platform")
  })

})


test_that("Parameter has to be available", {
  vcr::use_cassette("buildQueries_parameter_consistency", {
  expect_error(aq_measurements(page = 1, country = "AF", parameter="pm10"),
               "This parameter is not available for any location corresponding to your query.")

  expect_error(aq_measurements(page = 1, city="Escaldes-Engordany", parameter="lalala"),
               "You asked for an invalid parameter: see list of valid parameters in the Arguments section of the function help")

  })
})

test_that("bad value_from and value_to provoke errors", {

  expect_error(aq_measurements(page = 1, value_from=-3),
               "No negative value for value_from please!")

  expect_error(aq_measurements(page = 1, value_to=-3),
               "No negative value for value_to please!")

  expect_error(aq_measurements(page = 1, value_from=3, value_to=1),
               "The max value must be bigger than the min value.")
})



test_that("The format of date_from and date_to are checked", {

  expect_error(aq_measurements(page = 1, date_from="2014-29-12"),
               "date_from and date_to have to be inputed as year-month-day.")

  expect_error(aq_measurements(page = 1, date_to="2014-29-12"),
               "date_from and date_to have to be inputed as year-month-day.")

  expect_error(aq_measurements(page = 1, date_to="2014-12-29", date_from="2015-12-29"),
               "The start date must be smaller than the end date.")
})

test_that("An error is thrown is limit>10,000",{
  expect_error(aq_measurements(page = 1, has_geo=TRUE, limit=9999999, country="US"),
               "limit cannot be more than 10,000")
})

test_that("Errors are thrown if the geographical arguments are wrong",{
  expect_error(aq_latest(page = 1, latitude = 1),
               "If you input")

  expect_error(aq_latest(page = 1, latitude = 10000, longitude = 2),
               "Latitude should be")

  expect_error(aq_latest(page = 1, latitude = 2, longitude = 2000),
               "Longitude should be")

  expect_error(aq_latest(page = 1, radius = 25),
               "Radius has to be used with latitude and longitude.")
  })

test_that("Queries work with spaces and accents",{
  # Both skipping because of https://github.com/ropensci/vcr/issues/158
  skip_on_os("windows")
  skip_on_cran()
  skip_on_os("mac")
  vcr::use_cassette("buildQueries_accents", {
  result1 <- aq_measurements(city = "Heinz+Ott", country = "DE",
                             limit = 1, page = 1)
  expect_is(result1, "tbl_df")
  result2 <- aq_measurements(city = "J%EF%BF%BDrgen+Friesel", country = "DE",
                             limit = 1, page = 1)
  expect_is(result2, "tbl_df")
  })
})
