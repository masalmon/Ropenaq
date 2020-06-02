test_that("API endoint errors, status API errors too", {
  library("magrittr")
  webmockr::enable()

  webmockr::stub_request("get", paste0(base_url(), "countries?limit=0&page=1")) %>%
    webmockr::to_return(status = 404)

  webmockr::stub_request("get", status_url()) %>%
    webmockr::to_return(body = '{"meta":{"name":"openaq-api","license":"CC BY 4.0","website":"https://docs.openaq.org/"},"results":{"healthStatus":"red"}}')

  expect_error(aq_countries(), "uh oh")

  webmockr::disable()
})

test_that("API errors twice, status not", {
  library("magrittr")
  # when not all is well
  webmockr::enable()

  webmockr::stub_request("get", paste0(base_url(), "countries?limit=0&page=1")) %>%
    webmockr::to_return(status = 404, body = "retry!", times = 2) %>%
    webmockr::to_return(status = 200,
                        headers = list(date = "Thu, 09 Apr 2020 14:20:43 GMT"),
                        body = '{"meta":{"name":"openaq-api","license":"CC BY 4.0","website":"https://docs.openaq.org/","page":1,"limit":100,"found":98}}') # here I want to use a real response and cassette
  # how do I do that?

  webmockr::stub_request("get", status_url()) %>%
    webmockr::to_return(body = '{"meta":{"name":"openaq-api","license":"CC BY 4.0","website":"https://docs.openaq.org/"},"results":{"healthStatus":"green"}}')

  webmockr::stub_request("get", paste0(base_url(), "countries?limit=10000")) %>%
    webmockr::to_return(status = 200,
                        headers = list(date = "Thu, 09 Apr 2020 14:20:43 GMT"),
                        body = '{"meta":{"name":"openaq-api","license":"CC BY 4.0","website":"https://docs.openaq.org/","page":1,"limit":100,"found":98},"results":[{"code":"AD","count":118490,"locations":3,"cities":2,"name":"Andorra"},{"code":"AE","count":77527,"locations":4,"cities":3,"name":"United Arab Emirates"}]}')

  expect_message(countries <- aq_countries(), "Checking status")
  expect_is(countries, "tbl_df")

  webmockr::disable()
})

