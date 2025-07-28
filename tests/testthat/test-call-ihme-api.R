test_that("call_ihme_api works with basic endpoint", {
  skip_if_offline()

  # Test API version endpoint (should be lightweight)
  result <- call_ihme_api("api_version")
  expect_type(result, "list")
})

test_that("call_ihme_api handles parameters correctly", {
  skip_if_offline()

  # Test with parameters - use a specific location to limit response size
  result <- call_ihme_api("GetLocation", location_id = "102")
  expect_type(result, "list")
})

test_that("get_base_url returns correct URL", {
  url <- get_base_url()
  expect_type(url, "character")
  expect_true(stringr::str_detect(url, "api\\.healthdata\\.org"))
  expect_true(stringr::str_ends(url, "/"))
})

test_that("get_api_version works", {
  skip_if_offline()

  version <- get_api_version()
  expect_type(version, "list")
})

