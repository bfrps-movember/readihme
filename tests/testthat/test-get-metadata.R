test_that("metadata functions return data frames", {
  skip_if_offline()

  expect_s3_class(get_age_groups(), "data.frame")
  expect_s3_class(get_goals(), "data.frame")
  expect_s3_class(get_indicators(), "data.frame")
  expect_s3_class(get_locations(), "data.frame")
  expect_s3_class(get_scenarios(), "data.frame")
  expect_s3_class(get_sex_categories(), "data.frame")
  expect_s3_class(get_targets(), "data.frame")
})

test_that("metadata functions work with filters", {
  skip_if_offline()

  # Test filtered requests with specific IDs to limit response size
  age_groups_filtered <- get_age_groups("1")
  expect_s3_class(age_groups_filtered, "data.frame")

  goals_filtered <- get_goals(c(1, 3))
  expect_s3_class(goals_filtered, "data.frame")

  locations_filtered <- get_locations("102")
  expect_s3_class(locations_filtered, "data.frame")
})

test_that("metadata functions work offline with mocked data", {
  local_mocked_bindings(
    call_ihme_api = function(endpoint, ...) {
      switch(endpoint,
             "GetAgeGroup" = get_mock_api_response(),
             "GetGoal" = get_mock_api_response(),
             "GetIndicator" = get_mock_api_response(),
             "GetLocation" = get_mock_api_response(),
             "GetScenario" = get_mock_api_response(),
             "GetSex" = get_mock_api_response(),
             "GetTarget" = get_mock_api_response(),
             list()
      )
    }
  )

  expect_s3_class(httptest2::without_internet(get_age_groups()), "data.frame")
  expect_s3_class(httptest2::without_internet(get_goals()), "data.frame")
  expect_s3_class(httptest2::without_internet(get_indicators()), "data.frame")
  expect_s3_class(httptest2::without_internet(get_locations()), "data.frame")
})

