test_that("data retrieval functions work with limited parameters", {
  skip_if_offline()

  # Test basic data retrieval with very limited parameters to avoid large responses
  result_by_year <- get_results_by_year(
    year = 2020,
    location_id = 102,  # Just USA
    indicator_id = 1    # Just one indicator
  )
  expect_s3_class(result_by_year, "data.frame")

  result_by_location <- get_results_by_location(
    location_id = 102,
    year = 2020,
    indicator_id = 1
  )
  expect_s3_class(result_by_location, "data.frame")
})

test_that("get_sdg_data combines multiple requests", {
  skip_if_offline()

  # Test with minimal parameters to avoid timeout
  result <- get_sdg_data(
    indicator_ids = c(1),
    location_ids = c(102),
    years = c(2020)
  )
  expect_s3_class(result, "data.frame")
  expect_true("indicator_id" %in% names(result))
})

test_that("data functions work offline with mocked data", {
  local_mocked_bindings(
    call_ihme_api = function(endpoint, ...) {
      # Return mock data that looks like API response
      list(
        list(indicator_id = 1, location_id = 102, year = 2020, value = 10.5),
        list(indicator_id = 1, location_id = 102, year = 2021, value = 9.8)
      )
    }
  )

  result <- httptest2::without_internet(
    get_results_by_year(year = 2020, location_id = 102, indicator_id = 1)
  )
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("download_all_ihme_metadata works with mocked data", {
  temp_file <- tempfile(fileext = ".rds")

  # Mock all the metadata functions to avoid actual API calls
  local_mocked_bindings(
    get_age_groups = function() data.frame(age_group_id = 1:3, age_group_name = c("0-1", "1-4", "5-9")),
    get_goals = function() data.frame(goal_id = 1:3, goal_name = c("Goal 1", "Goal 2", "Goal 3")),
    get_indicators = function() data.frame(indicator_id = 1:2, indicator_name = c("Ind 1", "Ind 2")),
    get_locations = function() data.frame(location_id = 1:2, location_name = c("Country A", "Country B")),
    get_scenarios = function() data.frame(scenario_id = 1:2, scenario_name = c("Ref", "Alt")),
    get_sex_categories = function() data.frame(sex_id = 1:3, sex_name = c("Both", "Male", "Female")),
    get_targets = function() data.frame(target_id = 1:2, target_name = c("Target 1", "Target 2")),
    get_version_hash = function() "test_hash_123",
    has_ihme_key = function() TRUE
  )

  result <- httptest2::without_internet(
    download_all_ihme_metadata(file = temp_file, force = TRUE)
  )

  # Check structure
  expect_type(result, "list")
  expected_elements <- c("age_groups", "goals", "indicators", "locations",
                         "scenarios", "sex_categories", "targets", "downloaded", "version_hash")
  expect_true(all(expected_elements %in% names(result)))

  # Check file was created
  expect_true(file.exists(temp_file))

  # Test cached data detection
  expect_message(
    result2 <- download_all_ihme_metadata(file = temp_file, force = FALSE),
    "already exists and is the latest release"
  )

  unlink(temp_file)
})

test_that("batch_download_sdg_data works with mocked data", {
  local_mocked_bindings(
    get_sdg_data = function(indicator_ids, location_ids, years, ...) {
      # Return mock data for each indicator
      purrr::map_dfr(indicator_ids, ~{
        data.frame(
          indicator_id = .x,
          location_id = rep(location_ids, length(years)),
          year = rep(years, each = length(location_ids)),
          value = runif(length(location_ids) * length(years))
        )
      })
    }
  )

  result <- httptest2::without_internet(
    batch_download_sdg_data(
      indicator_ids = 1:7,  # Should create 2 batches with batch_size = 3
      location_ids = c(102, 6),
      years = c(2020, 2021),
      batch_size = 3,
      delay_between_batches = 0  # No delay for testing
    )
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("indicator_id" %in% names(result))
  expect_equal(length(unique(result$indicator_id)), 7)
})

test_that("convenience functions require authentication", {
  local_mocked_bindings(
    get_ihme_key = function(error_if_missing = TRUE) {
      if (error_if_missing) {
        stop("IHME API key not found")
      }
      return(NULL)
    }
  )

  expect_error(get_all_indicators(), "IHME API key not found")
  expect_error(get_all_locations(), "IHME API key not found")
  expect_error(get_all_sdg_data(), "IHME API key not found")
})

test_that("convenience functions work with authentication", {
  local_mocked_bindings(
    get_ihme_key = function(error_if_missing = TRUE) "mock_key",
    get_indicators = function() data.frame(indicator_id = 1:2, indicator_name = c("Ind 1", "Ind 2")),
    get_locations = function() data.frame(location_id = 1:2, location_name = c("Country A", "Country B")),
    batch_download_sdg_data = function(...) data.frame(indicator_id = 1, location_id = 1, year = 2020, value = 1.0)
  )

  expect_s3_class(httptest2::without_internet(get_all_indicators()), "data.frame")
  expect_s3_class(httptest2::without_internet(get_all_locations()), "data.frame")
  expect_s3_class(httptest2::without_internet(get_all_sdg_data(location_ids = c(102))), "data.frame")
})
