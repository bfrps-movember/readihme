test_that("tidy_ihme_response handles different inputs", {
  # Test with empty input
  expect_equal(nrow(tidy_ihme_response(NULL)), 0)
  expect_equal(nrow(tidy_ihme_response(list())), 0)

  # Test with simple list
  simple_list <- list(id = 1, name = "test")
  result <- tidy_ihme_response(simple_list)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)

  # Test with nested list
  nested_list <- list(
    list(id = 1, name = "test1"),
    list(id = 2, name = "test2")
  )
  result <- tidy_ihme_response(nested_list)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)

  # Test with data frame input
  df_input <- data.frame(id = 1:3, name = c("a", "b", "c"))
  result <- tidy_ihme_response(df_input)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

test_that("find_countries works with pattern matching", {
  local_mocked_bindings(
    get_locations = function() {
      data.frame(
        location_id = 1:4,
        location_name = c("United States", "United Kingdom", "China", "South Africa"),
        stringsAsFactors = FALSE
      )
    }
  )

  result <- httptest2::without_internet(find_countries("United"))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(stringr::str_detect(result$location_name, "United")))

  # Test case sensitivity
  result_case <- httptest2::without_internet(find_countries("united", ignore_case = FALSE))
  expect_equal(nrow(result_case), 0)
})

test_that("create_location_lookup works", {
  local_mocked_bindings(
    get_locations = function() {
      data.frame(
        location_id = c(3, 1, 2),
        location_name = c("China", "United States", "India"),
        other_col = c("x", "y", "z"),
        stringsAsFactors = FALSE
      )
    }
  )

  result <- httptest2::without_internet(create_location_lookup())
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_true(all(c("location_id", "location_name") %in% names(result)))
  # Check if it's sorted by name
  expect_equal(result$location_name[1], "China")
})

test_that("validate_years works correctly", {
  expect_true(validate_years(c(2000, 2010, 2020)))
  expect_true(validate_years(NULL))

  expect_warning(
    result <- validate_years(c(1989, 2031)),
    "Invalid years detected"
  )
  expect_false(result)
})

