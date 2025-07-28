skip_if_offline <- function() {
  if (!curl::has_internet()) {
    testthat::skip("No internet connection")
  }
}

# Mock data for offline testing
get_mock_locations <- function() {
  data.frame(
    location_id = c(102, 6, 101),
    location_name = c("United States", "China", "India"),
    stringsAsFactors = FALSE
  )
}

get_mock_indicators <- function() {
  data.frame(
    indicator_id = 1:3,
    indicator_name = c("Child mortality", "Maternal mortality", "Life expectancy"),
    stringsAsFactors = FALSE
  )
}

get_mock_api_response <- function(type = "list") {
  if (type == "list") {
    list(
      list(id = 1, name = "Item 1"),
      list(id = 2, name = "Item 2")
    )
  } else {
    data.frame(id = 1:2, name = c("Item 1", "Item 2"))
  }
}
