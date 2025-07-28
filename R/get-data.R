#' Get Results by Target
#'
#' @param target_id Target ID to retrieve results for
#' @param location_id Optional vector of location IDs to filter results
#' @param year Optional vector of years (1990-2030) to filter results
#' @param sex_id Optional vector of sex IDs to filter results
#' @param age_group_id Optional vector of age group IDs to filter results
#' @param scenario Optional vector of scenario IDs to filter results
#'
#' @return Data frame of results for the specified target
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_results_by_target(target_id = 1, location_id = c(102, 6), year = c(2020, 2021))
get_results_by_target <- function(target_id = NULL, location_id = NULL, year = NULL,
                                  sex_id = NULL, age_group_id = NULL, scenario = NULL) {
  params <- build_params(
    target_id = target_id,
    location_id = location_id,
    year = year,
    sex_id = sex_id,
    age_group_id = age_group_id,
    scenario = scenario
  )

  # Validate years if provided
  if (!is.null(year)) validate_years(year)

  if (length(params) > 0) {
    result <- call_ihme_api("GetResultsByTarget", !!!params)
  } else {
    result <- call_ihme_api("GetResultsByTarget")
  }
  tidy_ihme_response(result)
}

#' Get Results by Indicator
#'
#' @param indicator_id Indicator ID to retrieve results for
#' @param location_id Optional vector of location IDs to filter results
#' @param year Optional vector of years (1990-2030) to filter results
#' @param sex_id Optional vector of sex IDs to filter results
#' @param age_group_id Optional vector of age group IDs to filter results
#' @param scenario Optional vector of scenario IDs to filter results
#'
#' @return Data frame of results for the specified indicator
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_results_by_indicator(indicator_id = 1, location_id = c(102, 6), year = c(2020, 2021))
get_results_by_indicator <- function(indicator_id = NULL, location_id = NULL, year = NULL,
                                     sex_id = NULL, age_group_id = NULL, scenario = NULL) {
  params <- build_params(
    indicator_id = indicator_id,
    location_id = location_id,
    year = year,
    sex_id = sex_id,
    age_group_id = age_group_id,
    scenario = scenario
  )

  # Validate years if provided
  if (!is.null(year)) validate_years(year)

  if (length(params) > 0) {
    result <- call_ihme_api("GetResultsByIndicator", !!!params)
  } else {
    result <- call_ihme_api("GetResultsByIndicator")
  }
  tidy_ihme_response(result)
}

#' Get Results by Location
#'
#' @param location_id Location ID to retrieve results for
#' @param indicator_id Optional vector of indicator IDs to filter results
#' @param year Optional vector of years (1990-2030) to filter results
#' @param sex_id Optional vector of sex IDs to filter results
#' @param age_group_id Optional vector of age group IDs to filter results
#' @param scenario Optional vector of scenario IDs to filter results
#'
#' @return Data frame of results for the specified location
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_results_by_location(location_id = 102, indicator_id = c(1, 2), year = c(2020, 2021))
get_results_by_location <- function(location_id = NULL, indicator_id = NULL, year = NULL,
                                    sex_id = NULL, age_group_id = NULL, scenario = NULL) {
  params <- build_params(
    location_id = location_id,
    indicator_id = indicator_id,
    year = year,
    sex_id = sex_id,
    age_group_id = age_group_id,
    scenario = scenario
  )

  # Validate years if provided
  if (!is.null(year)) validate_years(year)

  if (length(params) > 0) {
    result <- call_ihme_api("GetResultsByLocation", !!!params)
  } else {
    result <- call_ihme_api("GetResultsByLocation")
  }
  tidy_ihme_response(result)
}

#' Get Results by Year
#'
#' @param year Year (1990-2030) to retrieve results for
#' @param indicator_id Optional vector of indicator IDs to filter results
#' @param location_id Optional vector of location IDs to filter results
#' @param sex_id Optional vector of sex IDs to filter results
#' @param age_group_id Optional vector of age group IDs to filter results
#' @param scenario Optional vector of scenario IDs to filter results
#'
#' @return Data frame of results for the specified year
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_results_by_year(year = 2020, indicator_id = c(1, 2), location_id = c(102, 6))
get_results_by_year <- function(year = NULL, indicator_id = NULL, location_id = NULL,
                                sex_id = NULL, age_group_id = NULL, scenario = NULL) {
  params <- build_params(
    year = year,
    indicator_id = indicator_id,
    location_id = location_id,
    sex_id = sex_id,
    age_group_id = age_group_id,
    scenario = scenario
  )

  # Validate years if provided
  if (!is.null(year)) validate_years(year)

  if (length(params) > 0) {
    result <- call_ihme_api("GetResultsByYear", !!!params)
  } else {
    result <- call_ihme_api("GetResultsByYear")
  }
  tidy_ihme_response(result)
}

#' Get SDG Data for Multiple Countries and Years
#'
#' This is a convenience function that retrieves data for multiple indicators,
#' countries, and years in a single call. It handles the API requests internally
#' and combines the results.
#'
#' @param indicator_ids Vector of indicator IDs
#' @param location_ids Vector of location IDs (countries)
#' @param years Vector of years
#' @param sex_id Optional sex ID filter
#' @param age_group_id Optional age group ID filter
#' @param scenario Optional scenario filter
#'
#' @return Combined data frame with results
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_sdg_data(indicator_ids = c(1, 2), location_ids = c(102, 6), years = c(2020, 2021))
get_sdg_data <- function(indicator_ids, location_ids, years, sex_id = NULL,
                         age_group_id = NULL, scenario = NULL) {

  # Validate inputs
  if (!is.null(years)) validate_years(years)

  if (length(indicator_ids) > 5 || length(location_ids) > 10 || length(years) > 10) {
    message("Large request detected. Consider using batch_download_sdg_data() for better performance.")
  }

  message(glue::glue("Retrieving data for {length(indicator_ids)} indicators, {length(location_ids)} locations, {length(years)} years"))

  # Get data for each indicator
  all_data <- purrr::map_dfr(indicator_ids, ~{
    message(glue::glue("Processing indicator {.x}"))

    get_results_by_indicator(
      indicator_id = .x,
      location_id = location_ids,
      year = years,
      sex_id = sex_id,
      age_group_id = age_group_id,
      scenario = scenario
    ) |>
      dplyr::mutate(indicator_id = .x)
  })

  message(glue::glue("Retrieved {nrow(all_data)} total observations"))
  all_data
}

#' Download All IHME Metadata
#'
#' Downloads and caches all available metadata from the IHME API including
#' locations, indicators, targets, goals, age groups, sex categories, and scenarios.
#'
#' @param file Path to save data (default: "ihme_metadata.rds")
#' @param force Force redownload, even if file exists and is current (default: FALSE)
#'
#' @return List containing all IHME metadata with elements:
#'   \itemize{
#'     \item age_groups - Available age group categories
#'     \item goals - SDG goals
#'     \item indicators - All available indicators
#'     \item locations - Countries and regions
#'     \item scenarios - Projection scenarios
#'     \item sex_categories - Sex categories (Both, Male, Female)
#'     \item targets - SDG targets
#'     \item downloaded - Timestamp of download
#'     \item version_hash - Hash for cache validation
#'   }
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' # Download all metadata to default file
#' metadata <- download_all_ihme_metadata()
#'
#' # Force fresh download
#' metadata <- download_all_ihme_metadata(force = TRUE)
download_all_ihme_metadata <- function(file = "ihme_metadata.rds", force = FALSE) {

  # Check for API key before proceeding
  if (!has_ihme_key()) {
    stop(
      "IHME API key is required to download data.\n",
      "Please set your API key using: set_ihme_key('your_key_here')\n",
      "Get your API key at: https://api.healthdata.org/sdg/v1/",
      call. = FALSE
    )
  }

  # Check if we can use cached data
  if (!force) {
    cached_data <- check_cached_data(file)
    if (!is.null(cached_data)) {
      return(cached_data)
    }
  }

  message("Downloading IHME metadata...")

  # Get all metadata
  metadata <- list(
    age_groups = get_age_groups(),
    goals = get_goals(),
    indicators = get_indicators(),
    locations = get_locations(),
    scenarios = get_scenarios(),
    sex_categories = get_sex_categories(),
    targets = get_targets(),
    downloaded = Sys.time(),
    version_hash = get_version_hash()
  )

  # Save metadata
  saveRDS(metadata, file, compress = TRUE)
  message(glue::glue(
    "Saved metadata: {nrow(metadata$locations)} locations, ",
    "{nrow(metadata$indicators)} indicators, ",
    "{nrow(metadata$targets)} targets, ",
    "{nrow(metadata$goals)} goals"
  ))

  metadata
}

#' Batch Download SDG Data
#'
#' Download large amounts of SDG data in batches to avoid overwhelming the API.
#'
#' @param indicator_ids Vector of indicator IDs
#' @param location_ids Vector of location IDs
#' @param years Vector of years
#' @param batch_size Number of indicators to process per batch (default: 5)
#' @param delay_between_batches Delay in seconds between batches (default: 1)
#' @param sex_id Optional sex ID filter
#' @param age_group_id Optional age group ID filter
#'
#' @return Combined data frame with all results
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' # Download data for many indicators in batches
#' large_dataset <- batch_download_sdg_data(
#'   indicator_ids = 1:20,
#'   location_ids = c(102, 6, 101),
#'   years = c(2020, 2021, 2022),
#'   batch_size = 3
#' )
batch_download_sdg_data <- function(indicator_ids, location_ids, years,
                                    batch_size = 5, delay_between_batches = 1,
                                    sex_id = NULL, age_group_id = NULL) {

  # Split indicators into batches
  indicator_batches <- split(indicator_ids, ceiling(seq_along(indicator_ids) / batch_size))

  message(glue::glue("Processing {length(indicator_ids)} indicators in {length(indicator_batches)} batches"))

  all_data <- purrr::map_dfr(seq_along(indicator_batches), ~{
    batch_num <- .x
    batch_indicators <- indicator_batches[[.x]]

    message(glue::glue("Processing batch {batch_num}/{length(indicator_batches)} ({length(batch_indicators)} indicators)"))

    # Get data for this batch
    batch_data <- get_sdg_data(
      indicator_ids = batch_indicators,
      location_ids = location_ids,
      years = years,
      sex_id = sex_id,
      age_group_id = age_group_id
    )

    # Add delay between batches (except for the last one)
    if (batch_num < length(indicator_batches)) {
      message(glue::glue("Waiting {delay_between_batches} second(s) before next batch..."))
      Sys.sleep(delay_between_batches)
    }

    batch_data
  })

  message(glue::glue("Batch download complete. Retrieved {nrow(all_data)} total observations"))
  all_data
}

#' Create Comprehensive Country Dataset
#'
#' Downloads all available indicators for specified countries and years.
#'
#' @param country_names Vector of country names to search for
#' @param years Vector of years
#' @param include_projections Include projection data (years > current year)
#'
#' @return Combined dataset with all indicators for specified countries
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' # Get all data for specific countries
#' country_data <- create_comprehensive_country_dataset(
#'   country_names = c("United States", "China", "India"),
#'   years = c(2015, 2020),
#'   include_projections = FALSE
#' )
create_comprehensive_country_dataset <- function(country_names, years,
                                                 include_projections = TRUE) {

  # Find country location IDs
  all_locations <- get_locations()

  if (!"location_name" %in% names(all_locations)) {
    stop("Cannot find location names in location data")
  }

  matched_countries <- all_locations |>
    dplyr::filter(location_name %in% country_names)

  if (nrow(matched_countries) == 0) {
    stop("No countries found matching the provided names")
  }

  missing_countries <- setdiff(country_names, matched_countries$location_name)
  if (length(missing_countries) > 0) {
    message(glue::glue("Could not find: {paste(missing_countries, collapse = ', ')}"))
  }

  # Get all available indicators
  all_indicators <- get_indicators()

  if (nrow(all_indicators) == 0 || !"indicator_id" %in% names(all_indicators)) {
    stop("Could not retrieve indicator information")
  }

  # Filter years if not including projections
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  if (!include_projections) {
    years <- years[years <= current_year]
  }

  message(glue::glue(
    "Downloading data for {nrow(matched_countries)} countries, ",
    "{nrow(all_indicators)} indicators, ",
    "{length(years)} years"
  ))

  # Use batch download for large requests
  comprehensive_data <- batch_download_sdg_data(
    indicator_ids = all_indicators$indicator_id,
    location_ids = matched_countries$location_id,
    years = years,
    batch_size = 5,
    delay_between_batches = 2
  )

  # Add metadata
  comprehensive_data |>
    dplyr::left_join(matched_countries, by = "location_id") |>
    dplyr::left_join(all_indicators, by = "indicator_id")
}

#' Get All SDG Indicators
#'
#' Get all indicators across all categories.
#'
#' @return Data frame of all indicators
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_all_indicators()
get_all_indicators <- function() {
  # Check for API key before proceeding
  get_ihme_key(error_if_missing = TRUE)
  get_indicators()
}

#' Get All Locations
#'
#' Get all available locations (countries and regions).
#'
#' @return Data frame of all locations
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_all_locations()
get_all_locations <- function() {
  # Check for API key before proceeding
  get_ihme_key(error_if_missing = TRUE)
  get_locations()
}

#' Get All SDG Data
#'
#' Get all available SDG data for specified parameters. Use with caution as this
#' can return very large datasets.
#'
#' @param location_ids Optional vector of location IDs to filter results
#' @param indicator_ids Optional vector of indicator IDs to filter results
#' @param years Optional vector of years to filter results
#' @param batch_size Number of indicators to process per batch (default: 5)
#'
#' @return Data frame of all available SDG data matching the filters
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' # Get data for specific countries and years
#' all_data <- get_all_sdg_data(
#'   location_ids = c(102, 6),
#'   years = c(2020, 2021),
#'   batch_size = 3
#' )
get_all_sdg_data <- function(location_ids = NULL, indicator_ids = NULL,
                             years = NULL, batch_size = 5) {

  # Check for API key before proceeding
  get_ihme_key(error_if_missing = TRUE)

  # Get all indicators if not specified
  if (is.null(indicator_ids)) {
    all_indicators <- get_indicators()
    indicator_ids <- all_indicators$indicator_id
    message(glue::glue("Retrieved {length(indicator_ids)} indicators"))
  }

  # Get all locations if not specified
  if (is.null(location_ids)) {
    all_locations <- get_locations()
    location_ids <- all_locations$location_id
    message(glue::glue("Retrieved {length(location_ids)} locations"))
  }

  # Default to recent years if not specified
  if (is.null(years)) {
    years <- c(2015, 2020, 2025)
    message("Using default years: 2015, 2020, 2025")
  }

  # Use batch download for large requests
  batch_download_sdg_data(
    indicator_ids = indicator_ids,
    location_ids = location_ids,
    years = years,
    batch_size = batch_size,
    delay_between_batches = 2
  )
}
