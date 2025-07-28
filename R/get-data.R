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
    target_id = target_id, location_id = location_id, year = year,
    sex_id = sex_id, age_group_id = age_group_id, scenario = scenario
  )

  if (!is.null(year)) validate_years(year)

  if (length(params) > 0) {
    result <- do.call(call_ihme_api, c(list("GetResultsByTarget"), params))
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
    indicator_id = indicator_id, location_id = location_id, year = year,
    sex_id = sex_id, age_group_id = age_group_id, scenario = scenario
  )

  if (!is.null(year)) validate_years(year)

  if (length(params) > 0) {
    result <- do.call(call_ihme_api, c(list("GetResultsByIndicator"), params))
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
    location_id = location_id, indicator_id = indicator_id, year = year,
    sex_id = sex_id, age_group_id = age_group_id, scenario = scenario
  )

  if (!is.null(year)) validate_years(year)

  if (length(params) > 0) {
    result <- do.call(call_ihme_api, c(list("GetResultsByLocation"), params))
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
    year = year, indicator_id = indicator_id, location_id = location_id,
    sex_id = sex_id, age_group_id = age_group_id, scenario = scenario
  )

  if (!is.null(year)) validate_years(year)

  if (length(params) > 0) {
    result <- do.call(call_ihme_api, c(list("GetResultsByYear"), params))
  } else {
    result <- call_ihme_api("GetResultsByYear")
  }
  tidy_ihme_response(result)
}

#' Get SDG Data for Multiple Countries and Years
#'
#' Convenience function that retrieves data for multiple indicators, countries,
#' and years in a single call. Handles API requests internally and combines results.
#'
#' @param indicator_ids Vector of indicator IDs
#' @param location_ids Vector of location IDs (countries)
#' @param years Vector of years
#' @param sex_id Optional sex ID filter
#' @param age_group_id Optional age group ID filter
#' @param scenario Optional scenario filter
#' @param location_batch_size Number of locations to process per batch (default: 50)
#' @param delay_between_location_batches Delay in seconds between location batches (default: 0.5)
#'
#' @return Combined data frame with results
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_sdg_data(indicator_ids = c(1, 2), location_ids = c(102, 6), years = c(2020, 2021))
get_sdg_data <- function(indicator_ids, location_ids, years, sex_id = NULL,
                         age_group_id = NULL, scenario = NULL,
                         location_batch_size = 50, delay_between_location_batches = 0.5) {

  if (!is.null(years)) validate_years(years)

  if (length(indicator_ids) > 5 || length(location_ids) > 10 || length(years) > 10) {
    message("Large request detected. Consider using batch_download_sdg_data() for better performance.")
  }

  message(glue::glue("Retrieving data for {length(indicator_ids)} indicators, {length(location_ids)} locations, {length(years)} years"))

  all_data_list <- list()

  for (current_indicator_id in indicator_ids) {
    message(glue::glue("Processing indicator {current_indicator_id}"))

    location_chunks <- split(location_ids, ceiling(seq_along(location_ids) / location_batch_size))

    for (i in seq_along(location_chunks)) {
      chunk_of_locations <- location_chunks[[i]]

      message(glue::glue("  - Processing location batch {i}/{length(location_chunks)} ({length(chunk_of_locations)} locations)"))

      call_params <- list(
        indicator_id = current_indicator_id,
        location_id = chunk_of_locations,
        year = years
      )

      if (!is.null(sex_id)) call_params$sex_id <- sex_id
      if (!is.null(age_group_id)) call_params$age_group_id <- age_group_id
      if (!is.null(scenario)) call_params$scenario <- scenario

      chunk_data <- do.call(get_results_by_indicator, call_params)

      if (!"indicator_id" %in% names(chunk_data) && nrow(chunk_data) > 0) {
        chunk_data$indicator_id <- current_indicator_id
      }

      all_data_list[[length(all_data_list) + 1]] <- chunk_data

      if (i < length(location_chunks)) {
        Sys.sleep(delay_between_location_batches)
      }
    }
  }

  all_data <- dplyr::bind_rows(all_data_list)
  message(glue::glue("Retrieved {nrow(all_data)} total observations"))
  all_data
}

#' Download All IHME Metadata
#'
#' Downloads and caches all available metadata from the IHME API.
#' Downloads ONLY metadata, not actual SDG indicator data.
#'
#' @param file Path to save data (default: "ihme_metadata.rds")
#' @param force Force redownload, even if file exists and is current (default: FALSE)
#'
#' @return List containing all IHME metadata
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' metadata <- download_all_ihme_metadata()
#' metadata <- download_all_ihme_metadata(force = TRUE)
download_all_ihme_metadata <- function(file = "ihme_metadata.rds", force = FALSE) {
  if (!has_ihme_key()) {
    stop(
      "IHME API key is required to download data.\n",
      "Please set your API key using: set_ihme_key('your_key_here')\n",
      "Get your API key at: https://api.healthdata.org/sdg/v1/",
      call. = FALSE
    )
  }

  if (!force) {
    cached_data <- check_cached_data(file)
    if (!is.null(cached_data)) {
      return(cached_data)
    }
  }

  message("Downloading IHME metadata...")

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
#' @param delay_between_batches Delay in seconds between indicator batches (default: 1)
#' @param sex_id Optional sex ID filter
#' @param age_group_id Optional age group ID filter
#' @param location_batch_size Number of locations to process per batch (default: 50)
#' @param delay_between_location_batches Delay in seconds between location batches (default: 0.5)
#'
#' @return Combined data frame with all results
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' large_dataset <- batch_download_sdg_data(
#'   indicator_ids = 1:20, location_ids = c(102, 6, 101),
#'   years = c(2020, 2021, 2022), batch_size = 3
#' )
batch_download_sdg_data <- function(indicator_ids, location_ids, years,
                                    batch_size = 5, delay_between_batches = 1,
                                    sex_id = NULL, age_group_id = NULL,
                                    location_batch_size = 50, delay_between_location_batches = 0.5) {

  indicator_batches <- split(indicator_ids, ceiling(seq_along(indicator_ids) / batch_size))
  message(glue::glue("Processing {length(indicator_ids)} indicators in {length(indicator_batches)} batches"))

  all_data <- list()

  for (batch_num in seq_along(indicator_batches)) {
    batch_indicators <- indicator_batches[[batch_num]]
    message(glue::glue("Processing batch {batch_num}/{length(indicator_batches)} ({length(batch_indicators)} indicators)"))

    batch_data <- get_sdg_data(
      indicator_ids = batch_indicators, location_ids = location_ids, years = years,
      sex_id = sex_id, age_group_id = age_group_id,
      location_batch_size = location_batch_size,
      delay_between_location_batches = delay_between_location_batches
    )

    all_data[[batch_num]] <- batch_data

    if (batch_num < length(indicator_batches)) {
      message(glue::glue("Waiting {delay_between_batches} second(s) before next indicator batch..."))
      Sys.sleep(delay_between_batches)
    }
  }

  final_data <- dplyr::bind_rows(all_data)
  message(glue::glue("Batch download complete. Retrieved {nrow(final_data)} total observations"))
  final_data
}

#' Download All IHME Data
#'
#' Downloads all available metadata AND SDG indicator data from the IHME API.
#'
#' @param file Path to save data (default: "ihme_data.rds")
#' @param force Force redownload, even if file exists and is current (default: FALSE)
#' @param years Vector of years to download data for (default: 1990:2030)
#' @param batch_size Number of indicators to process per batch (default: 3)
#' @param delay_between_batches Delay in seconds between indicator batches (default: 2)
#' @param include_projections Include projection years (default: TRUE)
#' @param location_batch_size Number of locations to process per batch (default: 50)
#' @param delay_between_location_batches Delay in seconds between location batches (default: 0.5)
#'
#' @return List containing all IHME data
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' all_data <- download_all_ihme_data()
#' recent_data <- download_all_ihme_data(years = c(2020, 2025), file = "ihme_recent.rds")
download_all_ihme_data <- function(file = "ihme_data.rds", force = FALSE,
                                   years = 1990:2030, batch_size = 3, delay_between_batches = 2,
                                   include_projections = TRUE,
                                   location_batch_size = 50, delay_between_location_batches = 0.5) {

  if (!has_ihme_key()) {
    stop(
      "IHME API key is required to download data.\n",
      "Please set your API key using: set_ihme_key('your_key_here')\n",
      "Get your API key at: https://api.healthdata.org/sdg/v1/",
      call. = FALSE
    )
  }

  if (!force) {
    cached_data <- check_cached_data(file)
    if (!is.null(cached_data)) {
      return(cached_data)
    }
  }

  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  if (!include_projections) {
    years <- years[years <= current_year]
  }

  message("Downloading all IHME data (metadata + SDG results)...")
  message(glue::glue("Years to download: {paste(years, collapse = ', ')}"))

  message("Step 1/3: Downloading metadata...")
  metadata <- list(
    age_groups = get_age_groups(), goals = get_goals(), indicators = get_indicators(),
    locations = get_locations(), scenarios = get_scenarios(),
    sex_categories = get_sex_categories(), targets = get_targets()
  )

  message(glue::glue(
    "Retrieved metadata: {nrow(metadata$locations)} locations, ",
    "{nrow(metadata$indicators)} indicators, {nrow(metadata$targets)} targets"
  ))

  all_indicator_ids <- metadata$indicators$indicator_id
  all_location_ids <- metadata$locations$location_id

  message("Step 2/3: Downloading SDG indicator data...")
  message(glue::glue(
    "Downloading data for {length(all_indicator_ids)} indicators, ",
    "{length(all_location_ids)} locations, {length(years)} years"
  ))

  sdg_data <- batch_download_sdg_data(
    indicator_ids = all_indicator_ids, location_ids = all_location_ids, years = years,
    batch_size = batch_size, delay_between_batches = delay_between_batches,
    location_batch_size = location_batch_size,
    delay_between_location_batches = delay_between_location_batches
  )

  message("Step 3/3: Packaging and saving data...")

  all_data <- list(
    metadata = metadata, sdg_data = sdg_data, downloaded = Sys.time(),
    version_hash = get_version_hash(), years_included = years
  )

  saveRDS(all_data, file, compress = TRUE)
  message(glue::glue(
    "Successfully saved complete IHME dataset:\n",
    "  - Metadata: {nrow(metadata$locations)} locations, {nrow(metadata$indicators)} indicators\n",
    "  - SDG Data: {nrow(sdg_data)} observations across {length(years)} years\n",
    "  - File: {file}"
  ))

  all_data
}
