#' Get Age Groups
#'
#' @param age_group_id Optional age group ID to filter results
#'
#' @return Data frame of age groups
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_age_groups()
#' get_age_groups("1")
get_age_groups <- function(age_group_id = NULL) {
  if (!is.null(age_group_id)) {
    result <- call_ihme_api("GetAgeGroup", age_group_id = age_group_id)
  } else {
    result <- call_ihme_api("GetAgeGroup")
  }
  tidy_ihme_response(result)
}

#' Get SDG Goals
#'
#' @param goal_id Optional vector of goal IDs to filter results
#'
#' @return Data frame of SDG goals
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_goals()
#' get_goals(c(1, 3))
get_goals <- function(goal_id = NULL) {
  if (!is.null(goal_id)) {
    result <- call_ihme_api("GetGoal", goal_id = goal_id)
  } else {
    result <- call_ihme_api("GetGoal")
  }
  tidy_ihme_response(result)
}

#' Get SDG Indicators
#'
#' @param indicator_id Optional indicator ID to filter results
#'
#' @return Data frame of indicators
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_indicators()
#' get_indicators("1.1.1")
get_indicators <- function(indicator_id = NULL) {
  if (!is.null(indicator_id)) {
    result <- call_ihme_api("GetIndicator", indicator_id = indicator_id)
  } else {
    result <- call_ihme_api("GetIndicator")
  }
  tidy_ihme_response(result)
}

#' Get Locations
#'
#' @param location_id Optional location ID to filter results
#'
#' @return Data frame of locations (countries, regions, etc.)
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_locations()
#' get_locations("102")
get_locations <- function(location_id = NULL) {
  if (!is.null(location_id)) {
    result <- call_ihme_api("GetLocation", location_id = location_id)
  } else {
    result <- call_ihme_api("GetLocation")
  }
  tidy_ihme_response(result)
}

#' Get Scenarios
#'
#' @param scenario Optional scenario to filter results
#'
#' @return Data frame of projection scenarios
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_scenarios()
#' get_scenarios("reference")
get_scenarios <- function(scenario = NULL) {
  if (!is.null(scenario)) {
    result <- call_ihme_api("GetScenario", scenario = scenario)
  } else {
    result <- call_ihme_api("GetScenario")
  }
  tidy_ihme_response(result)
}

#' Get Sex Categories
#'
#' @param sex_id Optional sex ID to filter results
#'
#' @return Data frame of sex categories
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_sex_categories()
#' get_sex_categories("1")
get_sex_categories <- function(sex_id = NULL) {
  if (!is.null(sex_id)) {
    result <- call_ihme_api("GetSex", sex_id = sex_id)
  } else {
    result <- call_ihme_api("GetSex")
  }
  tidy_ihme_response(result)
}

#' Get SDG Targets
#'
#' @param target_id Optional target ID to filter results
#'
#' @return Data frame of SDG targets
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_targets()
#' get_targets(1)
get_targets <- function(target_id = NULL) {
  if (!is.null(target_id)) {
    result <- call_ihme_api("GetTarget", target_id = target_id)
  } else {
    result <- call_ihme_api("GetTarget")
  }
  tidy_ihme_response(result)
}
