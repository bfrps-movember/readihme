# readihme

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/bfiripis/readihme/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bfiripis/readihme/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The purpose of `readihme` is to provide easy access to the Institute for Health Metrics and Evaluation (IHME) Sustainable Development Goals (SDG) API directly from R. This package allows users to retrieve SDG indicator data, metadata, and perform common data analysis tasks with IHME data.

## Installation

You can install the development version of readihme like so:

``` r
# install.packages("pak")
pak::pkg_install("yourusername/readihme")
```

## Authentication

**Important:** The IHME API requires authentication. You need to get an API key before using this package.

### Getting Your API Key

1. Visit [https://api.healthdata.org/sdg/v1/](https://api.healthdata.org/sdg/v1/)
2. Sign up or log in to get your API key
3. Copy your API key (it will look like: `xq541ghh07ihok3tbtff4g4c3p4z7a52`)

### Setting Your API Key

```r
library(readihme)

# Set your API key for the current session
set_ihme_key("your_api_key_here")

# Or set it and save for future sessions
set_ihme_key("your_api_key_here", install = TRUE)

# Check if your key is valid
validate_ihme_key()
```

## Basic Usage

### Getting Metadata

Start by exploring the available data:

```r
library(readihme)

# Make sure your API key is set
set_ihme_key("your_api_key_here")

# Get all available locations (countries/regions)
locations <- get_locations()
head(locations)

# Get all available indicators
indicators <- get_indicators()
head(indicators)

# Get all SDG targets
targets <- get_targets()
head(targets)

# Get age groups and sex categories
age_groups <- get_age_groups()
sex_categories <- get_sex_categories()
```

### Finding Specific Countries

Use helper functions to find countries by name:

```r
# Find countries with "United" in the name
united_countries <- find_countries("United")
print(united_countries)

# Find African countries
african_countries <- find_countries("Africa")
print(african_countries)
```

### Retrieving SDG Data

Get actual indicator data for specific countries and years:

```r
# Get data for specific indicators, countries, and years
sdg_data <- get_sdg_data(
  indicator_ids = c(1, 2),  # Specify indicator IDs
  location_ids = c(102, 6), # USA and China location IDs
  years = c(2015, 2020, 2025)
)

head(sdg_data)
```

### Alternative Data Retrieval Methods

You can also retrieve data using different approaches:

```r
# Get all data for a specific year
data_2020 <- get_results_by_year(
  year = 2020,
  location_id = c(102, 6),  # USA and China
  indicator_id = c(1, 2)
)

# Get all data for a specific country
usa_data <- get_results_by_location(
  location_id = 102,  # USA
  year = c(2015, 2020, 2025),
  indicator_id = c(1, 2, 3)
)

# Get data for a specific indicator across countries
indicator_data <- get_results_by_indicator(
  indicator_id = 1,
  location_id = c(102, 6, 101),  # Multiple countries
  year = c(2020, 2025)
)
```

## Example: Analyzing Child Mortality Trends

Here's a complete example analyzing child mortality trends:

```r
library(readihme)
library(dplyr)
library(ggplot2)

# Set your API key first
set_ihme_key("your_api_key_here")

# First, find the child mortality indicator
indicators <- get_indicators()
child_mortality_indicators <- indicators %>%
  filter(grepl("mortality|death", indicator_name, ignore.case = TRUE))

# Get data for selected countries
countries_of_interest <- find_countries("(United States|Canada|Mexico)")
country_ids <- countries_of_interest$location_id

# Retrieve child mortality data
mortality_data <- get_sdg_data(
  indicator_ids = child_mortality_indicators$indicator_id[1], # Use first mortality indicator
  location_ids = country_ids,
  years = seq(2000, 2020, 5)  # Every 5 years from 2000-2020
)

# Create a visualization
mortality_data %>%
  left_join(countries_of_interest, by = "location_id") %>%
  ggplot(aes(x = year, y = value, color = location_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Child Mortality Trends in North America",
    x = "Year",
    y = "Mortality Rate",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

## Bulk Data Download

For research projects requiring extensive data, you can download all metadata at once:

```r
# Download all metadata (locations, indicators, targets, etc.)
metadata <- download_all_ihme_metadata(file = "ihme_metadata.rds")

# Access specific metadata
all_locations <- metadata$locations
all_indicators <- metadata$indicators
all_targets <- metadata$targets
```

## Authentication Management

The package provides several functions to manage your API key:

```r
# Check if you have a key set
has_ihme_key()

# Get your current key
key <- get_ihme_key(error_if_missing = FALSE)

# Validate your key
validate_ihme_key()

# Remove your key
remove_ihme_key()

# Remove key from current session and .Renviron
remove_ihme_key(uninstall = TRUE)
```

## Data Structure

The IHME API returns data with the following key fields:

- `location_id` / `location_name`: Geographic identifiers
- `indicator_id` / `indicator_name`: SDG indicator identifiers  
- `year`: Year of observation or projection
- `value`: The indicator value
- `sex_id` / `sex_name`: Sex categories (Both, Male, Female)
- `age_group_id` / `age_group_name`: Age group categories
- `scenario`: Projection scenario (for future years)

## API Rate Limits and Best Practices

- The IHME API may have rate limits, so be mindful when making many requests
- Use specific filters (location_id, year, indicator_id) to reduce response sizes
- Cache metadata locally using `download_all_ihme_metadata()` for repeated use
- Consider using `Sys.sleep()` between large batch requests if needed
- **Keep your API key private** - never share it or commit it to version control

## Troubleshooting

### Authentication Errors

If you get authentication errors:

```r
# Check if your key is set
has_ihme_key()

# Validate your key
validate_ihme_key()

# If invalid, set a new key
set_ihme_key("your_new_api_key_here")
```

### Environment Variables

The package stores your API key in the `IHME_API_KEY` environment variable. You can also set this manually:

```r
# Set environment variable directly
Sys.setenv(IHME_API_KEY = "your_api_key_here")

# Or add to your .Renviron file:
# IHME_API_KEY=your_api_key_here
```

## Contributing

Bug reports and feature requests are welcome! Please file an issue on GitHub.

## Related Resources

- [IHME SDG API Documentation](https://api.healthdata.org/sdg/v1/)
- [IHME Website](https://www.healthdata.org/)
- [UN Sustainable Development Goals](https://sdgs.un.org/goals)

## License

MIT © readihme authors
