# ------------------------------------------------------------------
# ** data-raw/GENERATE_MOCK_DATA.R **
# ------------------------------------------------------------------
# This script generates the mock data for the dashboard.
# It is intended to be run *once* by the developer.
# It will create the `data/` directory and the required files.
# ------------------------------------------------------------------

# Install required packages if not present
if (!require("sf")) install.packages("sf")
if (!require("arrow")) install.packages("arrow")
if (!require("dplyr")) install.packages("dplyr")
if (!require("lubridate")) install.packages("lubridate")

library(sf)
library(arrow)
library(dplyr)
library(lubridate)

# --- Create 'data' directory ---
if (!dir.exists("data")) {
    dir.create("data")
    message("Created 'data/' directory.")
}

# --- 1. Generate Mock Geospatial Data (MPAs) ---
# Create two simple polygon features for our mock MPAs
mpa_polygons <- list(
    sf::st_polygon(list(matrix(c(
        -70.0, 15.0,
        -70.0, 15.5,
        -69.5, 15.5,
        -69.5, 15.0,
        -70.0, 15.0
    ), ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(
        -70.5, 14.5,
        -70.5, 15.0,
        -70.2, 15.0,
        -70.2, 14.5,
        -70.5, 14.5
    ), ncol = 2, byrow = TRUE)))
)

mpa_sf <- sf::st_sf(
    mpa_name = c("Hadley Reef Sanctuary", "Viz Cays Reserve"),
    established_date = as.Date(c("2020-01-01", "2021-06-01")),
    geometry = sf::st_sfc(mpa_polygons, crs = 4326)
)

# Save as GeoJSON
sf::st_write(mpa_sf, "data/mpa_boundaries.geojson", delete_dsn = TRUE)
message("Successfully generated 'data/mpa_boundaries.geojson'.")


# --- 2. Generate Mock Fisheries Data ---
set.seed(123)
n_rows <- 50000

# Define boundaries for our "fishing grounds"
lat_range <- c(14.0, 16.0)
lon_range <- c(-71.0, -69.0)

# Simulate vessel IDs
vessel_ids <- paste0("V", 100:150)
vessel_types <- c("small_scale", "industrial")

# Simulate dates
dates <- seq(as.Date("2019-01-01"), as.Date("2024-12-31"), by = "day")

# Generate the data
fishing_data <- tibble(
    vessel_id = sample(vessel_ids, n_rows, replace = TRUE),
    date = sample(dates, n_rows, replace = TRUE),
    latitude = runif(n_rows, lat_range, lat_range),
    longitude = runif(n_rows, lon_range, lon_range),
    fishing_hours = pmax(1, rpois(n_rows, 8) + rnorm(n_rows, 0, 2)),
    vessel_type = sample(vessel_types, n_rows, replace = TRUE, prob = c(0.8, 0.2))
)

# Simulate catch (CPUE)
# Make 'industrial' vessels more efficient
# Make catch slightly higher after 2021
fishing_data <- fishing_data %>%
    mutate(
        base_cpue = ifelse(vessel_type == "industrial", 15, 8),
        season_effect = sin(2 * pi * yday(date) / 365.25) + 1.5,
        year_effect = ifelse(year(date) > 2020, 1.1, 1.0),
        catch_kg = fishing_hours * base_cpue * season_effect * year_effect * pmax(0.5, rnorm(n_rows, 1, 0.1)),
        catch_kg = round(pmax(0, catch_kg), 1),
        fishing_hours = round(fishing_hours, 1)
    )

# Assign points to MPAs
# Convert data.frame to sf object
fishing_sf <- sf::st_as_sf(fishing_data, coords = c("longitude", "latitude"), crs = 4326)

# Perform spatial join
joined_data <- sf::st_join(fishing_sf, mpa_sf, join = sf::st_intersects)

# Convert back to data.frame, filling NA for non-MPA areas
fishing_data_final <- joined_data %>%
    sf::st_drop_geometry() %>%
    mutate(
        mpa_name = ifelse(is.na(mpa_name), "Outside", mpa_name),
        latitude = sf::st_coordinates(fishing_sf)[, 2],
        longitude = sf::st_coordinates(fishing_sf)[, 1]
    ) %>%
    select(
        vessel_id, date, latitude, longitude, fishing_hours, 
        catch_kg, vessel_type, mpa_name
    )

# Save as Parquet file
arrow::write_parquet(fishing_data_final, "data/community_fishing_effort.parquet")
message("Successfully generated 'data/community_fishing_effort.parquet'.")

message("Data generation complete.")