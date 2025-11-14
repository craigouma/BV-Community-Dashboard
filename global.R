# ------------------------------------------------------------------
# ** global.R **
# ------------------------------------------------------------------
# This script is executed once when the Shiny application starts.
# It is the designated place for:
# 1. Loading all necessary R packages.[22, 23]
# 2. Loading and pre-processing data that is static and shared
#    across all user sessions.
# This prevents data from being re-loaded every time a user
# interacts with the app, which is a major performance enhancement.[21]
# ------------------------------------------------------------------

# --- 0. Ensure renv Environment is Active ---
# This allows the app to be launched via `runApp("path")` without
# manually calling `renv::run()` beforehand. If renv is already active,
# this call is effectively a no-op.
if (file.exists("renv/activate.R")) {
    source("renv/activate.R")
}

# --- 1. Load Core Libraries ---
# These packages are now available to both ui.R and server.R
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(plotly)
library(reactable)
library(DT)
library(sf)
library(dplyr)
library(arrow)
library(magrittr)
library(lubridate)

# --- 2. Load Static Data ---
# Load high-performance Parquet data.
# This data is read once into memory.

# For shinyapps.io deployment, use tryCatch with fallback
if (file.exists("data/community_fishing_effort.parquet")) {
    tryCatch({
        fishing_data <- arrow::read_parquet("data/community_fishing_effort.parquet")
        cat("Loaded fishing data successfully\n")
    }, error = function(e) {
        cat("Error loading parquet file, generating mock data...\n")
        source("data-raw/GENERATE_MOCK_DATA.R")
        fishing_data <- arrow::read_parquet("data/community_fishing_effort.parquet")
    })
} else {
    cat("Data file not found, generating mock data...\n")
    source("data-raw/GENERATE_MOCK_DATA.R")
    fishing_data <- arrow::read_parquet("data/community_fishing_effort.parquet")
}

# Load geospatial data (MPA boundaries).[24, 25]
if (file.exists("data/mpa_boundaries.geojson")) {
    tryCatch({
        mpa_boundaries <- sf::st_read("data/mpa_boundaries.geojson", quiet = TRUE) %>%
            # Ensure CRS is WGS84 (EPSG:4326) for compatibility with Leaflet
            sf::st_transform(4326)
        cat("Loaded MPA boundaries successfully\n")
    }, error = function(e) {
        cat("Error loading geojson file, generating mock data...\n")
        source("data-raw/GENERATE_MOCK_DATA.R")
        mpa_boundaries <- sf::st_read("data/mpa_boundaries.geojson", quiet = TRUE) %>%
            sf::st_transform(4326)
    })
} else {
    cat("MPA boundaries file not found, generating mock data...\n")
    source("data-raw/GENERATE_MOCK_DATA.R")
    mpa_boundaries <- sf::st_read("data/mpa_boundaries.geojson", quiet = TRUE) %>%
        sf::st_transform(4326)
}

# --- 3. One-Time Data Processing ---
# Perform any calculations here that only need to be done once.
# For example, create a summary for the entire dataset.
global_summary_stats <- fishing_data %>%
    summarise(
        total_effort_hours = sum(fishing_hours, na.rm = TRUE),
        total_catch_kg = sum(catch_kg, na.rm = TRUE),
        unique_vessels = n_distinct(vessel_id)
    )

# --- 4. Source Module Files ---
# In a non-package structure, we source the module files here.
# In our full package structure, this is handled by the NAMESPACE.
# For this submission, we will source them to ensure it's runnable.
source("R/map_module.R")
source("R/plot_module.R")
source("R/health_module.R")
source("R/scenario_module.R")
source("R/qa_module.R")
source("R/table_module.R")