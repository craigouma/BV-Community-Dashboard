# Blue Ventures Fisheries Intelligence Platform

**A Senior-Level R/Shiny Demonstration for Marine Conservation**

**Author:** Craig Carlos Ouma  
**GitHub:** [Link](https://github.com/craigouma/)  
**LinkedIn:** [Link](https://linkedin.com/in/craig-carlos-ouma/)

## 1. Introduction: The "Why" of This Project

Upon reviewing the "Software Developer (R Shiny)" position, I was deeply impressed by Blue Ventures' mission-driven approach to "rebuild tropical fisheries with coastal communities". The role's focus on creating user-friendly tools to transform complex data into actionable insights for community-level decision-making is a challenge I find both critical and compelling.

My curriculum vitae shows over five years of senior-level experience in full-stack software and data engineering, with a focus on Python (FastAPI, Dash), SQL, and scalable cloud architecture (GCP, BigQuery, Airflow). While my production experience has been Python-based, the Blue Ventures mission and the technical challenge of this role inspired me to build this R Shiny dashboard specifically for this application.

This project was undertaken to demonstrate two key points:

- **Rapid Mastery & Application:** My ability to quickly master a new technology stack (R/Shiny) and apply it to the specific domain of marine conservation.

- **Transferable Seniority:** How my senior-level architectural principles—modularity, scalability, testability, and robust data engineering—are language-agnostic and translate directly into a production-grade R/Shiny application.

This dashboard is a functional prototype of the exact kind of tool described in the job description.

## 2. Live Demo

*(I'm unable to publish since I'm using the latest version of R)*

## 3. Core Features: Aligning with the Blue Ventures Mission

This dashboard is designed to empower a community leader, a field-based manager, or a data scientist at Blue Ventures to understand local fisheries data in a simple, interactive way.

### Feature 1: Interactive LMMA/MPA Monitoring

The main dashboard features a dynamic leaflet map. This map visualizes the boundaries of Locally Managed Marine Areas (LMMAs) or Marine Protected Areas (MPAs) overlaid with recent, (simulated) community-reported fishing activity. This directly addresses the need for tools supporting "community-based conservation and development initiatives" by providing a clear spatial context for management decisions.

### Feature 2: Actionable Insights via Linked Visuals

The dashboard's components are linked. Clicking an MPA polygon on the leaflet map instantly filters the plotly time-series charts below it. This is a key "senior" feature that moves beyond simple display to active analysis. A user can immediately answer questions like:

- "How has our total fishing effort changed inside this protected area since it was established?"
- "Is the Catch Per Unit Effort (CPUE) recovering within this LMMA compared to outside?"

This feature directly demonstrates the "transformation of complex datasets into actionable insights".

### Feature 3: Accessible Data for All Stakeholders

The "Data Explorer" tab provides a reactable data table. This component allows users to intuitively search, filter, sort, group, and paginate through the entire raw dataset. This single tool serves both non-technical audiences (who can easily find a specific vessel's data) and technical audiences (who can group data for analysis or export), fulfilling the requirement to present data in "clear, accessible formats" for all user types.

## 4. Technical Architecture: A Demonstration of Seniority

This application is built to be scalable, maintainable, and team-ready. Its design reflects senior-level software engineering principles.

### 4.1. Production-Grade Project Structure (R Package)

This app is not a single app.R script. It is structured as a formal R Package. All dependencies are explicitly declared in the DESCRIPTION file, and the code is organized in a standard R/ directory.

**Why:** This is the best practice for any large or long-term Shiny app. It ensures portability, simplifies dependency management (using renv), and makes the code base understandable and manageable for a collaborative team.

### 4.2. Scalable & Maintainable via Shiny Modules

The application is 100% modular. The leaflet map, the plotly charts, and the reactable table are each self-contained Shiny Modules. The main server.R file contains no plotting logic; it only acts as a "router" to initialize modules and pass messages between them.

**Why:** Modules are the R/Shiny solution to complexity. They prevent namespace collisions, make code reusable, and allow developers to work on different parts of the app without conflict. This is the R equivalent of Python blueprints or microservices, a pattern with which I have extensive experience.

### 4.3. High-Performance, Language-Agnostic Data Pipeline (The Python Bridge)

This app does not load data from a CSV. It reads data from a Parquet file (.parquet) using the arrow package.

**Why:** This is a deliberate architectural choice that bridges my data engineering background with the R ecosystem. The arrow framework provides a zero-copy, in-memory data bridge between Python and R. This demonstrates that I am already thinking about the full data pipeline: a (likely Python-based) data engineering process can clean and deposit data as Parquet, and this R/Shiny app can read it with maximum performance.

### 4.4. Enterprise-Ready Data Backend (The BigQuery Bridge)

The data logic in this app uses standard dplyr verbs. This code can be pointed at a local data frame (as in this demo) or directly at a database backend with zero changes to the app's logic, using the dbplyr package.

**Why:** This demonstrates a path to enterprise-scale. My CV shows strong expertise in Google BigQuery, a system that R can query natively using dbplyr and bigrquery. This app is architected to scale from a 10MB Parquet file to a 10PB BigQuery table without rewriting the application logic. This proves my existing data engineering skills are a direct asset to scaling Blue Ventures' R/Shiny tools.

### 4.5. Full Reproducibility (renv)

This project is structured to use renv. By running `renv::restore()`, you can instantly rebuild the exact R environment (all the correct package versions) used to develop this app. This guarantees the app "makes no mistakes" and runs flawlessly for you, just as it does for me.

## 5. Senior-Level Comprehensive Dashboard

This dashboard has been transformed into a **single, comprehensive intelligence platform** that consolidates all senior-level features into an integrated workflow. Instead of multiple separate tabs, we now have one powerful dashboard that demonstrates the full spectrum of marine conservation analytics.

### 5.1. Executive Summary Row
**Top metrics dashboard** showing:
- **Overall Health Score**: LMMA performance indicator combining effort, CPUE, and compliance
- **Effort Reduction Score**: Fishing pressure monitoring with trend indicators
- **CPUE Recovery Score**: Fish stock health assessment
- **Compliance Score**: Regulatory adherence tracking

### 5.2. Interactive MPA Map
**Full-width geospatial monitoring**:
- **Real-time Data Overlay**: Fishing effort visualization across MPAs
- **Click-to-Filter**: Interactive selection updates all downstream analytics
- **Geospatial Context**: Clear spatial understanding of conservation areas

### 5.3. Time Series Analysis
**Full-width temporal trends**:
- **Effort & CPUE Tracking**: Long-term monitoring of key conservation indicators
- **Policy Impact Visualization**: Before/after comparisons for management decisions
- **Trend Analysis**: Automated detection of conservation progress

### 5.4. Conservation Intelligence & Policy Analysis
**Side-by-side decision support**:
- **Conservation Intelligence**: AI-powered insights and priority actions
- **Policy Scenario Simulator**: Interactive impact modeling for management decisions
- **Season Closures & Gear Bans**: Toggle policies with customizable effectiveness sliders
- **Impact Analysis**: "Run Policy Analysis" button provides detailed impact assessment
- **Smart Alerts**: Threshold-based notifications for significant changes in effort/CPUE
- **Real-time Feedback**: Instant notifications showing percentage changes and health score impacts

### 5.5. Data Quality Dashboard
**Horizontal enterprise metrics**:
- **Completeness Monitoring**: Automated data coverage assessment
- **Accuracy Validation**: Quality assurance with progress indicators
- **Dataset Summary**: 50,000+ records with full temporal coverage
- **Export Capabilities**: Professional data downloads for stakeholder sharing

### 5.6. Enhanced Fisheries Data Explorer
**Comprehensive data analysis interface**:
- **Smart Filtering**: Preset views (High Effort, Industrial Vessels, Recent Activity, MPA Fishing)
- **Interactive Data Table**: Search, sort, and filter 50,000+ fishing observations
- **Advanced Export**: CSV downloads for offline analysis and reporting
- **Real-time Updates**: Dynamic filtering with immediate visual feedback

### 5.7. Technical Architecture Excellence

**Production-Grade Implementation**:
- **Modular Architecture**: Clean separation of UI and server logic
- **Reactive Data Flow**: Efficient state management across all components
- **Performance Optimization**: Parquet data format with lazy loading
- **Error Resilience**: Comprehensive validation and graceful failure handling
- **Scalable Design**: Ready for enterprise deployment and multi-user access

## 6. Deployment to ShinyApps.io

### Prerequisites
- R version 4.4.0 or compatible (see `runtime.txt`)
- All required packages installed via renv
- ShinyApps.io account

### Deployment Steps
1. **Install rsconnect package:**
   ```r
   install.packages("rsconnect")
   ```

2. **Connect to ShinyApps.io:**
   ```r
   rsconnect::setAccountInfo(name='your-account-name',
                             token='your-token',
                             secret='your-secret')
   ```

3. **Deploy the app:**
   ```r
   library(rsconnect)
   deployApp()
   ```

### Deployment Configuration
- **R Version**: Specified in `runtime.txt` (r-4.4.0)
- **Dependencies**: Managed via `renv.lock`
- **Ignored Files**: Listed in `.rsconnect_ignore`

### Troubleshooting
- If deployment fails due to R version, update `runtime.txt` to a supported version
- Ensure all data files are included in the bundle
- Check that renv dependencies are properly captured

## 7. Setup and Installation

### Clone the Repository:

```bash
git clone https://github.com/craigouma/BV_Community_Dashboard.git
cd BV_Community_Dashboard
```

### Generate Data (One-Time Step):
The project uses simulated data. To generate the data/ directory and its contents, please run the following R script from the project root:

```r
source("data-raw/GENERATE_MOCK_DATA.R")
```

*(This script will create data/community_fishing_effort.parquet and data/mpa_boundaries.geojson)*

### Restore the R Environment (using renv):
Open the BV_Community_Dashboard.Rproj file in RStudio. Then, run the following in the R console:

```r
if (!require("renv")) install.packages("renv")
renv::restore()
```

*(This will install all required packages from the renv.lock file.)*

### Run the Application:
You can run the app in two ways:

#### From RStudio:
Simply open global.R, ui.R, or server.R and click the "Run App" button. RStudio will automatically activate the renv environment.

#### From the Console (with renv):

```r
# Option 1: Use renv::run() (recommended)
renv::run(shiny::runApp())

# Option 2: Manually activate renv first
source("renv/activate.R")
shiny::runApp()
```

**Important:** When running from the console, you must activate renv first, otherwise the required packages won't be available.
