# Climate Shocks and Help-Seeking Behavior: A Spatial Econometric Pipeline

This repository contains the data processing and analysis pipeline for evaluating the impact of extreme precipitation events on help-seeking behavior among victims of violence. It merges spatial survey data (DHS) with high-resolution climate data (ERA5) to run single-country and multi-country pooled spatial regressions.

## Repository Structure

The project is divided into specific modular folders:

* **`DHS_Consolidation/`**

  * Contains the `DHS_Rainfall_Consolidation` notebook, which is used for processing and consolidating the raw demographic survey data.

* **`ERA5_Download_Request/`**

  * Contains Python scripts (`ERA_5_Rainfall_Download.py` and `ERA5_Soil_Moisture_Download.py`) that request and download raw ERA5 climate data directly to your connected Google Drive.

* **`ERA5_Rainfall_Exposure_Pipeline/`**

  * Contains the `ERA_DHS_EXPOSURE_VARIABLE_BUILDING` notebook, which extracts the downloaded climate data and merges it with the DHS spatial clusters to build exposure variables.

* **`Model_1_Climate_Shock_Exposure_Each_Country/`**

  * Contains the `Logistic_Regression_Individual_Country_Rain...` notebook to run independent GLM regressions for individual countries.

* **`Model_2_Climate_Shock_Exposure_All_Countries/`**

  * Contains the `Model_2_Climate_Shock_Exposure_All_Count...` notebook for pooling all country datasets and running a master multi-country GLM with fixed effects.

## Data Requirements and Access

This repository **does not include raw data** due to licensing and privacy restrictions. Users must obtain their own access to the required datasets:

**1. Demographic and Health Surveys (DHS)**
You must request access to the relevant country datasets and spatial covariates directly from [The DHS Program](https://dhsprogram.com/). Once approved, place the raw Stata/CSV files in your working directory before running the consolidation scripts.

**2. ERA5 Climate Data**
You do not need to download the ERA5 data manually. The scripts provided in the `ERA5_Download_Request` folder will connect to the Copernicus CDS API and download the requested precipitation and soil moisture GeoTIFFs directly to your Google Drive.

* *Requirement:* You must have a registered account with the Copernicus Climate Data Store and configure your CDS API key locally or in your Colab environment before running these download scripts.

## Usage Pipeline & Data Flow

1. **Configure API & Download Climate Data:** Run the python scripts in `ERA5_Download_Request/` to pull ERA5 data to your Google Drive.

2. **Consolidate Survey Data:** Run the notebook in `DHS_Consolidation/` on your approved, locally stored DHS files.

3. **Build Exposure Variables:** Use the notebook in `ERA5_Rainfall_Exposure_Pipeline/` to merge the DHS clusters with your new Google Drive ERA5 files.

4. **Run Models:** Execute the model notebooks to generate your final analysis.

### CSV File Flow Mapping

| Origin Folder / Step | Created CSV Output | Consumed By | Purpose | 
| ----- | ----- | ----- | ----- | 
| **`DHS_Consolidation/`** | Consolidated DHS Survey CSVs | `ERA5_Rainfall_Exposure_Pipeline/` | Provides baseline demographics and spatial cluster coordinates. | 
| **`ERA5_Rainfall_Exposure_Pipeline/`** | Country-specific `*_DHS_Spatial_Merged.csv` | Both `Model_1` & `Model_2` folders | The final merged datasets containing both help-seeking outcomes and extreme rainfall exposure metrics. | 
| **`Model_1_Climate_Shock_Exposure_Each_Country/`** | *None (Generates visualizations/tables)* | N/A | Ingests individual country CSVs to run isolated country-by-country regressions. | 
| **`Model_2_Climate_Shock_Exposure_All_Countries/`** | `ALL_AFRICA_DHS_Spatial_Merged.csv` | Master Pooled GLM | Concatenates all individual country CSVs into one master file to run a continent-wide regression with fixed effects. | 
