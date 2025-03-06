# Replication Package for Climate Risk and Poverty in Middle East and North Africa

## Overview
This repository contains the replication package for the paper "Climate Risk and Poverty in Middle East and North Africa" by Chitra Balasubramanian, Sandra Baquie, Alan Fuchs (025). 

### Paper Abstract
   The Middle East and North Africa (MENA) faces significant climate challenges, such as increasing temperatures, heightened flood risks, frequent droughts, and growing air pollution issues. These challenges are compounded by 30-60\% of poverty in different countries in the region. Indeed, people living in poverty are more exposed to poor air quality and natural disasters as they tend to disproportionately live in hazard-prone areas. They are also more vulnerable as they may have scarcer resources to cope with shocks. This paper combines remote sensing, geospatial data, and household surveys to provide high-resolution assessments of the exposure and vulnerability of the MENA population and poor people to four types of climate shocks. Of the almost 24 million extreme poor (below \$2.15) in the region that we have available data for, we estimate that the entire 24 million are exposed to at least one climate shock. The region hosts climate-poverty hot spots in Yemen and Morocco, where adaptation to climate change will be crucial to end poverty. The resulting high-resolution estimates of exposure and vulnerability can inform the targeting of climate adaptation measures.

## Repository Structure
```
.
├── DataWork/
│   ├── 00_Master
│   ├── 00_downloading_datasets/
│   ├── 01_creating_analysis_ready_datasets/
│   │   ├── 01_CREATE_GRID.R
│   │   ├── 02_EXTRACT_AIR_POL.R
│   │   ├── 02_EXTRACT_AIRPORTS.R
│   │   ├── 02_EXTRACT_CITY_CENTERS.R
│   │   ├── 02_EXTRACT_CONFLICT.R
│   │   ├── 02_EXTRACT_CROP_YIELD.R
│   │   ├── 02_EXTRACT_DROUGHT.R
│   │   ├── 02_EXTRACT_EDUC_FACILITIES.R
│   │   ├── 02_EXTRACT_FLOOD.R
│   │   ├── 02_EXTRACT_FLOOD_PARTI.R
│   │   ├── 02_EXTRACT_FOREST_LOSS.R
│   │   ├── 02_EXTRACT_HEALTH_FACILITIES.R
│   │   ├── 02_EXTRACT_HEAT_STRESS.R
│   │   ├── 02_EXTRACT_POP.R
│   │   ├── 02_EXTRACT_PORTS.R
│   │   ├── 02_EXTRACT_RWI.R
│   │   ├── 02a_CLEAN_GAS_FLARES.R
│   │   ├── 02b_EXTRACT_BLM_NTL.R
│   │   ├── 02c_AGGREGATE_NTL_GAS_FLARES.R
│   │   ├── 03a_MERGE_CITIES_PORTS_AIRPORTS.R
│   │   ├── 03b_COMPUTE_TT_HEALTH_FACILITIES.R
│   │   ├── 03c_COMPUTE_TT_EDUC_FACILITIES.R
│   │   ├── 03d_COMPUTE_TT_MARKETS.R
│   │   ├── 03e_MERGE_TT_GRID.R
│   │   ├── 03f_SCORECARD_DIMS.R
│   │   ├── 03g_MERGE_SCORECARD.R
│   │   └── 03h_MERGE_ALL_VARIABLES.R
│   ├── 02_analysis/
│   │   ├── 01_ESTIMATE_NUMBER_POOR_RWI.R
│   │   ├── 02_CREATE_EXPOSED_POOR_215_685_HAZARD.R
│   │   ├── 03_CREATE_VULNERABILITY_THRESHOLDS.R
│   │   ├── 04_CREATE_POOR_EXPOSED_VULNERABLE.R
│   │   └── 05_CALCULATE_TOT_EXTREME_POOR.R
│   └── 03_figures_tables/
│       ├── 01_GRAPH_HAZARDS.R
│       ├── 02_GRAPHS_EXPOSED_POPULATION.R
│       ├── 03_GRAPHS_EXPOSED_POOR_215.R
│       ├── 04_GRAPHS_VULNERABILITY_INDICATORS.R
│       ├── 05_GRAPH_PEOPLE_EXPOSED_ALLHAZARDS.R
│       ├── 06_GRAPH_HAZARD_MATRIX.R
│       ├── 07_GRAPH_POOR_EXPOSED_ALLHAZARDS.R
│       ├── 08_GRAPH_POOR_EXPOSED_EGYPT.R
│       ├── 09_GRAPH_EXPOSED_POOR_OVERLAPS.R
│       ├── 10_GRAPH_EXPOSED_POOR_VULNERABLE.R
│       ├── 11_GRAPH_DENSITY_PLOT.R
│       ├── 12_GRAPH_COMPOSITION_VULNERABILITY.R
│       └── 13_GRAPH_POOR_EXPOSED_VULNERABLE_MOR_YEM.R
└── Paper Tables and Figures/
    └── REPLICATION_OUTPUT/
        ├── GRAPHS/
        ├── MAPS/

```

## Software Requirements
- Programming Language(s): 
  - R 4.2.1
- Required Packages:
  - R: Use script 00_SET_PATHS.R


## Instructions for Replication

### Data Preparation
0. Before starting, make sure to change paths in 00_SET_PATHS.R
1. All raw datasets are in REPLICATION_RAW in the Sharepoint folder:[https://worldbankgroup.sharepoint.com/sites/MENAPOVGeospatialFiles/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2FMENAPOVGeospatialFiles%2FShared%20Documents%2FMENAPOV%20GEO%2FIntranet&p=true&ga=1](Link to raw data)
2. Run the scripts in 01_creating_analysis_ready_datasets/ to obtain the analysis datasets

### Analysis
Run the analysis scripts in 02_analysis/

### Expected Runtime 
One Day since there are two scripts ```02_EXTRACT_FLOOD.R + 02_EXTRACT_FLOOD_PARTI.R``` that are computationally heavy

## Results
The main results can be reproduced by running all the scripts in 03_figures_tables/

### Output Files
The output files are stored in Papers_and_Tables

## Additional Information
For any issues with replication, please contact cbalasubramania2@worldbank.org

## License


## Citation
```bibtex
@techreport{climate_risk_and_poverty,
  title={Climate Risk and Poverty in Middle East and North Africa},
  author={Balasubramanian,Chitra and Baquie, Sandra and Fuchs, Alan},
  year={2025},
  institution={World Bank Group},
  type={Policy Research Working Paper},
  address={Washington, D.C.},
}
```

## Acknowledgments
Analysis conducted as part of the Whole-of-Economy Trust Fund
