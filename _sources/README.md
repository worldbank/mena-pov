# Geospatial Poverty and Vulnerability Assessment - MENA Poverty and Equity Group

## Overview
The Geospatial Poverty and Vulnerability Assessment repository aims to centrally maintain cleaning and analysis of poverty and vulnerability assessments that leverage geospatial data conducted by the Poverty and Equity Group in the MENA region.

## Features 
- Import and manage large geospatial datasets
- Comprehensive data preprocessing and cleaning functions
- Spatial analysis tools for poverty and vulnerability assessment
- Visulalizations options for intuitive understanding of results
- Export of results for further analysis or presentation

## Repository Structure
In this structure, `cleaning` is at the same level as analysis under the `notebooks_scripts` directory. Within the `cleaning` directory, you have separate folders for each data source. This reflects that the cleaning and preprocessing is done in a single script for each data source, regardless of country.

The `analysis` directory, on the other hand, contains separate directories for each country, reflecting that the analysis is specific to each country.

As always, remember to replace the placeholders (country_1, country_2, source_1, source_2, etc.) with the actual names of the countries and data sources.
```
mena-pov
│
├── README.md                  # The file you are reading now
├── LICENSE                    # The license for this project
├── requirements.txt           # Python/R dependencies needed for this project
│
├── CONTRIBUTE.md              # Guidelines for contributing to this project
├── docs                       # Documentation, usage guides, etc.
│   └── ...
│
├── data                       # Folder for all data (should be kept empty in the repo due to size limitations)
│   └── ...
│
├── notebooks_script                        # Source code for this project
│   ├── cleaning               # Data cleaning and preprocessing scripts
│   │   ├── source_1           # Replace 'source_1' with the name of the geospatial source
│   │   └── source_2           # Repeat for each source
│   ├── analysis               # Data analysis scripts, separated by country
│   │   ├── country_1          # Replace 'country_1' with the name of the country
│   │   ├── country_2          # Repeat for each country
│   │   └── ...
│   └── ...
│
└── functions                      # scripts for user-written functions used for cleaning and analysis
    └── ...


```
## Contributing
We welcome contributions ! Please see our CONTRIBUTE.md file for details on how to contribute.



## Installation and Dependencies



## License

The <span style="color:#3EACAD">template</span> is licensed under the [**World Bank Master Community License Agreement**](LICENSE)
