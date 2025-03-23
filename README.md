# Housing Price Prediction with Machine Learning

This project aims to predict housing prices using various machine learning techniques, including **Ordinary Least Squares (OLS)**, **Decision Tree**, and **XGBoost**. The dataset includes housing-related features (e.g., log of house price, lot size, age, etc.) and geographical/spatial information.

## Project Overview

In this repository, we explore different machine learning models to predict housing prices, evaluate model performance, and compare their results.

Key components of the project:
- **OLS Regression**: Linear regression for predicting house prices.
- **Decision Tree**: A non-linear regression model to predict house prices.
- **XGBoost**: A gradient boosting method for accurate and efficient predictions.
- **Spatial Data Integration**: Incorporates spatial data (shapefiles) for geographically-aware predictions.
- **Data Folder**: Includes the raw housing and spatial data used for modeling.

## Repository Structure

- `#housing data/`: Folder containing housing and spatial data files required for training and testing the models.
  - `lucas-county-ohio-census-tracts.shp`: Spatial data (shapefile) for geographic information.
  - `LucasCountytemp2-NN50.dta`: Data file with housing features.
- `OLS/`: Folder containing code and analysis for the **OLS Regression** model.
- `DecisionTree/`: Folder containing code and analysis for the **Decision Tree** model.
- `XGBoost/`: Folder containing code and analysis for the **XGBoost** model, including spatial data integration.
- `README.md`: This file.
  
## Installation

To run the project, you will need to install the following dependencies in **R**:

1. Clone this repository:
    ```bash
    git clone https://github.com/FGiacomo/OLS-vs-ML-public-.git
    cd OLS-vs-ML-public-
    ```

2. Install required R packages:
    ```r
    install.packages(c('sf', 'spdep', 'sp', 'ggplot2', 'haven', 'tidyverse', 'metrica', 'dplyr', 'purrr', 'tidyr', 'oce', 'Metrics', 'lmtest', 'nortest', 'xgboost', 'caret', 'pROC'))
    ```

3. Ensure that the **housing data** files are correctly placed under the `#housing data/` folder for the scripts to work.


