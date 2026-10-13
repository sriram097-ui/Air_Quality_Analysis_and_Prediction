# Air Quality Analysis and Prediction — Professional R Dashboard

## Included features
- Executive KPI overview and interactive pollutant charts
- Date range, station and pollutant filters
- Pollutant trends, hourly patterns, correlation heatmap and station comparisons
- Prediction Lab with Random Forest baseline, chronological holdout metrics, feature importance and a clearly labelled scenario projection
- Searchable/exportable observation table
- Included synthetic demonstration data so the app starts immediately

## Run in VS Code
1. Install R: https://cran.r-project.org/
2. Install the **R** extension in VS Code and open this project folder.
3. In the R terminal, run:

```r
install.packages(c("shiny","shinydashboard","DT","plotly","dplyr","readr","ggplot2","scales","lubridate","randomForest","tidyr"))
shiny::runApp()
```

## Dataset
The included `data/air_quality.csv` is synthetic demo data. To use your own, replace it with a CSV at the same path. Recognized columns include:
- Date / Datetime / Timestamp
- Station / City / Location
- PM2.5 / PM25, PM10, NO2, SO2, CO, O3
- Temperature / Temp, Humidity / RH

The app supports common column variants; edit the mapping near the top of `app.R` for other schemas. Confirm pollutant units, timestamps, missing values and station metadata before using real data.

## Prediction caveats
The Random Forest model predicts PM2.5 from available pollutant/weather inputs and evaluates against a chronological holdout. The scenario plot repeats recent input rows; it is not a weather-aware atmospheric forecast and should not be used for health or regulatory decisions without validation.
