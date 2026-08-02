# Stock Returns and News Consumption

## Overview

This project examines the relationship between stock-return movements and online news-search activity using panel data. Search volume is used as an indicator of investor attention and news consumption.

## Research Question

Are larger movements in stock returns associated with increased online search activity for company-related news?

## Methods

The empirical analysis was conducted in R and includes:

* Pooled panel-data regression
* Individual fixed effects
* Time fixed effects
* Two-way fixed effects
* Lagged explanatory variables
* Market-return controls
* Heteroskedasticity-robust standard errors
* Standard errors clustered at the company level
* Partial-regression visualisation

## Empirical Specification

The dependent variable is the logarithm of the search-volume index. The main explanatory variable is the absolute value of the company’s stock return. Additional specifications control for market returns, lagged search activity, company fixed effects, and time fixed effects.

## Requirements

Install the required R packages before running the analysis:

```r
install.packages(c(
  "lmtest",
  "car",
  "sandwich",
  "plm",
  "stargazer",
  "ggplot2"
))
```

## Data

The dataset is not currently distributed through this repository. To reproduce the analysis, place an authorised copy of `SVI.csv` in:

```text
data/SVI.csv
```

## Running the Analysis

Set the repository as the working project directory and run:

```r
source("Stock returns and news consumption.R")
```

## Repository Structure

```text
.
├── data/
│   └── SVI.csv
├── Stock returns and news consumption.R
└── README.md
```

## Tools

R, plm, lmtest, sandwich, car, stargazer, and ggplot2.
