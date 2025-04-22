# Flood Frequency Analysis (FFA) using Statistical Methods

This repo contains <b>R</b> code and data used for a <b>FFA project</b> as part of our <b>Engineering Hydrology</b> course (CE F321). FFA is important for designing hydraulic structures and implementing effective flood mitigation strategies. Analysis was mainly conducted using discharge data from the Nizam Sagar reservoir (2015-2024) due to limitations in comprehensive river discharge data.

The project has been hosted as an interactive <b>Shiny app</b> which you may use: https://meghr.shinyapps.io/HydrologyFFA/

Note that any discharge dataset can be used, as long as the headers match - "l No" "date" "Inflow (Cusecs)" - in the sheet "Sheet1". The code could be easily modified to account for variations in such structure.

## Contents

The R directory includes the following files:

* `loaddata.R`: script for loading and preprocessing the inflow data from the Nizam Sagar reservoir
### Distributions
* `gumbel.R`: script for Gumbel Extreme Value distribution fitting and calculating flood discharge magnitudes
* `lognormal.R`: script for Log-Normal distribution fitting and calculating flood discharge magnitudes
* `logpearson.R`: script for Log-Pearson Type III distribution fitting and calculating flood discharge magnitudes
* `gamma.R`: script for Gamma distribution fitting and calculating flood discharge magnitudes
* `weibull.R`: script for Weibull distribution fitting and calculating flood discharge magnitudes
### GOF and Performance Metrics
* `KS.R`: script for Kolmogorov-Smirnov goodness-of-fit test
* `AD.R`: script for Anderson-Darling goodness-of-fit test
* `rmse.R`: script for Root Mean Square Error calculation
* `NSE.R`: script for Nash-Sutcliffe Efficiency calculation
* `KGE.R`: script for Kling-Gupta Efficiency calculation
### Interactive Web App
* `shiny.R`: web app for performing analysis and displaying results
* `monolith.R`: monolithic script

## Packages

Followin R packages/libraries are used:

* `readxl`: For reading Excel files into R
* `lubridate`: For manipulation and parsing of datetime data
* `dplyr`: For efficient data manipulation and transformation
* `extRemes`: For extreme value analysis, including fitting Gumbel distribution
* `fitdistrplus`: For fitting probability distributions (LNorm, Gamma, Weibull) to data using maximum likelihood and other methods
* `MASS`: Functions for statistical methods and distributions
* `evd`: For modeling extreme value distributions
* `goftest`: For performing goodness-of-fit tests
* `shiny`: For building interactive web applications in R
* `DT`: For rendering interactive data tables in Shiny apps

## Group Members

* <b>Meghraj Goswami</b> - 2022A2B41869H
* <b>Majji Sai Snigdha</b> - 2022A2PS1427H
* <b>Ananthula Radha Mythri Reddy</b> - 2022A2PS1432H