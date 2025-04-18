library(readxl)
library(lubridate)
library(dplyr)
library(extRemes)

# Load Data
data <- read_excel("../data/nizam_sagar_inflow.xlsx", sheet = "Sheet1")
data$date <- as.Date(data$date, format="%d/%m/%Y")
data$`Inflow (Cusecs)` <- as.numeric(data$`Inflow (Cusecs)`)
data$year <- format(data$date, "%Y")

# AMS Calc
ams_data <- data %>%
  group_by(year) %>%
  summarise(ams = max(`Inflow (Cusecs)`, na.rm = TRUE))
ams_data <- ams_data[order(ams_data$ams, decreasing = TRUE),]

# Weibull Plotting Positions
n <- nrow(ams_data)
ams_data$Rank <- 1:n
ams_data$WeibullProb <- ams_data$Rank/(n+1)
ams_data$ReturnPeriod <- 1/ams_data$WeibullProb

# Log-Normal
ams_data$log_ams <- log10(ams_data$ams)
mean_log_ams <- mean(ams_data$log_ams)
sd_log_ams <- sd(ams_data$log_ams)

# Manual method
get_lognormal_quantile <- function(T, mean_log, sd_log) {
  z <- qnorm(1 - 1/T) # standard normal deviate
  log_Q_T <- mean_log + z * sd_log
  return(10^log_Q_T)
}

# Predicted Q Values for AMS using Log-Normal
ams_data$Q_pred_LogNormal <- NA
for (i in 1:nrow(ams_data)) {
  T <- ams_data$ReturnPeriod[i]
  ams_data$Q_pred_LogNormal[i] <- get_lognormal_quantile(T, mean_log_ams, sd_log_ams)
}