library(readxl)
library(lubridate)
library(dplyr)
library(extRemes)
library(fitdistrplus)
library(MASS)
library(evd)

# Load Data
data <- read_excel("../data/nizam_sagar_inflow.xlsx", sheet = "Sheet1")
data$date <- as.Date(data$date, format="%d/%m/%Y")
data$`Inflow (Cusecs)` <- as.numeric(data$`Inflow (Cusecs)`)
data$year <- format(data$date, "%Y")

# AMS Calc
ams_data <- data %>%
  group_by(year) %>%
  summarise(ams = max(`Inflow (Cusecs)`, na.rm = TRUE)) %>%
  filter(ams > 0)
ams_data <- ams_data[order(ams_data$ams, decreasing = TRUE),]

# Weibull Plotting Positions
n <- nrow(ams_data)
ams_data$Rank <- 1:n
ams_data$WeibullProb <- ams_data$Rank/(n+1)
ams_data$ReturnPeriod <- 1/ams_data$WeibullProb

# Log transform data
ams_data$log_ams <- log10(ams_data$ams)

# Calculating mean, stdev and skewness of the log-transformed data
mean_log_ams <- mean(ams_data$log_ams)
sd_log_ams <- sd(ams_data$log_ams)
skewness_log_ams <- (sum((ams_data$log_ams - mean_log_ams)^3) * length(ams_data$log_ams)) / ((length(ams_data$log_ams) - 1) * (length(ams_data$log_ams) - 2) * sd_log_ams^3)

# Results table
results <- data.frame(T = c(2, 5, 10, 25, 50, 100, 200))
gof_results <- data.frame(
  Distribution = c("Gumbel (MME)", "Gumbel (MLE)", "Log-Normal (MME)", "Log-Normal (MLE)", "LP3", "Gamma (MLE)", "Weibull (MLE)")
)