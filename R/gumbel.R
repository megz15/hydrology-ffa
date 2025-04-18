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

# Fit Gumbel (shape = 0 in GEV)
fit <- fevd(ams_data$ams, type = "GEV", shape = 0)

# Predicted Q Values for AMS using Gumbel
ams_data$Q_pred_mle <- return.level(fit, ams_data$ReturnPeriod)

# Predicted Q Values for desired return periods
return_periods <- c(2, 5, 10, 25, 50, 100, 200)
return_levels <- return.level(fit, return_periods)

results <-
  data.frame(Return_Period = return_periods,
             Gumbel_Q_Pred = return_levels)

# Manual book method
mean_Q <- mean(ams_data$ams)
sd_Q <- sd(ams_data$ams)
Y_n <- 0.4952
S_n <- 0.9496

ams_data$Q_pred_manual <- NA
for (i in 1:nrow(ams_data)) {
  T <- ams_data$ReturnPeriod[i]
  
  # Calculate Y_T and K for the current Return Period
  Y_T <- -log(-log(1 - 1/T))
  K <- (Y_T - Y_n) / S_n
  ams_data$Q_pred_manual[i] <- mean_Q + K * sd_Q
}