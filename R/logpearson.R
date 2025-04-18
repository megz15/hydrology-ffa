library(readxl)
library(lubridate)
library(dplyr)
library(extRemes)

data <- read_excel("data/nizam_sagar_inflow.xlsx", sheet = "Sheet1")
data$date <- as.Date(data$date, format="%d/%m/%Y")
data$`Inflow (Cusecs)` <- as.numeric(data$`Inflow (Cusecs)`)
data$year <- format(data$date, "%Y")

# AMS Calc
ams_data <- data %>%
  group_by(year) %>%
  summarise(ams = max(`Inflow (Cusecs)`, na.rm = TRUE)) %>%
  filter(ams > 0)
ams_data <- ams_data[order(ams_data$ams, decreasing = TRUE),]

# Weibull plotting positions
n <- nrow(ams_data)
ams_data$Rank <- 1:n
ams_data$WeibullProb <- ams_data$Rank/(n+1)
ams_data$ReturnPeriod <- 1/ams_data$WeibullProb

# Log-Pearson Type III Distribution Fitting
# Log-transform the AMS data
ams_data$log_ams <- log10(ams_data$ams)

# Calculate mean, standard deviation, and skewness of the log-transformed data
mean_log_ams <- mean(ams_data$log_ams)
sd_log_ams <- sd(ams_data$log_ams)
skewness_log_ams <- (sum((ams_data$log_ams - mean_log_ams)^3) * length(ams_data$log_ams)) / ((length(ams_data$log_ams) - 1) * (length(ams_data$log_ams) - 2) * sd_log_ams^3)

# Function to get K (frequency factor) using Wilson-Hilferty approximation (https://tonyladson.wordpress.com/2015/03/03/695/)
get_K <- function(T, skewness) {
  z <- qnorm(1 - 1/T) # Standard normal deviate
  K <- z + (z^2 - 1) * skewness / 6 + (z^3 - 6*z) * (skewness^2) / 24 - (z^4 - 2.5*z^2) * (skewness^3) / 120
  return(K)
}

# Calculate Log-Pearson Type III predicted discharges
ams_data$Q_pred_LP3 <- NA
for (i in 1:nrow(ams_data)) {
  T <- ams_data$ReturnPeriod[i]
  K <- get_K(T, skewness_log_ams)
  log_Q_T <- mean_log_ams + K * sd_log_ams
  ams_data$Q_pred_LP3[i] <- 10^log_Q_T # Convert back from log
}

return_periods <- c(2, 5, 10, 25, 50, 100, 200)
predicted_discharges <-
  sapply(return_periods, function(T) {
    K <- get_K(T, skewness_log_ams)
    log_Q_T <- mean_log_ams + K * sd_log_ams
    return(10 ^ log_Q_T)
  })

results_lp3 <-
  data.frame(Return_Period = return_periods,
             Predicted_Discharge = predicted_discharges)