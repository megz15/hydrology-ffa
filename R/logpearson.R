library(readxl)
library(lubridate)
library(dplyr)
library(extRemes)

# Function to get K (frequency factor) using Wilson-Hilferty approximation (https://tonyladson.wordpress.com/2015/03/03/695/)
get_K <- function(T, skewness) {
  z <- qnorm(1 - 1/T) # Standard normal deviate
  K <- z + (z^2 - 1) * skewness / 6 + (z^3 - 6*z) * (skewness^2) / 24 - (z^4 - 2.5*z^2) * (skewness^3) / 120
  return(K)
}

# Predicted Q Values for AMS using LP3
ams_data$Q_pred_LP3 <- NA
for (i in 1:nrow(ams_data)) {
  T <- ams_data$ReturnPeriod[i]
  K <- get_K(T, skewness_log_ams)
  log_Q_T <- mean_log_ams + K * sd_log_ams
  ams_data$Q_pred_LP3[i] <- 10^log_Q_T
}

# Predicted Q Values for desired return periods
predicted_discharges <-
  sapply(results$T, function(T) {
    K <- get_K(T, skewness_log_ams)
    log_Q_T <- mean_log_ams + K * sd_log_ams
    return(10 ^ log_Q_T)
  })

results$LP3_Q_Pred <- predicted_discharges