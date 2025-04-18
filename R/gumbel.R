library(readxl)
library(lubridate)
library(dplyr)
library(extRemes)

# Fit Gumbel (shape = 0 in GEV)
fit <- fevd(ams_data$ams, type = "GEV", shape = 0)

# Predicted Q Values for AMS using Gumbel
ams_data$Q_pred_Gumbel_mle <- return.level(fit, ams_data$ReturnPeriod)

# Predicted Q Values for desired return periods
return_levels <- return.level(fit, results$T)
results$Gumbel_Q_Pred <- return_levels

# Manual book method
mean_Q <- mean(ams_data$ams)
sd_Q <- sd(ams_data$ams)
Y_n <- 0.4952
S_n <- 0.9496

ams_data$Q_pred_Gumbel_manual <- NA
for (i in 1:nrow(ams_data)) {
  T <- ams_data$ReturnPeriod[i]
  
  # Calculate Y_T and K for the current Return Period
  Y_T <- -log(-log(1 - 1/T))
  K <- (Y_T - Y_n) / S_n
  ams_data$Q_pred_Gumbel_manual[i] <- mean_Q + K * sd_Q
}