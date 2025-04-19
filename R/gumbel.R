# Fit Gumbel (shape = 0 in GEV)
gumbel_fit <- fevd(ams_data$ams, type = "GEV", shape = 0, method = "MLE")

# Predicted Q Values for AMS using Gumbel
ams_data$Gumbel_Q_Pred_MLE <- return.level(gumbel_fit, ams_data$ReturnPeriod)

# Predicted Q Values for desired return periods
return_levels <- return.level(gumbel_fit, results$T)
results$Gumbel_Q_Pred_MLE <- return_levels

# Manual book method
mean_Q <- mean(ams_data$ams)
sd_Q <- sd(ams_data$ams)
Y_n <- 0.4952
S_n <- 0.9496

ams_data$Gumbel_Q_Pred_Manual <- NA
for (i in 1:nrow(ams_data)) {
  T <- ams_data$ReturnPeriod[i]
  
  # Calculate Y_T and K for the current Return Period
  Y_T <- -log(-log(1 - 1/T))
  K <- (Y_T - Y_n) / S_n
  ams_data$Gumbel_Q_Pred_Manual[i] <- mean_Q + K * sd_Q
}

# Predicted Q Values for desired return periods (MLE)
predicted_discharges <- sapply(results$T, function(T) {
  Y_T <- -log(-log((T-1)/T))
  K <- (Y_T - Y_n) / S_n
  return (mean_Q + K * sd_Q)
})
results$Gumbel_Q_Pred_Manual <- predicted_discharges