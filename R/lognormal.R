# Manual method
get_lognormal_quantile <- function(T) {
  z <- qnorm(1 - 1/T) # standard normal deviate
  log_Q_T <- mean_log_ams + z * sd_log_ams
  return(10^log_Q_T)
}

# Fit log-normal distribution
lognormal_fit <- fitdist(ams_data$ams, "lnorm", method="mle")

ams_data$LogNormal_Q_Pred_MLE <- NA
for (i in 1:nrow(ams_data)) {
  T <- ams_data$ReturnPeriod[i]
  ams_data$LogNormal_Q_Pred_MLE[i] <- qlnorm(1 - ams_data$WeibullProb[i], 
                                             meanlog = lognormal_fit$estimate["meanlog"], 
                                             sdlog = lognormal_fit$estimate["sdlog"])
}

# Predicted Q Values for AMS using Log-Normal (manual)
ams_data$LogNormal_Q_Pred_Manual <- NA
for (i in 1:nrow(ams_data)) {
  T <- ams_data$ReturnPeriod[i]
  ams_data$LogNormal_Q_Pred_Manual[i] <- get_lognormal_quantile(T)
}

# Predicted Q Values for desired return periods
predicted_discharges <- sapply(results$T, function(T) {
  get_lognormal_quantile(T)
})

results$LogNormal_Q_Pred_Manual <- predicted_discharges

# Predicted Q Values for desired return periods (MLE)
predicted_discharges_mle <- sapply(results$T, function(T) {
  prob <- 1 - 1/T
  qlnorm(prob, 
         meanlog = lognormal_fit$estimate["meanlog"], 
         sdlog = lognormal_fit$estimate["sdlog"])
})
results$LogNormal_Q_Pred_MLE <- predicted_discharges_mle