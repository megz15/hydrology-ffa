# Fit gamma distribution
gamma_fit <- fitdist(ams_data$ams, "gamma", method="mle")

ams_data$Gamma_Q_Pred_MLE <- NA
for (i in 1:nrow(ams_data)) {
  T <- ams_data$ReturnPeriod[i]
  ams_data$Gamma_Q_Pred_MLE[i] <- qgamma(1 - ams_data$WeibullProb[i], 
                                             shape = gamma_fit$estimate["shape"], 
                                             rate = gamma_fit$estimate["rate"])
}

# Predicted Q Values for desired return periods (MLE)
predicted_discharges_mle <- sapply(results$T, function(T) {
  prob <- 1 - 1/T
  qgamma(prob, 
         shape = gamma_fit$estimate["shape"], 
         rate = gamma_fit$estimate["rate"])
})
results$Gamma_Q_Pred_MLE <- predicted_discharges_mle