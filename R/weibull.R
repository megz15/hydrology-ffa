# Fit weibull distribution
weibull_fit <- fitdist(ams_data$ams, "weibull", method="mle")

ams_data$Weibull_Q_Pred_MLE <- NA
for (i in 1:nrow(ams_data)) {
  T <- ams_data$ReturnPeriod[i]
  ams_data$Weibull_Q_Pred_MLE[i] <- qweibull(ams_data$WeibullProb[i], 
                                         shape = weibull_fit$estimate["shape"], 
                                         scale = weibull_fit$estimate["scale"],
                                         lower.tail = FALSE)
}

# Predicted Q Values for desired return periods (MLE)
predicted_discharges_mle <- sapply(results$T, function(T) {
  prob <- 1/T
  qweibull(prob, 
         shape = weibull_fit$estimate["shape"], 
         scale = weibull_fit$estimate["scale"],
         lower.tail = FALSE)
})
results$Weibull_Q_Pred_MLE <- predicted_discharges_mle