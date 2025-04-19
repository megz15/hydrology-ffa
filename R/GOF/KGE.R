kge <- function(observed, predicted) {
  observed_mean <- mean(observed, na.rm = TRUE)
  predicted_mean <- mean(predicted, na.rm = TRUE)
  
  r <- cor(observed, predicted, use = "complete.obs") # Correlation coefficient
  alpha <- sd(predicted, na.rm = TRUE) / sd(observed, na.rm = TRUE) # Variability ratio (alpha)
  beta <- predicted_mean / observed_mean # Bias ratio (beta)

  1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
}

kge_gumbel_manual = kge(ams_data$ams, ams_data$Gumbel_Q_Pred_Manual)
kge_gumbel_mle = kge(ams_data$ams, ams_data$Gumbel_Q_Pred_MLE)

kge_lognormal_manual = kge(ams_data$ams, ams_data$LogNormal_Q_Pred_Manual)
kge_lognormal_mle = kge(ams_data$ams, ams_data$LogNormal_Q_Pred_MLE)

kge_lp3 = kge(ams_data$ams, ams_data$LP3_Q_Pred)
kge_gamma_mle = kge(ams_data$ams, ams_data$Gamma_Q_Pred_MLE)
kge_weibull_mle = kge(ams_data$ams, ams_data$Weibull_Q_Pred_MLE)

perf_metrics$KGE = c(
  kge_gumbel_manual, kge_gumbel_mle,
  kge_lognormal_manual, kge_lognormal_mle,
  kge_lp3, kge_gamma_mle, kge_weibull_mle
)