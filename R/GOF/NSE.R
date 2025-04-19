nse <- function(observed, predicted) {
  observed_mean <- mean(observed, na.rm = TRUE)
  numerator <- sum((observed - predicted)^2, na.rm = TRUE)
  denominator <- sum((observed - observed_mean)^2, na.rm = TRUE)
  1 - (numerator / denominator)
}

nse_gumbel_manual = nse(ams_data$ams, ams_data$Gumbel_Q_Pred_Manual)
nse_gumbel_mle = nse(ams_data$ams, ams_data$Gumbel_Q_Pred_MLE)

nse_lognormal_manual = nse(ams_data$ams, ams_data$LogNormal_Q_Pred_Manual)
nse_lognormal_mle = nse(ams_data$ams, ams_data$LogNormal_Q_Pred_MLE)

nse_lp3 = nse(ams_data$ams, ams_data$LP3_Q_Pred)
nse_gamma_mle = nse(ams_data$ams, ams_data$Gamma_Q_Pred_MLE)
nse_weibull_mle = nse(ams_data$ams, ams_data$Weibull_Q_Pred_MLE)

perf_metrics$NSE = c(
  nse_gumbel_manual, nse_gumbel_mle,
  nse_lognormal_manual, nse_lognormal_mle,
  nse_lp3, nse_gamma_mle, nse_weibull_mle
)