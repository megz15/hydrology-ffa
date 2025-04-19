rmse <- function(observed, predicted) {
  sqrt(mean((observed - predicted)^2, na.rm=TRUE))
}

rmse_gumbel_manual = rmse(ams_data$ams, ams_data$Gumbel_Q_Pred_Manual)
rmse_gumbel_mle = rmse(ams_data$ams, ams_data$Gumbel_Q_Pred_MLE)

rmse_lognormal_manual = rmse(ams_data$ams, ams_data$LogNormal_Q_Pred_Manual)
rmse_lognormal_mle = rmse(ams_data$ams, ams_data$LogNormal_Q_Pred_MLE)

rmse_lp3 = rmse(ams_data$ams, ams_data$LP3_Q_Pred)
rmse_gamma_mle = rmse(ams_data$ams, ams_data$Gamma_Q_Pred_MLE)
rmse_weibull_mle = rmse(ams_data$ams, ams_data$Weibull_Q_Pred_MLE)

perf_metrics$RMSE = c(
  rmse_gumbel_manual, rmse_gumbel_mle,
  rmse_lognormal_manual, rmse_lognormal_mle,
  rmse_lp3, rmse_gamma_mle, rmse_weibull_mle
)