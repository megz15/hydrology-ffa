library(readxl)
library(lubridate)
library(dplyr)
library(extRemes)
library(fitdistrplus)
library(MASS)
library(evd)
library(goftest)

# Load Data
data <- read_excel("../data/nizam_sagar_inflow.xlsx", sheet = "Sheet1")
data$date <- as.Date(data$date)
data$`Inflow (Cusecs)` <- as.numeric(data$`Inflow (Cusecs)`)
data$year <- format(data$date, "%Y")

# AMS Calc
ams_data <- data %>%
  group_by(year) %>%
  summarise(ams = max(`Inflow (Cusecs)`, na.rm = TRUE)) %>%
  filter(ams > 0)
ams_data <- ams_data[order(ams_data$ams, decreasing = TRUE),]

# Weibull Plotting Positions
n <- nrow(ams_data)
ams_data$Rank <- 1:n
ams_data$WeibullProb <- ams_data$Rank/(n+1)
ams_data$ReturnPeriod <- 1/ams_data$WeibullProb

# Log transform data
ams_data$log_ams <- log10(ams_data$ams)

# Calculating mean, stdev and skewness of the log-transformed data
mean_log_ams <- mean(ams_data$log_ams)
sd_log_ams <- sd(ams_data$log_ams)
skewness_log_ams <- (sum((ams_data$log_ams - mean_log_ams)^3) * length(ams_data$log_ams)) / ((length(ams_data$log_ams) - 1) * (length(ams_data$log_ams) - 2) * sd_log_ams^3)

# Results table
results <- data.frame(T = c(2, 5, 10, 25, 50, 100, 200))
gof_results <- data.frame(
  Distribution = c("Gumbel", "Log-Normal", "Gamma", "Weibull")
)
perf_metrics <- data.frame(
  Distribution = c("Gumbel (MME)", "Gumbel (MLE)", "Log-Normal (MME)", "Log-Normal (MLE)", "LP3", "Gamma (MLE)", "Weibull (MLE)")
)

## Gumbel

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

## LNorm

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

## LP3

# Function to get K (frequency factor) using Wilson-Hilferty approximation (https://tonyladson.wordpress.com/2015/03/03/695/)
get_K <- function(T, skewness) {
  z <- qnorm(1 - 1/T) # Standard normal deviate
  K <- z + (z^2 - 1) * skewness / 6 + (z^3 - 6*z) * (skewness^2) / 24 - (z^4 - 2.5*z^2) * (skewness^3) / 120
  return(K)
}

# Predicted Q Values for AMS using LP3
ams_data$LP3_Q_Pred <- NA
for (i in 1:nrow(ams_data)) {
  T <- ams_data$ReturnPeriod[i]
  K <- get_K(T, skewness_log_ams)
  log_Q_T <- mean_log_ams + K * sd_log_ams
  ams_data$LP3_Q_Pred[i] <- 10^log_Q_T
}

# Predicted Q Values for desired return periods
predicted_discharges <-
  sapply(results$T, function(T) {
    K <- get_K(T, skewness_log_ams)
    log_Q_T <- mean_log_ams + K * sd_log_ams
    return(10 ^ log_Q_T)
  })

results$LP3_Q_Pred <- predicted_discharges

## Weibull

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

## Gamma

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

### GOF

## KS

# Kolmogorov-Smirnov

ks_gumbel_mle = ks.test(ams_data$ams, "pgev",
                         loc = gumbel_fit$results$par["location"],
                         scale = gumbel_fit$results$par["scale"],
                         shape = 0)

ks_lognormal_mle = ks.test(ams_data$ams, "plnorm",
                            meanlog = lognormal_fit$estimate["meanlog"],
                            sdlog = lognormal_fit$estimate["sdlog"])

ks_gamma_mle = ks.test(ams_data$ams, "pgamma",
                        shape = gamma_fit$estimate["shape"],
                        rate = gamma_fit$estimate["rate"])

ks_weibull_mle = ks.test(ams_data$ams, "pweibull",
                          shape = weibull_fit$estimate["shape"],
                          scale = weibull_fit$estimate["scale"])

gof_results$KS_Statistic = c(
  ks_gumbel_mle$statistic,
  ks_lognormal_mle$statistic,
  ks_gamma_mle$statistic, ks_weibull_mle$statistic
)

gof_results$KS_PValue = c(
  ks_gumbel_mle$p.value,
  ks_lognormal_mle$p.value,
  ks_gamma_mle$p.value, ks_weibull_mle$p.value
)

## AD

# Anderson-Darling

ad_gumbel_mle = ad.test(ams_data$ams, "pgev",
                        loc = gumbel_fit$results$par["location"],
                        scale = gumbel_fit$results$par["scale"],
                        shape = 0)

ad_lognormal_mle = ad.test(ams_data$ams, "plnorm",
                           meanlog = lognormal_fit$estimate["meanlog"],
                           sdlog = lognormal_fit$estimate["sdlog"])

ad_gamma_mle = ad.test(ams_data$ams, "pgamma",
                       shape = gamma_fit$estimate["shape"],
                       rate = gamma_fit$estimate["rate"])

ad_weibull_mle = ad.test(ams_data$ams, "pweibull",
                         shape = weibull_fit$estimate["shape"],
                         scale = weibull_fit$estimate["scale"])

gof_results$AD_Statistic = c(
  ad_gumbel_mle$statistic,
  ad_lognormal_mle$statistic,
  ad_gamma_mle$statistic, ad_weibull_mle$statistic
)

gof_results$AD_PValue = c(
  ad_gumbel_mle$p.value,
  ad_lognormal_mle$p.value,
  ad_gamma_mle$p.value, ad_weibull_mle$p.value
)

### Effiiency

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

## NSE

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

## KGE

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
