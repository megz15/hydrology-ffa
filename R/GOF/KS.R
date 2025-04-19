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
  NA, ks_gumbel_mle$statistic,
  NA, ks_lognormal_mle$statistic,
  NA, ks_gamma_mle$statistic, ks_weibull_mle$statistic
)

gof_results$KS_PValue = c(
  NA, ks_gumbel_mle$p.value,
  NA, ks_lognormal_mle$p.value,
  NA, ks_gamma_mle$p.value, ks_weibull_mle$p.value
)