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