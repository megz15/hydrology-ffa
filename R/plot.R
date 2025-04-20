library(ggplot2)

plot_data <- data.frame(
  ReturnPeriod = rep(results$T, 7),
  Discharge = c(
    results$Gumbel_Q_Pred_MLE,
    results$Gumbel_Q_Pred_Manual,
    results$LP3_Q_Pred,
    results$LogNormal_Q_Pred_MLE,
    results$Gumbel_Q_Pred_Manual,
    results$Gamma_Q_Pred_MLE,
    results$Weibull_Q_Pred_MLE
  ),
  Distribution = factor(rep(c("Gumbel (MLE)", "Gumbel (MME)", "Log-Pearson III", "Log-Normal (MME)", "Log-Normal (MLE)", "Gamma", "Weibull"), 
                            each = length(results$T)))
)

ggplot(plot_data, aes(x = ReturnPeriod, y = Discharge, color = Distribution, group = Distribution)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_x_log10(breaks = c(2, 5, 10, 25, 50, 100, 200)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Comparison of Predicted Flood Discharges for Different Distributions",
    x = "Return Period (years)",
    y = "Predicted Discharge (cusecs)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.minor = element_line(color = "gray90"),
    panel.grid.major = element_line(color = "gray85"),
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave("flood_frequency_comparison.png", width = 10, height = 7, dpi = 300, bg="white")
