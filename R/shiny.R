require(shiny)
require(readxl)
require(lubridate)
require(dplyr)
require(extRemes)
require(fitdistrplus)
require(MASS)
require(evd)
require(goftest)
require(DT)
require(ggplot2)
# library(rsconnect)

ui <- fluidPage(
  titlePanel("Flood Frequency Analysis Using Statistical Methods"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File (.xlsx)",
                accept = c(".xlsx")),
      helpText("File headers should be 'l No', 'date', 'Inflow (Cusecs)'"),
      
      hr(),
      
      actionButton("analyze", "Run Analysis", 
                   class = "btn-primary btn-block",
                   style = "margin-top: 10px;"),
      
      hr(),
      
      conditionalPanel(
        condition = "output.analysisComplete",
        downloadButton("downloadResults", "Download Results")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Summary",
                 h4("Annual Maximum Series"),
                 DTOutput("amsTable")),
        tabPanel("Distribution Results",
                 h4("Predicted Discharge for Standard Return Periods"),
                 DTOutput("resultsTable"),
                 hr(),
                 h4("Predicted Discharge for Return Periods Calculated from Data"),
                 DTOutput("weibullResultsTable")),
        tabPanel("Goodness-of-Fit Tests",
                 h4("Statistical Tests"),
                 DTOutput("gofTable")),
        tabPanel("Performance Metrics",
                 h4("Efficiency Metrics"),
                 DTOutput("perfTable")),

        tabPanel("Visualizations",
                 h4("Comparison of Predicted Flood Discharges"),
                 plotOutput("distributionComparisonPlot", height = "600px"),
                 hr(),
                 h4("Plot Settings"),
                 checkboxGroupInput("distributionsToShow", "Select Distributions to Display:",
                                    choices = c("Gumbel (MLE)" = "Gumbel_Q_Pred_MLE",
                                                "Gumbel (MME)" = "Gumbel_Q_Pred_Manual",
                                                "Log-Pearson III" = "LP3_Q_Pred", 
                                                "Log-Normal (MLE)" = "LogNormal_Q_Pred_MLE",
                                                "Log-Normal (MME)" = "LogNormal_Q_Pred_Manual",
                                                "Gamma (MLE)" = "Gamma_Q_Pred_MLE",
                                                "Weibull (MLE)" = "Weibull_Q_Pred_MLE"),
                                    selected = c("Gumbel_Q_Pred_MLE", "LP3_Q_Pred", 
                                                 "LogNormal_Q_Pred_MLE", "Gamma_Q_Pred_MLE", 
                                                 "Weibull_Q_Pred_MLE")
                 ),
                 downloadButton("downloadPlot", "Download Plot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # For storing analysis results
  rv <- reactiveValues(
    data = NULL,
    ams_data = NULL,
    results = NULL,
    gof_results = NULL,
    perf_metrics = NULL,
    analysisComplete = FALSE
  )
  
  # Analyze button clicked
  observeEvent(input$analyze, {
    req(input$file)
    
    # Following code is from monolith file
    
    # Load Data
    data <- read_excel(input$file$datapath, sheet = "Sheet1")
    data$date <- as.Date(data$date, format="%d/%m/%Y")
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
    skewness_log_ams <- (sum((ams_data$log_ams - mean_log_ams)^3) * length(ams_data$log_ams)) / 
      ((length(ams_data$log_ams) - 1) * (length(ams_data$log_ams) - 2) * sd_log_ams^3)
    
    # Results table
    results <- data.frame(T = c(2, 5, 10, 25, 50, 100, 200))
    
    gof_results <- data.frame(
      Distribution = c("Gumbel", "Log-Normal", "Gamma", "Weibull")
    )
    
    perf_metrics <- data.frame(
      Distribution = c("Gumbel (MME)", "Gumbel (MLE)", "Log-Normal (MME)", 
                       "Log-Normal (MLE)", "LP3", "Gamma (MLE)", "Weibull (MLE)")
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
      return(mean_Q + K * sd_Q)
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
    # Function to get K (frequency factor) using Wilson-Hilferty approximation
    get_K <- function(T, skewness) {
      z <- qnorm(1 - 1/T) # Standard normal deviate
      K <- z + (z^2 - 1) * skewness / 6 + (z^3 - 6*z) * (skewness^2) / 24 - 
        (z^4 - 2.5*z^2) * (skewness^3) / 120
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
    predicted_discharges <- sapply(results$T, function(T) {
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
    ## KS... # Kolmogorov-Smirnov
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
      ks_gamma_mle$statistic, 
      ks_weibull_mle$statistic
    )
    
    gof_results$KS_PValue = c(
      ks_gumbel_mle$p.value,
      ks_lognormal_mle$p.value,
      ks_gamma_mle$p.value, 
      ks_weibull_mle$p.value
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
      ad_gamma_mle$statistic, 
      ad_weibull_mle$statistic
    )
    
    gof_results$AD_PValue = c(
      ad_gumbel_mle$p.value,
      ad_lognormal_mle$p.value,
      ad_gamma_mle$p.value, 
      ad_weibull_mle$p.value
    )
    
    ### Efficiency
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
    
    # Store results in reactive values
    rv$data <- data
    rv$ams_data <- ams_data
    rv$results <- results
    rv$gof_results <- gof_results
    rv$perf_metrics <- perf_metrics
    rv$analysisComplete <- TRUE
  })
  
  # Output for AMS table
  output$amsTable <- renderDT({
    req(rv$ams_data)
    datatable(rv$ams_data[, c("year", "ams", "Rank", "WeibullProb", "ReturnPeriod")],
              rownames = FALSE)
  })
  
  # Output for results table
  output$resultsTable <- renderDT({
    req(rv$results)
    datatable(rv$results,
              options = list(scrollX = TRUE),
              rownames = FALSE,
              caption = "Predicted Discharge for Standard Return Periods")
  })
  
  output$weibullResultsTable <- renderDT({
    req(rv$ams_data)
    datatable(rv$ams_data[, c("ReturnPeriod", "Gumbel_Q_Pred_Manual", "Gumbel_Q_Pred_MLE", "LogNormal_Q_Pred_Manual", "LogNormal_Q_Pred_MLE", "LP3_Q_Pred", "Weibull_Q_Pred_MLE", "Gamma_Q_Pred_MLE")],
              options = list(scrollX = TRUE),
              rownames = FALSE,
              caption = "PredictedPredicted Discharge for Return Periods Calculated from Data")
  })
  
  # Output for GOF table
  output$gofTable <- renderDT({
    req(rv$gof_results)
    gof_formatted <- rv$gof_results
    gof_formatted$KS_Statistic <- round(gof_formatted$KS_Statistic, 4)
    gof_formatted$KS_PValue <- round(gof_formatted$KS_PValue, 4)
    gof_formatted$AD_Statistic <- round(gof_formatted$AD_Statistic, 4)
    gof_formatted$AD_PValue <- round(gof_formatted$AD_PValue, 4)
    
    datatable(gof_formatted,
              rownames = FALSE,
              caption = "Goodness-of-Fit Test Results")
  })
  
  # Output for performance metrics table
  output$perfTable <- renderDT({
    req(rv$perf_metrics)
    perf_formatted <- rv$perf_metrics
    perf_formatted$RMSE <- round(perf_formatted$RMSE, 2)
    perf_formatted$NSE <- round(perf_formatted$NSE, 4)
    perf_formatted$KGE <- round(perf_formatted$KGE, 4)
    
    datatable(perf_formatted,
              rownames = FALSE,
              caption = "Performance Metrics")
  })
  
  # Flag to indicate analysis is complete
  output$analysisComplete <- reactive({
    return(rv$analysisComplete)
  })
  outputOptions(output, "analysisComplete", suspendWhenHidden = FALSE)
  
  # Download handler for results
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("flood_frequency_analysis_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$results, file, row.names = FALSE)
    }
  )
  
  # Distribution comparison plot
  output$distributionComparisonPlot <- renderPlot({
    req(rv$results, rv$analysisComplete)
    req(input$distributionsToShow)
    selected_dists <- input$distributionsToShow
    
    dist_labels <- c(
      "Gumbel_Q_Pred_MLE" = "Gumbel (MLE)",
      "Gumbel_Q_Pred_Manual" = "Gumbel (MME)",
      "LP3_Q_Pred" = "Log-Pearson III",
      "LogNormal_Q_Pred_MLE" = "Log-Normal (MLE)",
      "LogNormal_Q_Pred_Manual" = "Log-Normal (MME)",
      "Gamma_Q_Pred_MLE" = "Gamma",
      "Weibull_Q_Pred_MLE" = "Weibull"
    )
    
    # plotting with only selected distributions
    plot_data <- data.frame(
      ReturnPeriod = rep(rv$results$T, length(selected_dists)),
      Discharge = numeric(length(rv$results$T) * length(selected_dists)),
      Distribution = character(length(rv$results$T) * length(selected_dists))
    )
    
    row_index <- 1
    for (dist in selected_dists) {
      for (i in 1:length(rv$results$T)) {
        plot_data$ReturnPeriod[row_index] <- rv$results$T[i]
        plot_data$Discharge[row_index] <- rv$results[[dist]][i]
        plot_data$Distribution[row_index] <- dist_labels[dist]
        row_index <- row_index + 1
      }
    }
    
    # conversion to factor to maintain order
    plot_data$Distribution <- factor(plot_data$Distribution, levels = dist_labels[selected_dists])
    
    ggplot(plot_data, aes(x = ReturnPeriod, y = Discharge, 
                          color = Distribution, group = Distribution)) +
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
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("flood_frequency_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(rv$results, rv$analysisComplete)
      req(input$distributionsToShow)
      
      selected_dists <- input$distributionsToShow
      
      dist_labels <- c(
        "Gumbel_Q_Pred_MLE" = "Gumbel (MLE)",
        "Gumbel_Q_Pred_Manual" = "Gumbel (MME)",
        "LP3_Q_Pred" = "Log-Pearson III",
        "LogNormal_Q_Pred_MLE" = "Log-Normal (MLE)",
        "LogNormal_Q_Pred_Manual" = "Log-Normal (MME)",
        "Gamma_Q_Pred_MLE" = "Gamma",
        "Weibull_Q_Pred_MLE" = "Weibull"
      )
      
      plot_data <- data.frame(
        ReturnPeriod = rep(rv$results$T, length(selected_dists)),
        Discharge = numeric(length(rv$results$T) * length(selected_dists)),
        Distribution = character(length(rv$results$T) * length(selected_dists))
      )
      
      row_index <- 1
      for (dist in selected_dists) {
        for (i in 1:length(rv$results$T)) {
          plot_data$ReturnPeriod[row_index] <- rv$results$T[i]
          plot_data$Discharge[row_index] <- rv$results[[dist]][i]
          plot_data$Distribution[row_index] <- dist_labels[dist]
          row_index <- row_index + 1
        }
      }
      
      plot_data$Distribution <- factor(plot_data$Distribution, levels = dist_labels[selected_dists])
      
      p <- ggplot(plot_data, aes(x = ReturnPeriod, y = Discharge, 
                                 color = Distribution, group = Distribution)) +
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
      
      ggsave(file, plot = p, width = 10, height = 7, dpi = 300, bg = "white")
    }
  )
}

shinyApp(ui = ui, server = server)
