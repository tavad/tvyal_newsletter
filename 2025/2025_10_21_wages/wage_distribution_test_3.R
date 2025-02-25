library(tidyverse)
library(stats)
library(GB2)
library(flexsurv)
library(actuar)
library(scales)

# Helper function to get latest date data
get_latest_data <- function(df) {
  max_date <- max(df$date)
  df %>% 
    filter(date == max_date) %>%
    select(wages_cumulative, employees)
}

# Distribution fitting functions
fit_lognormal <- function(data) {
  opt_result <- optim(
    par = c(log(median(data$wages_cumulative)), 1),
    fn = function(params, x, weights) {
      -sum(weights * dlnorm(x, meanlog = params[1], sdlog = params[2], log = TRUE))
    },
    x = data$wages_cumulative,
    weights = data$employees,
    method = "L-BFGS-B",
    lower = c(-Inf, 0.01),
    upper = c(Inf, Inf)
  )
  return(list(meanlog = opt_result$par[1], sdlog = opt_result$par[2]))
}

fit_loglogistic <- function(data) {
  opt_result <- optim(
    par = c(median(data$wages_cumulative), 1),
    fn = function(params, x, weights) {
      alpha <- params[1]
      beta <- params[2]
      -sum(weights * (log(beta) - beta * log(alpha) + (beta - 1) * log(x) -
                        2 * log(1 + (x / alpha)^beta)))
    },
    x = data$wages_cumulative,
    weights = data$employees,
    method = "L-BFGS-B",
    lower = c(0.01, 0.01)
  )
  return(list(scale = opt_result$par[1], shape = opt_result$par[2]))
}

fit_gamma <- function(data) {
  mean_val <- weighted.mean(data$wages_cumulative, data$employees)
  var_val <- weighted.mean((data$wages_cumulative - mean_val)^2, data$employees)
  
  init_shape <- (mean_val^2) / var_val
  init_scale <- var_val / mean_val
  
  opt_result <- optim(
    par = c(init_shape, init_scale),
    fn = function(params, x, weights) {
      -sum(weights * dgamma(x, shape = params[1], scale = params[2], log = TRUE))
    },
    x = data$wages_cumulative,
    weights = data$employees,
    method = "L-BFGS-B",
    lower = c(0.01, 0.01)
  )
  return(list(shape = opt_result$par[1], scale = opt_result$par[2]))
}

fit_weibull <- function(data) {
  opt_result <- optim(
    par = c(2, median(data$wages_cumulative)),
    fn = function(params, x, weights) {
      -sum(weights * dweibull(x, shape = params[1], scale = params[2], log = TRUE))
    },
    x = data$wages_cumulative,
    weights = data$employees,
    method = "L-BFGS-B",
    lower = c(0.01, 0.01)
  )
  return(list(shape = opt_result$par[1], scale = opt_result$par[2]))
}

fit_burr <- function(data) {
  opt_result <- optim(
    par = c(2, 2, median(data$wages_cumulative)),
    fn = function(params, x, weights) {
      -sum(weights * log(dburr(x, shape1 = params[1], shape2 = params[2], scale = params[3])))
    },
    x = data$wages_cumulative,
    weights = data$employees,
    method = "L-BFGS-B",
    lower = c(0.01, 0.01, 0.01)
  )
  return(list(shape1 = opt_result$par[1], shape2 = opt_result$par[2], scale = opt_result$par[3]))
}






library(tidyverse)
library(stats)
library(GB2)
library(flexsurv)
library(actuar)
library(scales)

# Helper function to get latest date data
get_latest_data <- function(df) {
  max_date <- max(df$date)
  df %>% 
    filter(date == max_date) %>%
    select(wages_cumulative, employees)
}



# Modified function to compare distributions with deciles data
compare_distributions_deciles <- function(df_params, deciles_df, type_filter = "Non-public") {
  # Get latest data for fitting
  latest_data <- get_latest_data(df_params)
  
  # Fit distributions
  lnorm_params <- fit_lognormal(latest_data)
  llogis_params <- fit_loglogistic(latest_data)
  gamma_params <- fit_gamma(latest_data)
  weibull_params <- fit_weibull(latest_data)
  burr_params <- fit_burr(latest_data)
  
  # Process deciles data - filter for specific type and calculate percentiles
  latest_deciles <- deciles_df %>%
    filter(
      date == max(date),
      type_eng == type_filter
    ) %>%
    mutate(
      decile_num = as.numeric(str_extract(deciles, "\\d+")),
      percentile = decile_num / 10
    ) %>%
    filter(!is.na(percentile)) %>%
    distinct(percentile, wages)  # Ensure unique percentile-wage pairs
  
  # Generate theoretical quantiles at the same percentiles
  theoretical_quantiles <- tibble(
    percentile = latest_deciles$percentile,
    lnorm = qlnorm(percentile, lnorm_params$meanlog, lnorm_params$sdlog),
    llogis = qllogis(percentile, scale = llogis_params$scale, shape = llogis_params$shape),
    gamma = qgamma(percentile, shape = gamma_params$shape, scale = gamma_params$scale),
    weibull = qweibull(percentile, shape = weibull_params$shape, scale = weibull_params$scale),
    burr = qburr(percentile, shape1 = burr_params$shape1, shape2 = burr_params$shape2, 
                 scale = burr_params$scale)
  )
  
  # Create plot data
  plot_data <- theoretical_quantiles %>%
    pivot_longer(
      -percentile,
      names_to = "distribution",
      values_to = "theoretical_wage"
    ) %>%
    left_join(
      latest_deciles %>% 
        select(percentile, empirical_wage = wages),
      by = "percentile"
    )
  
  # Calculate goodness of fit metrics
  fit_metrics <- plot_data %>%
    group_by(distribution) %>%
    summarise(
      RMSE = sqrt(mean((theoretical_wage - empirical_wage)^2)),
      MAE = mean(abs(theoretical_wage - empirical_wage)),
      MAPE = mean(abs((theoretical_wage - empirical_wage) / empirical_wage)) * 100,
      .groups = "drop"
    ) %>%
    arrange(RMSE)
  
  # Create QQ plot
  qq_plot <- ggplot(plot_data, aes(x = empirical_wage, y = theoretical_wage, color = distribution)) +
    geom_point(size = 3, alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    scale_color_manual(
      values = c(
        "lnorm" = "blue",
        "llogis" = "green",
        "gamma" = "orange",
        "weibull" = "purple",
        "burr" = "red"
      ),
      labels = c(
        "Log-normal",
        "Log-logistic",
        "Gamma",
        "Weibull",
        "Burr (Singh-Maddala)"
      )
    ) +
    scale_x_continuous(labels = number_format()) +
    scale_y_continuous(labels = number_format()) +
    labs(
      title = paste("Theoretical vs Empirical Quantiles -", type_filter),
      subtitle = paste("Date:", format(max(deciles_df$date), "%B %Y")),
      x = "Empirical Wages (AMD)",
      y = "Theoretical Wages (AMD)",
      color = "Distribution"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  # Create relative error plot
  error_plot <- ggplot(plot_data, 
                       aes(x = percentile * 100, 
                           y = (theoretical_wage - empirical_wage) / empirical_wage * 100,
                           color = distribution)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(
      values = c(
        "lnorm" = "blue",
        "llogis" = "green",
        "gamma" = "orange",
        "weibull" = "purple",
        "burr" = "red"
      ),
      labels = c(
        "Log-normal",
        "Log-logistic",
        "Gamma",
        "Weibull",
        "Burr (Singh-Maddala)"
      )
    ) +
    labs(
      title = paste("Relative Error by Decile -", type_filter),
      subtitle = paste("Date:", format(max(deciles_df$date), "%B %Y")),
      x = "Percentile",
      y = "Relative Error (%)",
      color = "Distribution"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  # Return results
  list(
    qq_plot = qq_plot,
    error_plot = error_plot,
    fit_metrics = fit_metrics,
    params = list(
      lnorm = lnorm_params,
      llogis = llogis_params,
      gamma = gamma_params,
      weibull = weibull_params,
      burr = burr_params
    )
  )
}

# Usage example for Non-public sector
results_non_public <- compare_distributions_deciles(lognormal_params, wage_deciles, "Non-public")

# Display results
print(results_non_public$qq_plot)
print(results_non_public$error_plot)
print(results_non_public$fit_metrics)

# Display fitted parameters
print("Fitted Parameters:")
str(results_non_public$params)

# If you want to analyze public sector as well
results_public <- compare_distributions_deciles(lognormal_params, wage_deciles, "Public")
