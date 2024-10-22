library(tidyverse)
library(stats)
library(GB2)
library(flexsurv)
library(actuar)  # For burr distribution
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

# Main analysis function
analyze_latest_wage_distribution <- function(df) {
  # Get latest date data
  latest_data <- get_latest_data(df)
  max_date <- max(df$date)
  
  # Fit distributions
  lnorm_params <- fit_lognormal(latest_data)
  llogis_params <- fit_loglogistic(latest_data)
  gamma_params <- fit_gamma(latest_data)
  weibull_params <- fit_weibull(latest_data)
  burr_params <- fit_burr(latest_data)
  
  # Generate theoretical distributions
  probs <- seq(0.01, 0.99, 0.01)
  theoretical_values <- tibble(
    lnorm = qlnorm(probs, lnorm_params$meanlog, lnorm_params$sdlog),
    llogis = qllogis(probs, scale = llogis_params$scale, shape = llogis_params$shape),
    gamma = qgamma(probs, shape = gamma_params$shape, scale = gamma_params$scale),
    weibull = qweibull(probs, shape = weibull_params$shape, scale = weibull_params$scale),
    burr = qburr(probs, shape1 = burr_params$shape1, shape2 = burr_params$shape2, 
                 scale = burr_params$scale)
  ) %>%
    pivot_longer(everything(), names_to = "distribution", values_to = "wages")
  
  # Create plot
  ggplot() +
    # Empirical distribution
    geom_density(
      data = latest_data,
      aes(x = wages_cumulative, weight = employees, color = "Empirical"),
      linewidth = 1
    ) +
    # Fitted distributions
    geom_density(
      data = theoretical_values,
      aes(x = wages, color = distribution),
      linewidth = 0.8
    ) +
    scale_color_manual(
      values = c(
        "Empirical" = "black",
        "lnorm" = "blue",
        "llogis" = "green",
        "gamma" = "orange",
        "weibull" = "purple",
        "burr" = "red"
      ),
      labels = c(
        "Empirical",
        "Log-normal",
        "Log-logistic",
        "Gamma",
        "Weibull",
        "Burr (Singh-Maddala)"
      ),
      name = "Distribution"
    ) +
    scale_x_continuous(
      breaks = seq(0, max(latest_data$wages_cumulative), 100000),
      labels = number_format(big.mark = ",")
    ) +
    labs(
      title = "Wage Distribution Fits by Sector",
      subtitle = paste("Date:", format(max_date, "%B %Y")),
      x = "Wages (AMD)",
      y = "Density"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

# Run the analysis
result_plot <- analyze_latest_wage_distribution(lognormal_params)
print(result_plot)

# Print fitted parameters
print_fitted_parameters <- function(df) {
  latest_data <- get_latest_data(df)
  
  # Fit distributions
  lnorm_params <- fit_lognormal(latest_data)
  llogis_params <- fit_loglogistic(latest_data)
  gamma_params <- fit_gamma(latest_data)
  weibull_params <- fit_weibull(latest_data)
  burr_params <- fit_burr(latest_data)
  
  # Print parameters
  cat("\nFitted Parameters:\n")
  cat("\nLog-normal distribution:")
  cat("\n  meanlog:", lnorm_params$meanlog)
  cat("\n  sdlog:", lnorm_params$sdlog)
  
  cat("\n\nLog-logistic distribution:")
  cat("\n  scale:", llogis_params$scale)
  cat("\n  shape:", llogis_params$shape)
  
  cat("\n\nGamma distribution:")
  cat("\n  shape:", gamma_params$shape)
  cat("\n  scale:", gamma_params$scale)
  
  cat("\n\nWeibull distribution:")
  cat("\n  shape:", weibull_params$shape)
  cat("\n  scale:", weibull_params$scale)
  
  cat("\n\nBurr (Singh-Maddala) distribution:")
  cat("\n  shape1:", burr_params$shape1)
  cat("\n  shape2:", burr_params$shape2)
  cat("\n  scale:", burr_params$scale)
  cat("\n")
}