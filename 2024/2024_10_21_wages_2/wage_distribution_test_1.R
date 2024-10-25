neg_log_likelihood_loglogistic <- function(params, x, weights) {
  alpha <- params[1]
  beta <- params[2]
  
  -sum(weights * (log(beta) - beta * log(alpha) + (beta - 1) * log(x) -
                    2 * log(1 + (x / alpha)^beta)))
}

library(stats)

estimate_loglogistic_params <- function(data) {
  opt_result <- optim(
    par = c(median(data$wages_cumulative), 1),  # Initial guesses for alpha and beta
    fn = neg_log_likelihood_loglogistic,
    x = data$wages_cumulative,
    weights = data$employees,
    method = "L-BFGS-B",
    lower = c(0, 0),  # Parameters must be positive
    upper = c(Inf, Inf)
  )
  
  return(tibble(alpha = opt_result$par[1], beta = opt_result$par[2]))
}

lognormal_params <- lognormal_params_return(adjusted_prices = FALSE, return_db = 1) 

loglogistic_params <- lognormal_params |>  # Assuming this is your original data frame
  group_by(date) |>
  group_modify(~ estimate_loglogistic_params(.x)) |>
  ungroup()



alpha_param <- loglogistic_params$alpha |> last()  # scale
beta_param <- loglogistic_params$beta |> last() # shape


library(flexsurv)

last_date_llogis_values <- 
  tibble(
    wages = qllogis(seq(0.01,0.99,0.01), shape = beta_param, scale = alpha_param, lower.tail = TRUE, log.p = FALSE)
  )

########################################

wage_1000 |> 
  filter(date == max(date))

last_date_lnorm_params <- 
  lognormal_params_return(adjusted_prices = FALSE, return_db = 2) |> 
  filter(date == max(date))


last_date_lnorm_values <- 
  tibble(
    wages = qlnorm(seq(0.01,0.99,0.01), last_date_lnorm_params$mean, last_date_lnorm_params$sd)
  )

ggplot() +
  geom_density(
    data = wage_1000 |> filter(date == max(date)),
    aes(wages), color = "red"
  ) +
  geom_density(
    data = last_date_lnorm_values,
    aes(wages), color = "blue"
  ) +
  geom_density(
    data = last_date_llogis_values,
    aes(wages), color = "green"
  ) +
  scale_x_continuous(breaks = seq(100, 1000, 100) * 1000, labels = number_format()) +
  labs(
    title = "wage distrubution calculated on weighted quantiles (red)\nand log-normal dist. asumption with plnorm (blue)
    green is log-logistic distribution",
    subtitle = "date == max(date)"
  )


###################################################

library(GB2)
library(numDeriv)

# Negative log-likelihood function for GB2 with weights
neg_log_likelihood_gb2 <- function(params, x, weights) {
  a <- params[1]
  b <- params[2]
  p <- params[3]
  q <- params[4]
  
  # Calculate log-density manually
  log_density <- log(a) + (a*p - 1) * log(x) - (p + q) * log(1 + (x/b)^a) + 
    log(gamma(p + q)) - log(gamma(p)) - log(gamma(q)) - a*p*log(b)
  
  -sum(weights * log_density)
}

# Function to estimate GB2 parameters
estimate_gb2_params <- function(data) {
  # Initial parameter guesses
  init_params <- c(
    a = 1,
    b = median(data$wages_cumulative),
    p = 1,
    q = 1
  )
  
  # Optimization
  opt_result <- optim(
    par = init_params,
    fn = neg_log_likelihood_gb2,
    x = data$wages_cumulative,
    weights = data$employees,
    method = "L-BFGS-B",
    lower = c(0.01, 0.01, 0.01, 0.01),  # Avoid zero for numerical stability
    upper = c(Inf, Inf, Inf, Inf),
    control = list(maxit = 1000)
  )
  
  # Return estimated parameters
  return(tibble(
    a = opt_result$par[1],
    b = opt_result$par[2],
    p = opt_result$par[3],
    q = opt_result$par[4]
  ))
}

# Apply the estimation to your data
gb2_params <- lognormal_params |>  # Assuming this is your original data frame
  group_by(date) |>
  group_modify(~ estimate_gb2_params(.x)) |>
  ungroup()

# Print the results
print(gb2_params)

###################################################



# Define the quantile function for GB2
gb2_quantile <- function(p, a, b, p_param, q_param) {
  f <- function(x) pgb2(x, a, b, p_param, q_param) - p
  uniroot(f, interval = c(0, 1e7))$root
}

# Get the last date parameters
last_date <- max(gb2_params$date)
last_date_gb2_params <- gb2_params |> filter(date == last_date)

# Calculate quantiles for GB2
last_date_gb2_values <- tibble(
  wages = sapply(seq(0.01, 0.99, 0.01), function(p) 
    gb2_quantile(p, last_date_gb2_params$a, last_date_gb2_params$b, 
                 last_date_gb2_params$p, last_date_gb2_params$q))
)

# Create the plot
ggplot() +
  geom_density(
    data = wage_1000 |> filter(date == max(date)),
    aes(wages), color = "red"
  ) +
  geom_density(
    data = last_date_lnorm_values,
    aes(wages), color = "blue"
  ) +
  geom_density(
    data = last_date_llogis_values,
    aes(wages), color = "green"
  ) +
  geom_density(
    data = last_date_gb2_values,
    aes(wages), color = "purple"
  ) +
  scale_x_continuous(breaks = seq(100, 1000, 100) * 1000, labels = number_format()) +
  labs(
    title = "Wage distribution calculated on weighted quantiles (red)",
    subtitle = paste("Log-normal (blue), Log-logistic (green), and GB2 (purple) distributions\nDate:", last_date),
    x = "Wages",
    y = "Density"
  ) +
  theme_minimal()

#################################################
# without log transformation

neg_log_likelihood_gb2 <- function(params, x, weights) {
  a <- params[1]
  b <- params[2]
  p <- params[3]
  q <- params[4]
  
  -sum(weights * log(dgb2(x, a, b, p, q)))
}

estimate_gb2_params <- function(data) {
  # Initial parameter guesses
  init_params <- c(
    a = 2,
    b = median(data$wages_cumulative),
    p = 1,
    q = 1
  )
  
  # Optimization
  opt_result <- optim(
    par = init_params,
    fn = neg_log_likelihood_gb2,
    x = data$wages_cumulative,
    weights = data$employees,
    method = "L-BFGS-B",
    lower = c(0.01, 0.01, 0.01, 0.01),
    upper = c(Inf, Inf, Inf, Inf),
    control = list(maxit = 5000)
  )
  
  # Return estimated parameters
  return(tibble(
    a = opt_result$par[1],
    b = opt_result$par[2],
    p = opt_result$par[3],
    q = opt_result$par[4]
  ))
}

gb2_params <- lognormal_params |>
  group_by(date) |>
  group_modify(~ estimate_gb2_params(.x)) |>
  ungroup()


ggplot() +
  geom_density(
    data = wage_1000 |> filter(date == max(date)),
    aes(wages), color = "red"
  ) +
  geom_density(
    data = last_date_lnorm_values,
    aes(wages), color = "blue"
  ) +
  geom_density(
    data = last_date_llogis_values,
    aes(wages), color = "green"
  ) +
  geom_density(
    data = last_date_gb2_values,
    aes(wages), color = "purple"
  ) +
  scale_x_continuous(breaks = seq(100, 1000, 100) * 1000, labels = number_format()) +
  labs(
    title = "Wage distribution calculated on weighted quantiles (red)",
    subtitle = paste("Log-normal (blue), Log-logistic (green), and GB2 (purple) distributions\nDate:", last_date),
    x = "Wages",
    y = "Density"
  ) +
  theme_minimal()


###########################################################


library(stats)
library(dplyr)
library(ggplot2)
library(scales)

# Negative log-likelihood function for Gamma distribution
neg_log_likelihood_gamma <- function(params, x, weights) {
  shape <- params[1]  # k parameter (shape)
  scale <- params[2]  # theta parameter (scale)
  
  -sum(weights * dgamma(x, shape = shape, scale = scale, log = TRUE))
}

# Function to estimate Gamma parameters
estimate_gamma_params <- function(data) {
  # Initial parameter guesses
  # Using method of moments for initial guesses
  mean_val <- weighted.mean(data$wages_cumulative, data$employees)
  var_val <- weighted.mean((data$wages_cumulative - mean_val)^2, data$employees)
  
  init_shape <- (mean_val^2) / var_val
  init_scale <- var_val / mean_val
  
  # Optimization
  opt_result <- optim(
    par = c(init_shape, init_scale),
    fn = neg_log_likelihood_gamma,
    x = data$wages_cumulative,
    weights = data$employees,
    method = "L-BFGS-B",
    lower = c(0.01, 0.01),
    upper = c(Inf, Inf),
    control = list(maxit = 5000)
  )
  
  # Return estimated parameters
  return(tibble(
    shape = opt_result$par[1],
    scale = opt_result$par[2]
  ))
}

# Apply to your data
gamma_params <- lognormal_params |>
  group_by(date) |>
  group_modify(~ estimate_gamma_params(.x)) |>
  ungroup()

# Get last date parameters for plotting
last_date <- max(gamma_params$date)
last_date_gamma_params <- gamma_params |> filter(date == last_date)

# Calculate quantiles for Gamma distribution
last_date_gamma_values <- tibble(
  wages = qgamma(seq(0.01, 0.99, 0.01), 
                 shape = last_date_gamma_params$shape, 
                 scale = last_date_gamma_params$scale)
)

# Plot with all distributions
ggplot() +
  geom_density(
    data = wage_1000 |> filter(date == max(date)),
    aes(wages), color = "red"
  ) +
  geom_density(
    data = last_date_lnorm_values,
    aes(wages), color = "blue"
  ) +
  geom_density(
    data = last_date_llogis_values,
    aes(wages), color = "green"
  ) +
  geom_density(
    data = last_date_gamma_values,
    aes(wages), color = "orange"
  ) +
  scale_x_continuous(breaks = seq(100, 1000, 100) * 1000, labels = number_format()) +
  labs(
    title = "Wage distribution calculated on weighted quantiles (red)",
    subtitle = paste("Log-normal (blue), Log-logistic (green), and Gamma (orange) distributions\nDate:", last_date),
    x = "Wages",
    y = "Density"
  ) +
  theme_minimal()

##################################################################################



