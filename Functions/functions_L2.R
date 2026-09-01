

library(ggplot2)
library(scales)

simulate_sales <- function(intercept = 100, slope = 10, 
                           n_obs = 300, noise_sd = 0,
                           x_max = 200, y_max = 1000) {
  set.seed(123)
  
  # Create data
  marketing <- round(runif(n_obs, 0, x_max), 0)
  sales <- intercept + slope * marketing + 
    round(rnorm(n_obs, 0, noise_sd), 0)
  
  data <- data.frame(
    marketing = marketing,
    sales = sales
  )
  
  # Plot
  p <- ggplot(data, aes(x = marketing, y = sales)) +
    geom_point(color = "steelblue", alpha = 0.6) +
    labs(title = "Relationship between Marketing and Sales",
         x = "Marketing (thousand SEK)",
         y = "Sales (thousand SEK)") +
    scale_x_continuous(labels = label_number(big.mark = " ", decimal.mark = ","),
                       limits = c(0, x_max),
                       expand = c(0,0)) +
    scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ","),
                       limits = c(0, y_max),
                       expand = c(0,0)) +
    theme_minimal()
  
  return(list(data = data, plot = p))
}

# Example usage:
# result <- simulate_sales(intercept = 100, slope = 10, n_obs = 300, noise_sd = 20)
# df <- result$data
# result$plot



library(ggplot2)

simulate_ols <- function(intercept = 2,
                         slope = 1.5,
                         n_obs = 30,
                         noise_sd = 2,
                         x_max = 10,
                         y_max = NULL) {
  
  set.seed(123)
  
  # 1. Create a "reality" with some noise
  x <- runif(n_obs, 0, x_max)
  y_true <- intercept + slope * x + rnorm(n_obs, 0, noise_sd)
  data <- data.frame(x, y_true)
  
  # 2. Fit a linear model (OLS)
  model <- lm(y_true ~ x, data = data)
  
  # 3. Add model predictions and residuals
  data$pred <- predict(model)
  data$resid <- residuals(model)
  
  # 4. Visualization: data points, regression line, and residuals
  p <- ggplot(data, aes(x = x, y = y_true)) +
    geom_point(color = "steelblue", size = 2) +
    geom_abline(intercept = coef(model)[1], slope = coef(model)[2],
                color = "grey30", linewidth = 1) +
    geom_segment(aes(xend = x, yend = pred),
                 color = "tomato", alpha = 0.7) +
    labs(
      title = "OLS: How the model finds the best-fitting line",
      subtitle = "The red lines show the residuals – the distances OLS tries to minimize",
      x = "x (independent variable)",
      y = "y (dependent variable)"
    ) +
    theme_minimal(base_size = 13)
  
  # If user provides y_max, limit the y-axis
  if (!is.null(y_max)) {
    p <- p + ylim(0, y_max)
  }
  
  return(list(data = data, plot = p))
}




library(ggplot2)
library(dplyr)

simulate_logistic_data <- function(intercept = -5,
                              slope = 1,
                              n_obs = 100,
                              x_max = 10) {
  
  set.seed(123)
  
  # 1. Create simulated "reality"
  x <- runif(n_obs, 0, x_max)
  
  # Logistic function: probability of y = 1
  p <- 1 / (1 + exp(-(intercept + slope * x)))
  
  # Outcome variable (0 = no, 1 = yes)
  y <- rbinom(n_obs, size = 1, prob = p)
  
  # Combine into data frame
  data <- data.frame(x, y, p)
  
  # 2. Fit logistic regression model
  model <- glm(y ~ x, data = data, family = binomial)
  
  # 3. Predict probabilities
  data$pred <- predict(model, type = "response")
  
  # 4. Visualization
  p_plot <- ggplot(data, aes(x = x, y = y)) +
    geom_point(aes(color = as.factor(y)), size = 2, alpha = 0.8) +
    #geom_line(aes(y = pred), color = "grey30", linewidth = 1.2) +
    theme_minimal(base_size = 13) +
    scale_color_manual(values = c("steelblue", "tomato"),
                       labels = c("0 = No", "1 = Yes"),
                       name = "Outcome") +
    labs(
      title = "Logistic Regression: Modelled Probability Curve",
      #subtitle = "Grey curve shows the logistic prediction",
      x = "x",
      y = "Probability / Outcome"
    )
  
  return(list(data = data, plot = p_plot, model = model))
}
