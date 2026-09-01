
# Splitting data

split_data <- function(df, 
                       train_prop = 0.70, 
                       test_prop = 0.30,
                       seed = 123) {
  
  # Check that proportions sum to 1
  if (train_prop + test_prop != 1) {
    stop("The proportions must sum to 1.")
  }
  
  # Set seed
  set.seed(seed)
  
  # Number of observations
  n <- nrow(df)
  
  # Randomly shuffle the rows
  rows <- sample(n)
  
  # Number of observations in the training data
  train_end <- floor(train_prop * n)
  
  # Create training data
  train_data <- df[
    rows[1:train_end], 
  ]
  
  # Create test data
  test_data <- df[
    rows[(train_end + 1):n], 
  ]
  
  # Return datasets
  list(
    train_data = train_data,
    test_data = test_data
  )
}


# Draw model predictions

library(ggplot2)
library(rlang)

plot_regression_line <- function(data,
                            model,
                            xvar,
                            yvar,
                            xlab = "X",
                            ylab = "Y") {
  
  xvar <- enquo(xvar)
  yvar <- enquo(yvar)
  
  # Create data for the regression line
  prediction_df <- data.frame(
    x = seq(
      min(pull(data, !!xvar), na.rm = TRUE),
      max(pull(data, !!xvar), na.rm = TRUE),
      length.out = 100
    )
  )
  
  # Rename x to the actual predictor name
  names(prediction_df) <- as_name(xvar)
  
  # Predictions from the fitted model
  prediction_df$pred <- predict(model, newdata = prediction_df)
  
  # Plot
  ggplot(data, aes(x = !!xvar, y = !!yvar)) +
    geom_point(
      color = "black",
      fill = "palegreen3",
      shape = 21,
      size = 2,
      alpha = 0.7
    ) +
    geom_line(
      data = prediction_df,
      aes(x = !!xvar, y = pred),
      inherit.aes = FALSE,
      linewidth = 1.2
    ) +
    labs(
      x = xlab,
      y = ylab
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank()
    )
}


# Residual plots

plot_residuals <- function(data,
                           xvar,
                           errorvar,
                           xlab = "X",
                           ylab = "Error (Observed - Predicted)") {
  
  xvar <- enquo(xvar)
  errorvar <- enquo(errorvar)
  
  ggplot(data, aes(x = !!xvar, y = !!errorvar)) +
    geom_point(
      color = "black",
      fill = "palegreen3",
      shape = 21,
      size = 2,
      alpha = 0.7
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    labs(
      x = xlab,
      y = ylab
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank()
    )
}



# Plot predictions. Lab 2
# Plot observed and predicted values

library(ggplot2)
library(rlang)

plot_predictions <- function(data,
                             observed,
                             predicted,
                             xlab = "Observed",
                             ylab = "Predicted") {
  
  observed <- enquo(observed)
  predicted <- enquo(predicted)
  
  ggplot(
    data,
    aes(
      x = !!observed,
      y = !!predicted
    )
  ) +
    geom_point(
      color = "black",
      fill = "palegreen3",
      shape = 21,
      size = 2,
      alpha = 0.7
    ) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      linewidth = 1
    ) +
    labs(
      x = xlab,
      y = ylab
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank()
    )
}











# 
# # raw data points plus the single best-fitting regression line
# plot_regression <- function(data,
#                             title = "Linear Regression") {
#   
#   ggplot(data, aes(x = marketing, y = sales)) +
#     geom_point(alpha = 0.6) +
#     geom_smooth(
#       method = "lm",
#       se = FALSE,
#       linewidth = 1.2
#     ) +
#     labs(
#       title = title,
#       x = "Marketing",
#       y = "Sales"
#     ) +
#     theme_minimal(base_size = 14)
# }




# 1. Create a prediction grid for marketing × region
create_prediction_grid <- function(data, marketing_var = marketing, regions = c("A", "B", "C"), n = 100) {
  marketing_var <- rlang::ensym(marketing_var)
  
  marketing_values <- seq(
    min(dplyr::pull(data, !!marketing_var)),
    max(dplyr::pull(data, !!marketing_var)),
    length.out = n
  )
  
  expand.grid(
    marketing = marketing_values,
    region = regions
  )
}



# 2. Add model predictions to the grid
add_predictions <- function(model, grid) {
  grid$pred <- predict(model, newdata = grid)
  grid
}



# 3. Plot regression lines + raw data
plot_regression_lines <- function(data, prediction_df,
                                  title = "Regression Lines by Region") {
  
  ggplot(data, aes(x = marketing, y = sales, color = region)) +
    geom_point(alpha = 0.4) +
    geom_line(
      data = prediction_df,
      aes(x = marketing, y = pred, color = region),
      linewidth = 1.2
    ) +
    labs(
      title = title,
      x = "Marketing",
      y = "Sales"
    ) +
    theme_minimal(base_size = 14)
}



# 4. Plot regression lines (learned on train) on NEW (test) data
plot_regression_on_test <- function(test_data, prediction_df) {
  plot_regression_lines(
    data = test_data,
    prediction_df = prediction_df,
    title = "Regression Lines (trained on train data) on test data"
  )
}


#################################



# Function 1: manual_prediction()
manual_prediction <- function(b0, b_marketing, region_effect, marketing_value) {
  b0 + b_marketing * marketing_value + region_effect
}


# Function 2: prediction_point_df()
prediction_point_df <- function(marketing_value, predicted_sales) {
  data.frame(
    marketing = marketing_value,
    sales = predicted_sales
  )
}


# Function 3: plot_manual_prediction()
plot_manual_prediction <- function(data, pred_df, marketing_var = marketing, sales_var = sales) {
  
  ggplot(data, aes(x = {{ marketing_var }}, y = {{ sales_var }}, color = region)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_point(data = pred_df,
               aes(x = marketing, y = sales),
               color = "red", size = 4, shape = 18) +
    labs(
      title = paste("Manual prediction at marketing =", pred_df$marketing),
      x = "Marketing",
      y = "Sales"
    ) +
    theme_minimal()
}



rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}



rmse_vec <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}



rmse_model <- function(model, data, actual_var = "sales") {
  predicted <- predict(model, newdata = data)
  actual <- data[[actual_var]]
  
  rmse_vec(actual, predicted)
}



rmse_test <- function(model, test_data, actual_var = "sales") {
  rmse_model(model, test_data, actual_var)
}





plot_manual_prediction <- function(df,
                                   marketing_value,
                                   predicted_sales) {
  
  # Data frame for the prediction point
  pred_df <- data.frame(
    marketing = marketing_value,
    sales = predicted_sales
  )
  
  # Plot
  ggplot(df, aes(x = marketing, y = sales, color = region)) +
    geom_point(alpha = 0.6) +                     # raw data
    geom_smooth(method = "lm", se = FALSE) +      # regression line
    geom_point(data = pred_df,
               aes(x = marketing, y = sales),
               color = "red",
               size = 4,
               shape = 18) +                      # manual prediction
    labs(
      title = paste("Manual prediction at marketing =", marketing_value),
      x = "Marketing",
      y = "Sales"
    ) +
    theme_minimal()
}
