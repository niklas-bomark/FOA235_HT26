
library(dplyr)
library(tidyr)
library(ggplot2)

library(dplyr)
library(tidyr)
library(ggplot2)

plot_error_scatter <- function(data, outcome, pred_cols) {
  # data      = data frame
  # outcome   = name of outcome column as string, e.g. "sales"
  # pred_cols = character vector of prediction column names, e.g. c("pred_m1", "pred_m2")
  
  outcome_sym <- rlang::sym(outcome)
  
  data_err <- data %>%
    # build error_* columns for the chosen prediction columns
    mutate(across(all_of(pred_cols),
                  ~ !!outcome_sym - .x,
                  .names = "error_{.col}")) %>%
    pivot_longer(
      cols = starts_with("error_"),
      names_to = "model",
      values_to = "error"
    ) %>%
    # cleaner labels: remove "error_" prefix
    mutate(model = gsub("^error_", "", model))
  
  ggplot(data_err, aes(x = !!outcome_sym, y = error, color = model)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Prediction Error vs Actual",
      x = paste("Actual", outcome),
      y = "Error (Actual – Predicted)"
    ) +
    theme_minimal()
}





library(dplyr)
library(tidyr)
library(ggplot2)

boxplot_residuals <- function(data,
                              outcome,          # character: outcome column name (e.g., "sales")
                              pred_cols,        # character vector of prediction column names
                              xlab = "Model",
                              ylab = "Residual (Actual - Predicted)",
                              fill = "palegreen3",
                              color = "grey30",
                              outlier.color = "darkgreen",
                              outlier.size = 2,
                              base_size = 14) {
  
  outcome_sym <- rlang::sym(outcome)
  
  # Compute residuals for chosen models
  long_data <- data %>%
    mutate(across(all_of(pred_cols),
                  ~ !!outcome_sym - .x,
                  .names = "res_{.col}")) %>%
    pivot_longer(
      cols = starts_with("res_"),
      names_to = "model",
      values_to = "residual"
    ) %>%
    mutate(
      model = gsub("^res_", "", model)  # cleaner model labels
    )
  
  ggplot(long_data, aes(x = model, y = residual)) +
    geom_boxplot(
      fill = fill,
      color = color,
      outlier.color = outlier.color,
      outlier.size = outlier.size
    ) +
    labs(x = xlab, y = ylab) +
    theme_minimal(base_size = base_size) +
    theme(panel.grid = element_blank())
}
