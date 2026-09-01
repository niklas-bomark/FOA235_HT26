



plot_logistic_predictions <- function(model,
                                      df,
                                      x_var = "marketing",
                                      y_var = "purchase",
                                      pred_var = "pred_prob",
                                      threshold = NULL,     # horizontal prob threshold
                                      title = "Predicted probabilities from logistic regression",
                                      x_label = "Marketing (X)",
                                      y_label = "Probability of outcome (Y = 1)") {
  
  library(ggplot2)
  library(dplyr)
  
  # -------------------------------------------------------------
  # 0. Prediktera sannolikheter (efter logistisk transformation)
  # -------------------------------------------------------------
  df[[pred_var]] <- predict(model, newdata = df, type = "response")
  
  # -------------------------------------------------------------
  # 1. Compute x_threshold automatically (if threshold given)
  # -------------------------------------------------------------
  x_threshold <- NULL
  if (!is.null(threshold)) {
    # find row where predicted probability is closest to the threshold
    idx <- which.min(abs(df[[pred_var]] - threshold))
    x_threshold <- df[[x_var]][idx]
  }
  
  # -------------------------------------------------------------
  # 2. Build plot
  # -------------------------------------------------------------
  p <- ggplot() +
    # Actual observations
    geom_jitter(
      data = df,
      aes(x = .data[[x_var]], y = .data[[y_var]]),
      color = "grey70",
      alpha = 0.8,
      width = 0.2,
      height = 0.02,
      size = 0.5
    ) +
    # Model predictions
    geom_point(
      data = df,
      aes(x = .data[[x_var]], y = .data[[pred_var]]),
      color = "steelblue",
      size = 1.2
    ) +
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    theme_minimal()
  
  # Add horizontal threshold line
  if (!is.null(threshold)) {
    p <- p + geom_hline(
      yintercept = threshold,
      linetype = "dashed",
      color = "gray50"
    )
  }
 
 
  return(p)
}

















compute_classification_results <- function(df,
                                           model = NULL,                 # optional logistic model
                                           prob_var = "pred_prob",       # name of probability column to use/create
                                           actual_var = "purchase",
                                           predicted_var = "pred_purchase_binary",
                                           threshold_value = NULL) {
  
  # -------------------------------------------------------------
  # 0. If a model is supplied, compute predicted probabilities
  # -------------------------------------------------------------
  if (!is.null(model)) {
    df[[prob_var]] <- predict(model, newdata = df, type = "response")
  }
  
  # -------------------------------------------------------------
  # 1. Convert probabilities to binary predictions using threshold
  # -------------------------------------------------------------
  if (!is.null(threshold_value)) {
    
    if (is.null(df[[prob_var]])) {
      stop("Probability column specified in 'prob_var' does not exist and no model was provided.")
    }
    
    df[[predicted_var]] <- ifelse(df[[prob_var]] > threshold_value, 1, 0)
  }
  
  # Ensure the predicted column exists
  if (is.null(df[[predicted_var]])) {
    stop("Predicted binary column does not exist. Provide a threshold and either a model or an existing probability column.")
  }
  
  predicted <- df[[predicted_var]]
  actual    <- df[[actual_var]]
  
  # -------------------------------------------------------------
  # 2. Confusion matrix components
  # -------------------------------------------------------------
  TP <- sum(predicted == 1 & actual == 1)
  TN <- sum(predicted == 0 & actual == 0)
  FP <- sum(predicted == 1 & actual == 0)
  FN <- sum(predicted == 0 & actual == 1)
  
  confusion_matrix <- matrix(
    c(TP, FP, FN, TN),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      Predicted = c("1", "0"),
      Actual    = c("1", "0")
    )
  )
  
  # -------------------------------------------------------------
  # 3. Performance metrics
  # -------------------------------------------------------------
  accuracy  <- (TP + TN) / (TP + TN + FP + FN)
  precision <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
  recall    <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))
  f1_score  <- ifelse(is.na(precision) | is.na(recall) | (precision + recall) == 0,
                      NA,
                      2 * (precision * recall) / (precision + recall))
  
  # -------------------------------------------------------------
  # 4. Output all results (including updated df)
  # -------------------------------------------------------------
  return(list(
    confusion_matrix = confusion_matrix,
    accuracy         = accuracy,
    precision        = precision,
    recall           = recall,
    f1_score         = f1_score,
    data             = df   # <---- added so pred_prob and predictions are returned
  ))
}



plot_roc_curve <- function(df,
                           prob_var = "pred_prob",
                           actual_var = "purchase",
                           title = "ROC Curve") {
  
  library(pROC)
  library(ggplot2)
  
  # Extract variables
  prob <- df[[prob_var]]
  actual <- df[[actual_var]]
  
  # Compute ROC
  roc_obj <- roc(actual, prob)
  
  # Extract AUC
  auc_value <- auc(roc_obj)
  
  # Prepare ROC data for ggplot
  roc_data <- data.frame(
    specificity = roc_obj$specificities,
    sensitivity = roc_obj$sensitivities
  )
  
  # Make plot
  p <- ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity)) +
    geom_line(color = "steelblue", size = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
    labs(
      title = paste0(title, " (AUC = ", round(auc_value, 3), ")"),
      x = "False Positive Rate (1 − Specificity)",
      y = "True Positive Rate (Sensitivity)"
    ) +
    theme_minimal()
  
  return(list(
    roc_object = roc_obj,
    auc = auc_value,
    plot = p
  ))
}





plot_binary_counts <- function(df,
                               outcome_var = "purchase",
                               title = "Number of observations by outcome",
                               x_label = "Outcome",
                               y_label = "Count",
                               fill_color = "steelblue") {
  
  library(ggplot2)
  
  ggplot(df, aes(x = factor(.data[[outcome_var]]))) +
    geom_bar(fill = fill_color, alpha = 0.7) +
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    theme_minimal()
}





plot_group_counts_for_outcome <- function(df,
                                          outcome_var = "purchase",
                                          outcome_value = 1,
                                          group_var = "region",
                                          title = "Number of observations per group",
                                          x_label = "Group",
                                          y_label = "Count",
                                          fill_color = "steelblue") {
  
  library(ggplot2)
  library(dplyr)
  
  # Filter for the specified outcome value (e.g., purchase == 1)
  df_filtered <- df %>% 
    filter(.data[[outcome_var]] == outcome_value)
  
  ggplot(df_filtered, aes(x = .data[[group_var]])) +
    geom_bar(fill = fill_color, alpha = 0.7) +
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    theme_minimal()
}




plot_train_test_distribution <- function(train_data,
                                         test_data,
                                         outcome_var = "purchase",
                                         title = "Distribution of outcome in training and test data",
                                         x_label = "Outcome (0 = no, 1 = yes)",
                                         y_label = "Number of observations",
                                         train_color = "steelblue",
                                         test_color = "orange") {
  
  library(dplyr)
  library(ggplot2)
  
  # Combine datasets and compute counts
  df_dist <- bind_rows(
    train_data %>% mutate(dataset = "Train"),
    test_data %>% mutate(dataset = "Test")
  ) %>%
    group_by(dataset, .data[[outcome_var]]) %>%
    summarise(count = n(), .groups = "drop")
  
  # Create plot
  ggplot(df_dist, aes(x = factor(.data[[outcome_var]]), 
                      y = count, 
                      fill = dataset)) +
    geom_col(position = "dodge", alpha = 0.7) +
    scale_fill_manual(values = c("Train" = train_color, "Test" = test_color)) +
    labs(
      title = title,
      x = x_label,
      y = y_label,
      fill = "Dataset"
    ) +
    theme_minimal()
}

