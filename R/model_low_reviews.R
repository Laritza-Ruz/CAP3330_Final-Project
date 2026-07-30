# model_low_reviews.R
# Purpose: Logistic regression model for late_delivery using order-level predictors.
# Note: Reviews data are unavailable; this script models late delivery only.

source("R/validation_helpers.R")

CANDIDATE_PREDICTORS <- c(
  "total_item_price",
  "total_freight_value",
  "item_count",
  "seller_count",
  "estimated_delivery_days",
  "maximum_installments",
  "purchase_month",
  "primary_payment_type"
)

REQUIRED_FIELDS <- c(
  "late_delivery",
  "completed_order",
  "canceled_or_unavailable",
  "actual_delivery_days",
  "actual_delivery_date",
  "estimated_delivery_date",
  CANDIDATE_PREDICTORS
)

calc_skewness <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3 || sd(x) == 0) {
    return(0)
  }
  m <- mean(x)
  s <- sd(x)
  mean((x - m)^3) / (s^3)
}

calculate_auc <- function(predictions, labels) {
  valid_idx <- !is.na(predictions) & !is.na(labels)
  predictions <- predictions[valid_idx]
  labels <- labels[valid_idx]

  n1 <- sum(labels == 1)
  n0 <- sum(labels == 0)
  if (n1 == 0 || n0 == 0) {
    return(NA_real_)
  }

  r <- rank(predictions)
  sum_ranks_pos <- sum(r[labels == 1])
  u_stat <- sum_ranks_pos - (n1 * (n1 + 1)) / 2
  u_stat / (n1 * n0)
}

compute_classification_metrics <- function(actual, predicted, positive_label = 1) {
  tp <- sum(actual == positive_label & predicted == positive_label)
  tn <- sum(actual != positive_label & predicted != positive_label)
  fp <- sum(actual != positive_label & predicted == positive_label)
  fn <- sum(actual == positive_label & predicted != positive_label)

  accuracy <- (tp + tn) / length(actual)
  sensitivity <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  specificity <- if ((tn + fp) > 0) tn / (tn + fp) else 0
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  f1_score <- if ((precision + sensitivity) > 0) {
    2 * (precision * sensitivity) / (precision + sensitivity)
  } else {
    0
  }

  list(
    tp = tp,
    tn = tn,
    fp = fp,
    fn = fn,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    f1_score = f1_score
  )
}

select_threshold_train <- function(train_probs, train_labels, default_threshold = 0.5) {
  prevalence <- mean(train_labels == 1, na.rm = TRUE)
  default_metrics <- compute_classification_metrics(
    train_labels,
    ifelse(train_probs >= default_threshold, 1, 0)
  )

  if (default_metrics$sensitivity > 0 && default_metrics$f1_score >= 0.05) {
    return(list(
      threshold = default_threshold,
      reason = sprintf(
        paste(
          "Default threshold 0.50 retained because training sensitivity was %.3f",
          "and F1 was %.3f at the minority-class prevalence of %.2f%%."
        ),
        default_metrics$sensitivity,
        default_metrics$f1_score,
        100 * prevalence
      )
    ))
  }

  candidate_thresholds <- unique(c(
    seq(0.05, 0.45, by = 0.01),
    default_threshold,
    prevalence
  ))
  candidate_thresholds <- sort(candidate_thresholds[candidate_thresholds > 0 & candidate_thresholds < 1])

  best_threshold <- default_threshold
  best_score <- -Inf

  for (thr in candidate_thresholds) {
    preds <- ifelse(train_probs >= thr, 1, 0)
    metrics <- compute_classification_metrics(train_labels, preds)
    youden <- metrics$sensitivity + metrics$specificity - 1
    if (youden > best_score) {
      best_score <- youden
      best_threshold <- thr
    }
  }

  list(
    threshold = best_threshold,
    reason = sprintf(
      paste(
        "Threshold %.3f selected on training data using Youden's J because class imbalance",
        "(late prevalence = %.2f%%) made the default 0.50 threshold predict almost no late orders",
        "(training sensitivity = %.3f, F1 = %.3f at 0.50)."
      ),
      best_threshold,
      100 * prevalence,
      default_metrics$sensitivity,
      default_metrics$f1_score
    )
  )
}

build_roc_curve <- function(probabilities, labels) {
  thresholds <- sort(unique(c(0, 1, probabilities)), decreasing = TRUE)
  rows <- lapply(thresholds, function(thr) {
    pred <- ifelse(probabilities >= thr, 1, 0)
    metrics <- compute_classification_metrics(labels, pred)
    data.frame(
      threshold = thr,
      tpr = metrics$sensitivity,
      fpr = 1 - metrics$specificity,
      stringsAsFactors = FALSE
    )
  })
  roc <- do.call(rbind, rows)
  roc <- roc[order(roc$threshold, decreasing = TRUE), ]
  rownames(roc) <- NULL
  roc
}

prepare_analysis_population <- function(df) {
  removal_log <- list(
    starting_rows = nrow(df),
    not_completed = 0,
    canceled_or_unavailable = 0,
    missing_actual_delivery_date = 0,
    missing_estimated_delivery_date = 0,
    missing_late_delivery = 0,
    negative_delivery_duration = 0,
    missing_predictors = 0,
    not_paid_payment_type = 0
  )

  keep <- rep(TRUE, nrow(df))

  not_completed <- df$completed_order != 1
  removal_log$not_completed <- sum(not_completed, na.rm = TRUE)
  keep <- keep & !not_completed

  canceled <- df$canceled_or_unavailable == 1
  removal_log$canceled_or_unavailable <- sum(canceled, na.rm = TRUE)
  keep <- keep & !canceled

  missing_actual <- is.na(df$actual_delivery_date) |
    is.na(df$order_delivered_customer_date) |
    df$order_delivered_customer_date == ""
  removal_log$missing_actual_delivery_date <- sum(missing_actual, na.rm = TRUE)
  keep <- keep & !missing_actual

  missing_estimated <- is.na(df$estimated_delivery_date) |
    is.na(df$order_estimated_delivery_date) |
    df$order_estimated_delivery_date == ""
  removal_log$missing_estimated_delivery_date <- sum(missing_estimated, na.rm = TRUE)
  keep <- keep & !missing_estimated

  missing_outcome <- is.na(df$late_delivery)
  removal_log$missing_late_delivery <- sum(missing_outcome, na.rm = TRUE)
  keep <- keep & !missing_outcome

  negative_duration <- !is.na(df$actual_delivery_days) & df$actual_delivery_days < 0
  removal_log$negative_delivery_duration <- sum(negative_duration, na.rm = TRUE)
  keep <- keep & !negative_duration

  not_paid <- df$primary_payment_type == "not_paid"
  removal_log$not_paid_payment_type <- sum(not_paid, na.rm = TRUE)
  keep <- keep & !not_paid

  df_keep <- df[keep, , drop = FALSE]

  missing_predictors <- !complete.cases(df_keep[, CANDIDATE_PREDICTORS, drop = FALSE])
  removal_log$missing_predictors <- sum(missing_predictors)
  df_model <- df_keep[!missing_predictors, , drop = FALSE]

  removal_log$final_rows <- nrow(df_model)
  removal_log$rows_removed <- removal_log$starting_rows - removal_log$final_rows

  list(data = df_model, removal_log = removal_log)
}

validate_model_inputs <- function(df_model) {
  for (var in REQUIRED_FIELDS) {
    validate_variable_exists(df_model, var)
  }

  validate_variable_type(df_model, "late_delivery", "numeric")
  validate_two_groups(df_model, "late_delivery")

  if (nrow(df_model) < 100) {
    stop("[VALIDATION ERROR] Fewer than 100 observations available for modeling.")
  }

  if (any(!is.finite(df_model$total_item_price)) || any(!is.finite(df_model$total_freight_value))) {
    stop("[VALIDATION ERROR] Monetary predictors contain non-finite values.")
  }

  TRUE
}

stratified_split <- function(outcome, train_fraction = 0.8, seed = 42) {
  set.seed(seed)
  idx <- seq_along(outcome)
  late_idx <- idx[outcome == 1]
  ontime_idx <- idx[outcome == 0]

  train_late <- sample(late_idx, size = floor(length(late_idx) * train_fraction))
  train_ontime <- sample(ontime_idx, size = floor(length(ontime_idx) * train_fraction))

  sort(unique(c(train_late, train_ontime)))
}

add_transformed_predictors <- function(df, use_log_item_price, use_log_freight) {
  if (use_log_item_price) {
    df$log1p_total_item_price <- log1p(df$total_item_price)
  }
  if (use_log_freight) {
    df$log1p_total_freight_value <- log1p(df$total_freight_value)
  }
  df
}

build_model_formula <- function(use_log_item_price, use_log_freight) {
  price_term <- if (use_log_item_price) "log1p_total_item_price" else "total_item_price"
  freight_term <- if (use_log_freight) "log1p_total_freight_value" else "total_freight_value"

  as.formula(paste(
    "late_delivery ~",
    paste(
      c(
        price_term,
        freight_term,
        "item_count",
        "seller_count",
        "estimated_delivery_days",
        "maximum_installments",
        "purchase_month",
        "primary_payment_type"
      ),
      collapse = " + "
    )
  ))
}

align_factor_levels <- function(train_data, test_data, factor_cols) {
  for (col in factor_cols) {
    train_levels <- levels(factor(train_data[[col]]))
    train_data[[col]] <- factor(train_data[[col]], levels = train_levels)
    test_data[[col]] <- factor(test_data[[col]], levels = train_levels)
  }

  list(train = train_data, test = test_data)
}

train_predictive_model <- function(
    data_path = "data/processed/order_level_analysis.rds",
    output_dir = "output/model",
    seed = 42) {
  cat("[MODEL] Loading processed order-level dataset...\n")

  if (!file.exists(data_path)) {
    stop(sprintf("Processed dataset not found at %s. Run clean_data.R first.", data_path))
  }

  df <- readRDS(data_path)
  prep <- prepare_analysis_population(df)
  df_model <- prep$data
  removal_log <- prep$removal_log

  validate_model_inputs(df_model)

  missingness <- sapply(CANDIDATE_PREDICTORS, function(var) sum(is.na(df_model[[var]])))
  class_balance <- table(df_model$late_delivery)
  class_balance_pct <- prop.table(class_balance) * 100

  cat("[MODEL] Missingness in candidate predictors (analysis population):\n")
  print(missingness)
  cat("[MODEL] Outcome class balance:\n")
  print(class_balance)
  print(round(class_balance_pct, 2))

  use_log_item_price <- calc_skewness(df_model$total_item_price) > 1
  use_log_freight <- calc_skewness(df_model$total_freight_value) > 1

  df_model <- add_transformed_predictors(df_model, use_log_item_price, use_log_freight)

  if (use_log_item_price && any(!is.finite(df_model$log1p_total_item_price))) {
    stop("[VALIDATION ERROR] log1p(total_item_price) produced non-finite values.")
  }
  if (use_log_freight && any(!is.finite(df_model$log1p_total_freight_value))) {
    stop("[VALIDATION ERROR] log1p(total_freight_value) produced non-finite values.")
  }

  train_idx <- stratified_split(df_model$late_delivery, train_fraction = 0.8, seed = seed)
  train_data <- df_model[train_idx, , drop = FALSE]
  test_data <- df_model[-train_idx, , drop = FALSE]

  aligned <- align_factor_levels(train_data, test_data, c("purchase_month", "primary_payment_type"))
  train_data <- aligned$train
  test_data <- aligned$test

  unseen_payment <- is.na(test_data$primary_payment_type)
  unseen_month <- is.na(test_data$purchase_month)
  if (any(unseen_payment) || any(unseen_month)) {
    warning(
      sprintf(
        "[MODEL WARNING] Excluding %d test rows with unseen factor levels not present in training data.",
        sum(unseen_payment | unseen_month)
      ),
      call. = FALSE
    )
    test_data <- test_data[!(unseen_payment | unseen_month), , drop = FALSE]
  }

  model_formula <- build_model_formula(use_log_item_price, use_log_freight)

  cat("[MODEL] Fitting logistic regression...\n")
  cat("[MODEL] Formula:", deparse(model_formula), "\n")
  model <- glm(model_formula, data = train_data, family = binomial(link = "logit"))

  train_probs <- predict(model, newdata = train_data, type = "response")
  threshold_info <- select_threshold_train(train_probs, train_data$late_delivery)
  classification_threshold <- threshold_info$threshold

  test_probs <- predict(model, newdata = test_data, type = "response")
  test_preds <- ifelse(test_probs >= classification_threshold, 1, 0)

  metrics <- compute_classification_metrics(test_data$late_delivery, test_preds)
  roc_auc <- calculate_auc(test_probs, test_data$late_delivery)
  roc_curve <- build_roc_curve(test_probs, test_data$late_delivery)

  model_summary <- summary(model)
  coef_table <- as.data.frame(model_summary$coefficients)
  coef_table$term <- rownames(coef_table)
  rownames(coef_table) <- NULL
  names(coef_table)[1:4] <- c("estimate", "std_error", "z_value", "p_value")

  ci_mat <- suppressMessages(confint.default(model))
  odds_ratios <- data.frame(
    term = rownames(ci_mat),
    odds_ratio = exp(ci_mat[, 1]),
    or_ci_lower = exp(ci_mat[, 1] - 1.96 * coef_table$std_error[match(rownames(ci_mat), coef_table$term)]),
    or_ci_upper = exp(ci_mat[, 1] + 1.96 * coef_table$std_error[match(rownames(ci_mat), coef_table$term)]),
    stringsAsFactors = FALSE
  )

  if (nrow(odds_ratios) > 1) {
    importance_rank <- odds_ratios[odds_ratios$term != "(Intercept)", ]
    importance_rank$abs_log_or <- abs(log(importance_rank$odds_ratio))
    importance_rank <- importance_rank[order(importance_rank$abs_log_or, decreasing = TRUE), ]
    top_predictors <- head(importance_rank$term, 3)
  } else {
    top_predictors <- character(0)
  }

  prediction_summary <- data.frame(
    statistic = c("min", "q1", "median", "mean", "q3", "max"),
    value = as.numeric(summary(test_probs)),
    stringsAsFactors = FALSE
  )

  confusion_matrix <- data.frame(
    actual_late = c(0, 0, 1, 1),
    predicted_late = c(0, 1, 0, 1),
    count = c(metrics$tn, metrics$fp, metrics$fn, metrics$tp),
    stringsAsFactors = FALSE
  )

  model_metrics <- data.frame(
    metric = c(
      "analysis_sample_size",
      "train_size",
      "test_size",
      "late_prevalence_test_pct",
      "classification_threshold",
      "accuracy",
      "sensitivity",
      "specificity",
      "precision",
      "f1_score",
      "roc_auc",
      "rows_removed",
      "use_log_item_price",
      "use_log_freight"
    ),
    value = c(
      nrow(df_model),
      nrow(train_data),
      nrow(test_data),
      100 * mean(test_data$late_delivery == 1),
      classification_threshold,
      metrics$accuracy,
      metrics$sensitivity,
      metrics$specificity,
      metrics$precision,
      metrics$f1_score,
      roc_auc,
      removal_log$rows_removed,
      as.integer(use_log_item_price),
      as.integer(use_log_freight)
    ),
    stringsAsFactors = FALSE
  )

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(model_metrics, file.path(output_dir, "model_metrics.csv"), row.names = FALSE)
  write.csv(confusion_matrix, file.path(output_dir, "confusion_matrix.csv"), row.names = FALSE)
  write.csv(coef_table, file.path(output_dir, "coefficient_table.csv"), row.names = FALSE)
  write.csv(odds_ratios, file.path(output_dir, "odds_ratios.csv"), row.names = FALSE)
  write.csv(prediction_summary, file.path(output_dir, "prediction_summary.csv"), row.names = FALSE)
  write.csv(roc_curve, file.path(output_dir, "roc_curve.csv"), row.names = FALSE)
  write.csv(
    data.frame(metric = names(removal_log), count = unname(unlist(removal_log))),
    file.path(output_dir, "row_removal_log.csv"),
    row.names = FALSE
  )
  saveRDS(model, file.path(output_dir, "late_delivery_glm.rds"))

  interpretation <- sprintf(
    paste(
      "Higher estimated delivery windows and order complexity variables that rank highly in the model",
      "are associated with late delivery probability in this observational dataset.",
      "The selected threshold (%.3f) balances sensitivity and specificity on training data",
      "for a minority late-delivery class (%.2f%% in the analysis sample).",
      "Associations do not prove causation."
    ),
    classification_threshold,
    100 * mean(df_model$late_delivery == 1)
  )

  warning_text <- paste(
    "WARNING: Predictors are associated with late delivery but do not prove causal effects.",
    "Threshold selection used training data only; test data were reserved for evaluation."
  )
  warning(warning_text, call. = FALSE)

  cat(sprintf("[MODEL] Analysis sample size: %d\n", nrow(df_model)))
  cat(sprintf("[MODEL] Train size: %d | Test size: %d\n", nrow(train_data), nrow(test_data)))
  cat(sprintf("[MODEL] Threshold: %.3f\n", classification_threshold))
  cat(sprintf("[MODEL] Test accuracy: %.4f | sensitivity: %.4f | specificity: %.4f | AUC: %.4f\n",
              metrics$accuracy, metrics$sensitivity, metrics$specificity, roc_auc))

  list(
    model = model,
    model_formula = model_formula,
    final_predictors = CANDIDATE_PREDICTORS,
    transformed_predictors = c(
      if (use_log_item_price) "log1p_total_item_price" else "total_item_price",
      if (use_log_freight) "log1p_total_freight_value" else "total_freight_value",
      "item_count",
      "seller_count",
      "estimated_delivery_days",
      "maximum_installments",
      "purchase_month",
      "primary_payment_type"
    ),
    analysis_sample_size = nrow(df_model),
    train_size = nrow(train_data),
    test_size = nrow(test_data),
    train_distribution = table(train_data$late_delivery),
    test_distribution = table(test_data$late_delivery),
    class_balance_pct = class_balance_pct,
    missingness = missingness,
    removal_log = removal_log,
    classification_threshold = classification_threshold,
    threshold_reason = threshold_info$reason,
    coefficients = coef_table,
    odds_ratios = odds_ratios,
    top_predictors = top_predictors,
    confusion_matrix = confusion_matrix,
    accuracy = metrics$accuracy,
    sensitivity = metrics$sensitivity,
    specificity = metrics$specificity,
    precision = metrics$precision,
    f1_score = metrics$f1_score,
    roc_auc = roc_auc,
    prediction_summary = prediction_summary,
    interpretation = interpretation,
    warning = warning_text,
    output_dir = output_dir
  )
}

if (sys.nframe() == 0L) {
  invisible(train_predictive_model())
}
