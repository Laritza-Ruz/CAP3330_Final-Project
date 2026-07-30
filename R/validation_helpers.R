# validation_helpers.R
# Purpose: Programmatic validation functions for analysis inputs and data preparation.

#' Validate Variable Exists
validate_variable_exists <- function(df, var_name) {
  if (!var_name %in% names(df)) {
    stop(sprintf(
      "[VALIDATION ERROR] Variable '%s' does not exist in the dataset. Please verify the column name.",
      var_name
    ))
  }
  return(TRUE)
}

#' Validate Variable Type
validate_variable_type <- function(df, var_name, expected_type) {
  validate_variable_exists(df, var_name)
  actual_type <- class(df[[var_name]])

  if (expected_type == "numeric" && !actual_type %in% c("numeric", "integer", "double")) {
    stop(sprintf(
      "[VALIDATION ERROR] Variable '%s' is of type '%s', but type 'numeric' was expected.",
      var_name, actual_type
    ))
  } else if (expected_type == "factor" && !actual_type %in% c("factor", "character")) {
    stop(sprintf(
      "[VALIDATION ERROR] Variable '%s' is of type '%s', but type 'factor' or 'character' was expected.",
      var_name, actual_type
    ))
  } else if (expected_type %in% c("character", "logical") && actual_type != expected_type) {
    stop(sprintf(
      "[VALIDATION ERROR] Variable '%s' is of type '%s', but type '%s' was expected.",
      var_name, actual_type, expected_type
    ))
  }
  return(TRUE)
}

#' Validate Grouping Variable Has Exactly Two Groups
validate_two_groups <- function(df, grouping_var) {
  validate_variable_exists(df, grouping_var)

  values <- df[[grouping_var]]
  values <- values[!is.na(values)]
  unique_values <- unique(values)
  num_groups <- length(unique_values)

  if (num_groups != 2) {
    stop(sprintf(
      "[VALIDATION ERROR] Grouping variable '%s' contains %d groups (%s). Exactly 2 groups are required for this analysis.",
      grouping_var, num_groups, paste(unique_values, collapse = ", ")
    ))
  }
  return(TRUE)
}

#' Validate Required Date
validate_required_date <- function(df, date_col) {
  validate_variable_exists(df, date_col)

  non_na_dates <- df[[date_col]][!is.na(df[[date_col]]) & df[[date_col]] != ""]
  if (length(non_na_dates) == 0) {
    stop(sprintf(
      "[VALIDATION ERROR] Date column '%s' contains only missing or empty values. Analysis cannot proceed.",
      date_col
    ))
  }
  return(TRUE)
}

#' Validate Prediction Inputs
validate_prediction_inputs <- function(required_vars, input_df) {
  for (var in required_vars) {
    if (!var %in% names(input_df)) {
      stop(sprintf(
        "[VALIDATION ERROR] Missing required prediction input variable: '%s'. Prediction aborted.",
        var
      ))
    }
    if (any(is.na(input_df[[var]]))) {
      stop(sprintf(
        "[VALIDATION ERROR] Input variable '%s' contains missing values (NA) which are not permitted during prediction.",
        var
      ))
    }
  }
  return(TRUE)
}

#' Validate No Causal Conclusions in Statement
validate_no_causal_conclusion <- function(statement) {
  causal_keywords <- c("cause", "caused", "causation", "proves", "proving", "forces", "determines")
  lower_stmt <- tolower(statement)
  matched_keywords <- sapply(causal_keywords, function(kw) grepl(kw, lower_stmt))

  if (any(matched_keywords)) {
    stop(sprintf(
      "[VALIDATION ERROR] The conclusion statement contains causal words (%s). Causal claims are prohibited on observational datasets; only associations can be inferred.",
      paste(names(matched_keywords)[matched_keywords], collapse = ", ")
    ))
  }
  return(TRUE)
}

#' Validate a Single Column Is Unique
validate_unique_column <- function(df, col_name, dataset_label = "dataset") {
  validate_variable_exists(df, col_name)
  n_rows <- nrow(df)
  n_unique <- length(unique(df[[col_name]]))

  if (n_rows != n_unique) {
    stop(sprintf(
      "[VALIDATION ERROR] '%s' is not unique in %s (%d rows, %d unique values).",
      col_name, dataset_label, n_rows, n_unique
    ))
  }

  message(sprintf("[VALIDATION PASS] '%s' is unique in %s (%d rows).", col_name, dataset_label, n_rows))
  return(TRUE)
}

#' Validate a Composite Key Is Unique
validate_unique_composite_key <- function(df, key_cols, dataset_label = "dataset") {
  for (col in key_cols) {
    validate_variable_exists(df, col)
  }

  key <- do.call(paste, c(df[key_cols], sep = "|"))
  n_rows <- nrow(df)
  n_unique <- length(unique(key))

  if (n_rows != n_unique) {
    stop(sprintf(
      "[VALIDATION ERROR] Composite key (%s) is not unique in %s (%d rows, %d unique combinations).",
      paste(key_cols, collapse = " + "), dataset_label, n_rows, n_unique
    ))
  }

  message(sprintf(
    "[VALIDATION PASS] Composite key (%s) is unique in %s (%d rows).",
    paste(key_cols, collapse = " + "), dataset_label, n_rows
  ))
  return(TRUE)
}

#' Validate Row Count Did Not Increase After a Join
validate_row_count_unchanged <- function(before_count, after_count, context = "join") {
  if (after_count != before_count) {
    stop(sprintf(
      "[VALIDATION ERROR] Row count changed during %s (before: %d, after: %d).",
      context, before_count, after_count
    ))
  }

  message(sprintf("[VALIDATION PASS] Row count unchanged after %s (%d rows).", context, after_count))
  return(TRUE)
}

#' Validate an Aggregated Numeric Total Was Preserved
validate_total_preserved <- function(original_total, merged_total, metric_label, tolerance = 0.01) {
  diff_abs <- abs(merged_total - original_total)
  if (diff_abs > tolerance) {
    stop(sprintf(
      "[VALIDATION ERROR] %s total mismatch (original: %.2f, merged: %.2f, difference: %.2f).",
      metric_label, original_total, merged_total, diff_abs
    ))
  }

  message(sprintf(
    "[VALIDATION PASS] %s total preserved (original: %.2f, merged: %.2f).",
    metric_label, original_total, merged_total
  ))
  return(TRUE)
}

#' Validate Delivery Duration Columns Contain No Negative Values
validate_no_negative_durations <- function(df, duration_cols = c("actual_delivery_days", "estimated_delivery_days")) {
  results <- list()

  for (col in duration_cols) {
    if (!col %in% names(df)) {
      next
    }

    values <- df[[col]]
    negatives <- !is.na(values) & values < 0
    n_neg <- sum(negatives)

    if (n_neg > 0) {
      stop(sprintf(
        "[VALIDATION ERROR] '%s' contains %d negative delivery durations. Negative durations are not accepted.",
        col, n_neg
      ))
    }

    results[[col]] <- list(
      checked = TRUE,
      negative_count = n_neg,
      na_count = sum(is.na(values))
    )
    message(sprintf("[VALIDATION PASS] '%s' has no negative durations (%d NA values).", col, results[[col]]$na_count))
  }

  return(results)
}

#' Run All Data-Preparation Validation Checks
validate_data_preparation <- function(order_level_df,
                                      seller_order_df,
                                      original_order_count,
                                      original_payment_total,
                                      original_item_price_total,
                                      original_freight_total,
                                      tolerance = 0.01) {
  cat("[VALIDATION] Running data-preparation checks...\n")

  validate_unique_column(order_level_df, "order_id", "order-level dataset")
  validate_unique_composite_key(
    seller_order_df,
    c("seller_id", "order_id"),
    "seller-order dataset"
  )
  validate_row_count_unchanged(
    original_order_count,
    nrow(order_level_df),
    "order-level table construction"
  )

  validate_total_preserved(
    original_payment_total,
    sum(order_level_df$total_payment_value, na.rm = TRUE),
    "Payment value",
    tolerance
  )
  validate_total_preserved(
    original_item_price_total,
    sum(order_level_df$total_item_price, na.rm = TRUE),
    "Item price",
    tolerance
  )
  validate_total_preserved(
    original_freight_total,
    sum(order_level_df$total_freight_value, na.rm = TRUE),
    "Freight value",
    tolerance
  )

  duration_results <- validate_no_negative_durations(order_level_df)

  single_seller_check <- sum(
    order_level_df$seller_count == 1 & order_level_df$single_seller_order == 1,
    na.rm = TRUE
  )
  if (single_seller_check == 0 && sum(order_level_df$seller_count == 1, na.rm = TRUE) > 0) {
    stop("[VALIDATION ERROR] single_seller_order flag is inconsistent with seller_count.")
  }

  message(sprintf(
    "[VALIDATION PASS] single_seller_order aligned for %d single-seller orders.",
    single_seller_check
  ))

  cat("[VALIDATION] All data-preparation checks passed.\n")

  return(list(
    order_rows = nrow(order_level_df),
    order_cols = ncol(order_level_df),
    seller_order_rows = nrow(seller_order_df),
    seller_order_cols = ncol(seller_order_df),
    duration_checks = duration_results,
    single_seller_orders = single_seller_check
  ))
}
