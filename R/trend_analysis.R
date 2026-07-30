# trend_analysis.R
# Purpose: Monthly completed-order volume and payment trend analysis.

source("R/validation_helpers.R")

REQUIRED_VARS <- c(
  "completed_order",
  "purchase_date",
  "purchase_month",
  "total_payment_value"
)

validate_trend_inputs <- function(df) {
  for (var in REQUIRED_VARS) {
    validate_variable_exists(df, var)
  }

  validate_variable_type(df, "total_payment_value", "numeric")
  validate_required_date(df, "purchase_date")

  valid_months <- df$purchase_month[!is.na(df$purchase_month) & df$purchase_month != ""]
  if (length(unique(valid_months)) < 3) {
    stop("[VALIDATION ERROR] Fewer than 3 distinct purchase months available for trend analysis.")
  }

  TRUE
}

prepare_trend_population <- function(df) {
  exclusion_counts <- list(
    starting_rows = nrow(df),
    not_completed = sum(df$completed_order != 1, na.rm = TRUE),
    missing_purchase_date = sum(is.na(df$purchase_date)),
    missing_purchase_month = sum(is.na(df$purchase_month) | df$purchase_month == ""),
    missing_payment_value = sum(is.na(df$total_payment_value)),
    negative_payment_value = sum(!is.na(df$total_payment_value) & df$total_payment_value < 0)
  )

  keep <- df$completed_order == 1 &
    !is.na(df$purchase_date) &
    !is.na(df$purchase_month) &
    df$purchase_month != "" &
    !is.na(df$total_payment_value) &
    df$total_payment_value >= 0

  list(
    data = df[keep, , drop = FALSE],
    exclusion_counts = exclusion_counts,
    rows_analyzed = sum(keep)
  )
}

aggregate_monthly_trends <- function(df) {
  month_split <- split(df, df$purchase_month)

  trend_df <- do.call(rbind, lapply(names(month_split), function(month_label) {
    month_df <- month_split[[month_label]]
    data.frame(
      purchase_month = month_label,
      completed_order_count = nrow(month_df),
      total_payment_value = sum(month_df$total_payment_value, na.rm = TRUE),
      average_payment_value = mean(month_df$total_payment_value, na.rm = TRUE),
      median_payment_value = median(month_df$total_payment_value, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))

  trend_df <- trend_df[order(trend_df$purchase_month), ]
  rownames(trend_df) <- NULL

  if (any(duplicated(trend_df$purchase_month))) {
    stop("[VALIDATION ERROR] Duplicate purchase_month rows detected after aggregation.")
  }

  trend_df
}

add_mom_changes <- function(trend_df) {
  n_months <- nrow(trend_df)
  trend_df$order_count_mom_pct <- NA_real_
  trend_df$payment_mom_pct <- NA_real_

  if (n_months > 1) {
    for (i in 2:n_months) {
      prev_count <- trend_df$completed_order_count[i - 1]
      prev_payment <- trend_df$total_payment_value[i - 1]

      if (prev_count > 0) {
        trend_df$order_count_mom_pct[i] <-
          ((trend_df$completed_order_count[i] - prev_count) / prev_count) * 100
      }

      if (prev_payment > 0) {
        trend_df$payment_mom_pct[i] <-
          ((trend_df$total_payment_value[i] - prev_payment) / prev_payment) * 100
      }
    }
  }

  trend_df
}

identify_incomplete_boundary_months <- function(trend_df) {
  n <- nrow(trend_df)
  if (n < 3) {
    return(list(
      incomplete_months = character(0),
      low_volume_months = character(0),
      reasons = character(0)
    ))
  }

  incomplete <- character(0)
  reasons <- character(0)

  interior_counts <- trend_df$completed_order_count[2:(n - 1)]
  interior_median <- median(interior_counts, na.rm = TRUE)

  first_month <- trend_df$purchase_month[1]
  first_count <- trend_df$completed_order_count[1]
  second_count <- trend_df$completed_order_count[2]

  if (first_count < 0.5 * second_count || first_count < 0.05 * interior_median) {
    incomplete <- c(incomplete, first_month)
    reasons <- c(
      reasons,
      sprintf(
        "%s excluded: first month volume (%d orders) is far below the second month (%d) and interior median (%d), indicating a partial tracking window.",
        first_month, first_count, second_count, interior_median
      )
    )
  }

  last_month <- trend_df$purchase_month[n]
  last_count <- trend_df$completed_order_count[n]
  penultimate_count <- trend_df$completed_order_count[n - 1]

  if (last_count < 0.5 * penultimate_count || last_count < 0.05 * interior_median) {
    incomplete <- c(incomplete, last_month)
    reasons <- c(
      reasons,
      sprintf(
        "%s excluded: last month volume (%d orders) is far below the prior month (%d) and interior median (%d), indicating a partial tracking window.",
        last_month, last_count, penultimate_count, interior_median
      )
    )
  }

  low_volume <- trend_df$purchase_month[
    trend_df$completed_order_count < 0.05 * interior_median &
      !trend_df$purchase_month %in% incomplete
  ]

  if (length(low_volume) > 0) {
    for (month_label in low_volume) {
      reasons <- c(
        reasons,
        sprintf(
          "%s excluded from peak/low and MoM comparisons: monthly volume (%d orders) is below 5%% of the interior median (%d).",
          month_label,
          trend_df$completed_order_count[trend_df$purchase_month == month_label],
          interior_median
        )
      )
    }
  }

  list(
    incomplete_months = incomplete,
    low_volume_months = low_volume,
    excluded_comparison_months = unique(c(incomplete, low_volume)),
    reasons = reasons
  )
}

compute_key_results <- function(trend_df, excluded_comparison_months) {
  trend_df$is_incomplete_boundary <- trend_df$purchase_month %in% excluded_comparison_months
  core_trend <- trend_df[!trend_df$is_incomplete_boundary, , drop = FALSE]
  core_median_count <- median(core_trend$completed_order_count, na.rm = TRUE)
  min_valid_count <- 0.05 * core_median_count

  peak_count_idx <- which.max(core_trend$completed_order_count)
  lowest_count_idx <- which.min(core_trend$completed_order_count)
  peak_payment_idx <- which.max(core_trend$total_payment_value)

  trend_df$prev_month_count <- c(NA, trend_df$completed_order_count[-nrow(trend_df)])
  mom_valid <- !is.na(trend_df$order_count_mom_pct) &
    !trend_df$is_incomplete_boundary &
    trend_df$completed_order_count >= min_valid_count &
    trend_df$prev_month_count >= min_valid_count

  mom_core <- trend_df[mom_valid, , drop = FALSE]

  largest_order_increase <- mom_core[which.max(mom_core$order_count_mom_pct), , drop = FALSE]
  largest_order_decrease <- mom_core[which.min(mom_core$order_count_mom_pct), , drop = FALSE]
  largest_payment_increase <- mom_core[which.max(mom_core$payment_mom_pct), , drop = FALSE]
  largest_payment_decrease <- mom_core[which.min(mom_core$payment_mom_pct), , drop = FALSE]

  list(
    full_trends = trend_df[, !names(trend_df) %in% "prev_month_count", drop = FALSE],
    core_trends = core_trend,
    core_median_count = core_median_count,
    min_valid_count_for_mom = min_valid_count,
    peak_month_count = core_trend$purchase_month[peak_count_idx],
    peak_count_val = core_trend$completed_order_count[peak_count_idx],
    lowest_month_count = core_trend$purchase_month[lowest_count_idx],
    lowest_count_val = core_trend$completed_order_count[lowest_count_idx],
    peak_month_payment = core_trend$purchase_month[peak_payment_idx],
    peak_payment_val = core_trend$total_payment_value[peak_payment_idx],
    largest_order_increase_month = largest_order_increase$purchase_month,
    largest_order_increase_pct = largest_order_increase$order_count_mom_pct,
    largest_order_decrease_month = largest_order_decrease$purchase_month,
    largest_order_decrease_pct = largest_order_decrease$order_count_mom_pct,
    largest_payment_increase_month = largest_payment_increase$purchase_month,
    largest_payment_increase_pct = largest_payment_increase$payment_mom_pct,
    largest_payment_decrease_month = largest_payment_decrease$purchase_month,
    largest_payment_decrease_pct = largest_payment_decrease$payment_mom_pct
  )
}

plot_monthly_trends <- function(trend_df, plot_dir) {
  x_idx <- seq_len(nrow(trend_df))
  incomplete_idx <- which(trend_df$is_incomplete_boundary)

  png(file.path(plot_dir, "monthly_order_trend.png"), width = 900, height = 500)
  par(mar = c(8, 4, 4, 2) + 0.1)
  plot(
    x_idx,
    trend_df$completed_order_count,
    type = "o",
    pch = 16,
    col = "#3b82f6",
    lwd = 2,
    main = "Monthly Completed Order Volume",
    xlab = "",
    ylab = "Completed Orders",
    xaxt = "n"
  )
  axis(1, at = x_idx, labels = trend_df$purchase_month, las = 2, cex.axis = 0.8)
  if (length(incomplete_idx) > 0) {
    points(
      x_idx[incomplete_idx],
      trend_df$completed_order_count[incomplete_idx],
      col = "#ef4444",
      pch = 4,
      cex = 2,
      lwd = 2
    )
    legend(
      "topleft",
      legend = c("Complete month", "Incomplete boundary month"),
      col = c("#3b82f6", "#ef4444"),
      pch = c(16, 4),
      bty = "n"
    )
  }
  dev.off()

  png(file.path(plot_dir, "monthly_payment_trend.png"), width = 900, height = 500)
  par(mar = c(8, 4, 4, 2) + 0.1)
  plot(
    x_idx,
    trend_df$total_payment_value / 1000,
    type = "o",
    pch = 16,
    col = "#10b981",
    lwd = 2,
    main = "Monthly Total Payment Value (R$ Thousands)",
    xlab = "",
    ylab = "Total Payment Value (R$ K)",
    xaxt = "n"
  )
  axis(1, at = x_idx, labels = trend_df$purchase_month, las = 2, cex.axis = 0.8)
  if (length(incomplete_idx) > 0) {
    points(
      x_idx[incomplete_idx],
      trend_df$total_payment_value[incomplete_idx] / 1000,
      col = "#ef4444",
      pch = 4,
      cex = 2,
      lwd = 2
    )
  }
  dev.off()
}

run_trend_analysis <- function(
    data_path = "data/processed/order_level_analysis.rds",
    output_dir = "output/trend",
    plot_dir = "output/trend") {
  cat("[TREND] Loading processed order-level dataset...\n")

  if (!file.exists(data_path)) {
    stop(sprintf("Processed dataset not found at %s. Run clean_data.R first.", data_path))
  }

  df <- readRDS(data_path)
  validate_trend_inputs(df)

  warnings_list <- list()
  negative_payments <- sum(!is.na(df$total_payment_value) & df$total_payment_value < 0, na.rm = TRUE)
  if (negative_payments > 0) {
    msg <- sprintf(
      "[TREND WARNING] %d orders have negative total_payment_value and were excluded from analysis.",
      negative_payments
    )
    warning(msg, call. = FALSE)
    warnings_list[[length(warnings_list) + 1]] <- msg
  }

  prep <- prepare_trend_population(df)
  analysis_df <- prep$data

  if (nrow(analysis_df) < 10) {
    stop("[VALIDATION ERROR] Fewer than 10 completed orders remain after filtering.")
  }

  trend_df <- aggregate_monthly_trends(analysis_df)
  trend_df <- add_mom_changes(trend_df)

  boundary_info <- identify_incomplete_boundary_months(trend_df)
  results <- compute_key_results(trend_df, boundary_info$excluded_comparison_months)

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

  write.csv(results$full_trends, file.path(output_dir, "monthly_trend_summary.csv"), row.names = FALSE)

  key_results <- data.frame(
    metric = c(
      "date_range_start",
      "date_range_end",
      "months_analyzed",
      "incomplete_boundary_months",
      "low_volume_excluded_months",
      "excluded_comparison_months",
      "completed_orders_analyzed",
      "total_payment_value_analyzed",
      "peak_complete_month_order_count",
      "peak_complete_order_count",
      "lowest_complete_month_order_count",
      "lowest_complete_order_count",
      "peak_complete_month_total_payment",
      "peak_complete_total_payment",
      "largest_order_mom_increase_month",
      "largest_order_mom_increase_pct",
      "largest_order_mom_decrease_month",
      "largest_order_mom_decrease_pct",
      "largest_payment_mom_increase_month",
      "largest_payment_mom_increase_pct",
      "largest_payment_mom_decrease_month",
      "largest_payment_mom_decrease_pct"
    ),
    value = c(
      results$full_trends$purchase_month[1],
      results$full_trends$purchase_month[nrow(results$full_trends)],
      nrow(results$full_trends),
      paste(boundary_info$incomplete_months, collapse = "; "),
      paste(boundary_info$low_volume_months, collapse = "; "),
      paste(boundary_info$excluded_comparison_months, collapse = "; "),
      prep$rows_analyzed,
      sum(analysis_df$total_payment_value, na.rm = TRUE),
      results$peak_month_count,
      results$peak_count_val,
      results$lowest_month_count,
      results$lowest_count_val,
      results$peak_month_payment,
      results$peak_payment_val,
      results$largest_order_increase_month,
      results$largest_order_increase_pct,
      results$largest_order_decrease_month,
      results$largest_order_decrease_pct,
      results$largest_payment_increase_month,
      results$largest_payment_increase_pct,
      results$largest_payment_decrease_month,
      results$largest_payment_decrease_pct
    ),
    stringsAsFactors = FALSE
  )

  write.csv(key_results, file.path(output_dir, "trend_key_results.csv"), row.names = FALSE)
  write.csv(
    data.frame(reason = boundary_info$reasons, stringsAsFactors = FALSE),
    file.path(output_dir, "incomplete_month_notes.csv"),
    row.names = FALSE
  )
  write.csv(
    data.frame(
      metric = names(prep$exclusion_counts),
      count = unname(unlist(prep$exclusion_counts)),
      stringsAsFactors = FALSE
    ),
    file.path(output_dir, "trend_exclusion_counts.csv"),
    row.names = FALSE
  )

  plot_monthly_trends(results$full_trends, plot_dir)

  total_payment <- sum(analysis_df$total_payment_value, na.rm = TRUE)

  cat(sprintf("[TREND] Completed orders analyzed: %d\n", prep$rows_analyzed))
  cat(sprintf("[TREND] Date range: %s to %s\n",
              results$full_trends$purchase_month[1],
              results$full_trends$purchase_month[nrow(results$full_trends)]))
  cat(sprintf("[TREND] Incomplete boundary months: %s\n",
              paste(boundary_info$incomplete_months, collapse = ", ")))
  if (length(boundary_info$low_volume_months) > 0) {
    cat(sprintf("[TREND] Low-volume months excluded from comparisons: %s\n",
                paste(boundary_info$low_volume_months, collapse = ", ")))
  }
  cat(sprintf("[TREND] Peak complete month (orders): %s (%d)\n",
              results$peak_month_count, results$peak_count_val))
  cat(sprintf("[TREND] Lowest complete month (orders): %s (%d)\n",
              results$lowest_month_count, results$lowest_count_val))
  cat(sprintf("[TREND] Peak complete month (payment): %s (R$ %.2f)\n",
              results$peak_month_payment, results$peak_payment_val))

  list(
    full_trends = results$full_trends,
    core_trends = results$core_trends,
    core_trends_compat = transform(results$core_trends, month = purchase_month),
    full_trends_compat = transform(results$full_trends, month = purchase_month),
    date_range = c(
      results$full_trends$purchase_month[1],
      results$full_trends$purchase_month[nrow(results$full_trends)]
    ),
    incomplete_boundary_months = boundary_info$incomplete_months,
    low_volume_excluded_months = boundary_info$low_volume_months,
    excluded_comparison_months = boundary_info$excluded_comparison_months,
    incomplete_boundary_reasons = boundary_info$reasons,
    completed_orders_analyzed = prep$rows_analyzed,
    total_payment_value_analyzed = total_payment,
    peak_month_count = results$peak_month_count,
    peak_count_val = results$peak_count_val,
    lowest_month_count = results$lowest_month_count,
    lowest_count_val = results$lowest_count_val,
    peak_month_payment = results$peak_month_payment,
    peak_payment_val = results$peak_payment_val,
    largest_order_increase_month = results$largest_order_increase_month,
    largest_order_increase_pct = results$largest_order_increase_pct,
    largest_order_decrease_month = results$largest_order_decrease_month,
    largest_order_decrease_pct = results$largest_order_decrease_pct,
    largest_payment_increase_month = results$largest_payment_increase_month,
    largest_payment_increase_pct = results$largest_payment_increase_pct,
    largest_payment_decrease_month = results$largest_payment_decrease_month,
    largest_payment_decrease_pct = results$largest_payment_decrease_pct,
    exclusion_counts = prep$exclusion_counts,
    warnings = warnings_list,
    plot_paths = c(
      file.path(plot_dir, "monthly_order_trend.png"),
      file.path(plot_dir, "monthly_payment_trend.png")
    ),
    trend_plot_path = file.path(plot_dir, "monthly_order_trend.png")
  )
}

if (sys.nframe() == 0L) {
  invisible(run_trend_analysis())
}
