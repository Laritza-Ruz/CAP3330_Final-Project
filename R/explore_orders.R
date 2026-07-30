# explore_orders.R
# Purpose: Descriptive analysis of order value, freight, delivery time, status, and lateness.

source("R/validation_helpers.R")

PRIMARY_VARS <- c(
  "total_payment_value", "total_freight_value", "actual_delivery_days",
  "delivery_delay_days", "late_delivery", "completed_order",
  "canceled_or_unavailable", "order_status", "seller_count", "single_seller_order"
)

warn_if_negative <- function(x, var_name, warnings_list) {
  n_neg <- sum(!is.na(x) & x < 0)
  if (n_neg > 0) {
    msg <- sprintf(
      "[EXPLORE WARNING] '%s' contains %d negative values. Statistics include all non-missing rows unless noted.",
      var_name, n_neg
    )
    warning(msg, call. = FALSE)
    warnings_list[[length(warnings_list) + 1]] <- msg
  }
  warnings_list
}

validate_explore_inputs <- function(df) {
  required <- c(
    "total_payment_value", "total_freight_value", "actual_delivery_days",
    "late_delivery", "completed_order", "canceled_or_unavailable", "order_status"
  )
  for (var in required) {
    validate_variable_exists(df, var)
  }

  for (var in c("total_payment_value", "total_freight_value", "actual_delivery_days")) {
    validate_variable_type(df, var, "numeric")
  }

  if (sum(!is.na(df$total_payment_value)) < 10) {
    stop("[VALIDATION ERROR] Fewer than 10 non-missing payment values. Explore analysis cannot proceed.")
  }

  delivered <- df$completed_order == 1 & !is.na(df$actual_delivery_days)
  if (sum(delivered) < 10) {
    stop("[VALIDATION ERROR] Fewer than 10 delivered orders with valid delivery dates.")
  }

  if (sum(!is.na(df$purchase_date)) == 0) {
    stop("[VALIDATION ERROR] No valid purchase dates available for date-dependent summaries.")
  }

  return(TRUE)
}

run_explore_orders <- function(
    data_path = "data/processed/order_level_analysis.rds",
    output_dir = "output",
    plot_dir = "images") {
  cat("[EXPLORE] Loading processed order-level dataset...\n")

  if (!file.exists(data_path)) {
    stop(sprintf("Processed dataset not found at %s. Run clean_data.R first.", data_path))
  }

  df <- readRDS(data_path)
  validate_explore_inputs(df)

  warnings_list <- list()
  for (var in c("total_payment_value", "total_freight_value", "actual_delivery_days")) {
    warnings_list <- warn_if_negative(df[[var]], var, warnings_list)
  }

  total_orders <- nrow(df)
  completed_orders <- sum(df$completed_order == 1, na.rm = TRUE)
  canceled_or_unavailable_orders <- sum(df$canceled_or_unavailable == 1, na.rm = TRUE)
  missing_delivery_dates <- sum(
    is.na(df$actual_delivery_date) |
      is.na(df$order_delivered_customer_date) |
      df$order_delivered_customer_date == ""
  )

  delivered_sub <- df[df$completed_order == 1 & !is.na(df$actual_delivery_days), ]
  late_count <- sum(delivered_sub$late_delivery == 1, na.rm = TRUE)
  late_rate <- if (nrow(delivered_sub) > 0) late_count / nrow(delivered_sub) else NA_real_

  payment_summary <- c(
    mean = mean(df$total_payment_value, na.rm = TRUE),
    sd = sd(df$total_payment_value, na.rm = TRUE),
    min = min(df$total_payment_value, na.rm = TRUE),
    q1 = unname(quantile(df$total_payment_value, 0.25, na.rm = TRUE)),
    median = median(df$total_payment_value, na.rm = TRUE),
    q3 = unname(quantile(df$total_payment_value, 0.75, na.rm = TRUE)),
    max = max(df$total_payment_value, na.rm = TRUE)
  )

  freight_summary <- c(
    mean = mean(df$total_freight_value, na.rm = TRUE),
    median = median(df$total_freight_value, na.rm = TRUE)
  )

  delivery_summary <- c(
    mean = mean(delivered_sub$actual_delivery_days, na.rm = TRUE),
    median = median(delivered_sub$actual_delivery_days, na.rm = TRUE)
  )

  missing_counts <- sapply(PRIMARY_VARS, function(var) sum(is.na(df[[var]])))
  unique_counts <- c(
    order_status = length(unique(df$order_status)),
    order_id = length(unique(df$order_id)),
    seller_count = length(unique(df$seller_count[!is.na(df$seller_count)]))
  )

  status_table <- as.data.frame(sort(table(df$order_status), decreasing = TRUE))
  names(status_table) <- c("order_status", "count")
  status_table$pct <- round(100 * status_table$count / total_orders, 2)

  summary_table <- data.frame(
    metric = c(
      "Total orders",
      "Delivered/completed orders",
      "Delivered/completed (%)",
      "Canceled or unavailable orders",
      "Canceled or unavailable (%)",
      "Orders missing delivery dates",
      "Mean total payment value",
      "Median total payment value",
      "SD total payment value",
      "Min payment value",
      "Q1 payment value",
      "Q3 payment value",
      "Max payment value",
      "Mean total freight value",
      "Median total freight value",
      "Mean actual delivery days (delivered)",
      "Median actual delivery days (delivered)",
      "Late deliveries (delivered orders)",
      "Late delivery rate (delivered orders)"
    ),
    value = c(
      total_orders,
      completed_orders,
      round(100 * completed_orders / total_orders, 2),
      canceled_or_unavailable_orders,
      round(100 * canceled_or_unavailable_orders / total_orders, 2),
      missing_delivery_dates,
      round(payment_summary["mean"], 2),
      round(payment_summary["median"], 2),
      round(payment_summary["sd"], 2),
      round(payment_summary["min"], 2),
      round(payment_summary["q1"], 2),
      round(payment_summary["q3"], 2),
      round(payment_summary["max"], 2),
      round(freight_summary["mean"], 2),
      round(freight_summary["median"], 2),
      round(delivery_summary["mean"], 2),
      round(delivery_summary["median"], 2),
      late_count,
      round(100 * late_rate, 2)
    ),
    stringsAsFactors = FALSE
  )

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

  write.csv(summary_table, file.path(output_dir, "explore_summary_table.csv"), row.names = FALSE)
  write.csv(status_table, file.path(output_dir, "explore_order_status_table.csv"), row.names = FALSE)
  write.csv(
    data.frame(variable = names(missing_counts), missing_count = as.integer(missing_counts)),
    file.path(output_dir, "explore_missing_counts.csv"),
    row.names = FALSE
  )

  payment_cap <- 500
  payment_plot_vals <- df$total_payment_value[
    !is.na(df$total_payment_value) & df$total_payment_value <= payment_cap
  ]

  png(file.path(plot_dir, "order_value_distribution.png"), width = 800, height = 500)
  par(mar = c(5, 4, 4, 2) + 0.1)
  hist(
    payment_plot_vals,
    breaks = 50,
    col = "#3b82f6",
    border = "white",
    main = "Distribution of Order Payment Values\n(Display capped at R$ 500; statistics use all orders)",
    xlab = "Total Payment Value (R$)",
    ylab = "Frequency"
  )
  abline(v = payment_summary["median"], col = "#ef4444", lwd = 2, lty = 2)
  legend(
    "topright",
    legend = sprintf("Median (all orders): R$ %.2f", payment_summary["median"]),
    bty = "n",
    text.col = "#ef4444"
  )
  dev.off()

  late_bar <- c(
    OnTime = sum(delivered_sub$late_delivery == 0, na.rm = TRUE),
    Late = sum(delivered_sub$late_delivery == 1, na.rm = TRUE),
    Missing = sum(is.na(delivered_sub$late_delivery))
  )

  png(file.path(plot_dir, "late_delivery_distribution.png"), width = 800, height = 500)
  par(mar = c(5, 4, 4, 2) + 0.1)
  barplot(
    late_bar[c("OnTime", "Late")],
    col = c("#10b981", "#ef4444"),
    border = "white",
    main = "Late vs. On-Time Deliveries\n(Delivered orders with valid delivery dates)",
    ylab = "Number of Orders",
    names.arg = c("On-Time", "Late")
  )
  dev.off()

  delivery_plot_cap <- 60
  delivery_plot_vals <- delivered_sub$actual_delivery_days[
    !is.na(delivered_sub$actual_delivery_days) &
      delivered_sub$actual_delivery_days <= delivery_plot_cap
  ]

  png(file.path(plot_dir, "delivery_days_distribution.png"), width = 800, height = 500)
  par(mar = c(5, 4, 4, 2) + 0.1)
  hist(
    delivery_plot_vals,
    breaks = 40,
    col = "#6366f1",
    border = "white",
    main = "Distribution of Actual Delivery Days\n(Display capped at 60 days; statistics use all delivered orders)",
    xlab = "Actual Delivery Days",
    ylab = "Frequency"
  )
  abline(v = delivery_summary["median"], col = "#ef4444", lwd = 2, lty = 2)
  dev.off()

  cat(sprintf("[EXPLORE] Total orders: %d\n", total_orders))
  cat(sprintf("[EXPLORE] Completed orders: %d (%.2f%%)\n", completed_orders, 100 * completed_orders / total_orders))
  cat(sprintf("[EXPLORE] Late delivery rate (delivered): %.2f%%\n", 100 * late_rate))

  list(
    total_orders = total_orders,
    completed_orders = completed_orders,
    completed_pct = 100 * completed_orders / total_orders,
    canceled_or_unavailable_orders = canceled_or_unavailable_orders,
    canceled_or_unavailable_pct = 100 * canceled_or_unavailable_orders / total_orders,
    missing_delivery_dates = missing_delivery_dates,
    mean_payment = payment_summary["mean"],
    median_payment = payment_summary["median"],
    sd_payment = payment_summary["sd"],
    payment_fivenum = payment_summary,
    mean_freight = freight_summary["mean"],
    median_freight = freight_summary["median"],
    mean_delivery_time = delivery_summary["mean"],
    median_delivery_time = delivery_summary["median"],
    late_delivery_count = late_count,
    late_delivery_rate = late_rate,
    missing_counts = missing_counts,
    unique_counts = unique_counts,
    status_table = status_table,
    summary_table = summary_table,
    warnings = warnings_list,
    plot_paths = c(
      file.path(plot_dir, "order_value_distribution.png"),
      file.path(plot_dir, "late_delivery_distribution.png"),
      file.path(plot_dir, "delivery_days_distribution.png")
    )
  )
}

if (sys.nframe() == 0L) {
  invisible(run_explore_orders())
}
