# data_quality_audit.R
# Purpose: Programmatic audit of processed and raw Olist datasets for analysis reliability.

source("R/validation_helpers.R")

PAYMENT_TOLERANCE <- 0.05

pct_of <- function(count, total) {
  if (total <= 0) {
    return(0)
  }
  round(100 * count / total, 4)
}

iqr_outlier_count <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 4) {
    return(list(count = 0L, cutoff = NA_real_))
  }
  q1 <- as.numeric(quantile(x, 0.25, na.rm = TRUE))
  q3 <- as.numeric(quantile(x, 0.75, na.rm = TRUE))
  iqr <- q3 - q1
  cutoff <- q3 + 1.5 * iqr
  list(count = as.integer(sum(x > cutoff)), cutoff = cutoff)
}

make_issue <- function(name, count, total, severity, interpretation, recommended_action) {
  data.frame(
    issue_name = name,
    issue_count = as.integer(count),
    issue_percentage = pct_of(count, total),
    severity = severity,
    interpretation = interpretation,
    recommended_action = recommended_action,
    stringsAsFactors = FALSE
  )
}

validate_audit_outputs <- function(order_df, seller_df, issue_summary) {
  if (any(duplicated(order_df$order_id))) {
    stop("[VALIDATION ERROR] order_id is not unique in processed order-level dataset.")
  }

  seller_key <- paste(seller_df$seller_id, seller_df$order_id, sep = "|")
  if (any(duplicated(seller_key))) {
    stop("[VALIDATION ERROR] seller_id + order_id is not unique in processed seller-order dataset.")
  }

  max_order_rows <- nrow(order_df)
  order_level_issues <- issue_summary[
    grepl(
      "order|payment_difference|missing_purchase|missing_estimated|missing_actual|delivery|approval|seller_count|multi_seller|zero_payment",
      issue_summary$issue_name
    ) &
      !grepl("seller_order_rows|raw|items|payments|missing_months", issue_summary$issue_name),
  ]
  if (any(order_level_issues$issue_count > max_order_rows, na.rm = TRUE)) {
    stop("[VALIDATION ERROR] An order-level issue count exceeds dataset size.")
  }

  if (any(!is.finite(issue_summary$issue_percentage))) {
    stop("[VALIDATION ERROR] Issue summary contains non-finite percentages.")
  }

  TRUE
}

run_data_quality_audit <- function(
    raw_dir = "data/raw",
    order_path = "data/processed/order_level_analysis.rds",
    seller_path = "data/processed/seller_order_analysis.rds",
    processed_path = NULL,
    output_dir = "output/data_quality") {
  if (!is.null(processed_path)) {
    order_path <- processed_path
  }
  cat("[AUDIT] Starting data quality audit...\n")

  if (!file.exists(order_path)) {
    stop(sprintf("Order-level dataset not found at %s.", order_path))
  }
  if (!file.exists(seller_path)) {
    stop(sprintf("Seller-order dataset not found at %s.", seller_path))
  }

  order_df <- readRDS(order_path)
  seller_df <- readRDS(seller_path)

  to_posix <- function(x) {
    if (inherits(x, "POSIXct")) {
      return(x)
    }
    if (inherits(x, "Date")) {
      return(as.POSIXct(x))
    }
    x <- as.character(x)
    x[x == ""] <- NA
    out <- suppressWarnings(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    need_alt <- is.na(out) & !is.na(x)
    if (any(need_alt)) {
      out[need_alt] <- suppressWarnings(as.POSIXct(x[need_alt], format = "%Y-%m-%d", tz = "UTC"))
    }
    out
  }

  orders_raw <- read.csv(file.path(raw_dir, "olist_orders_dataset.csv"), stringsAsFactors = FALSE)
  items_raw <- read.csv(file.path(raw_dir, "olist_order_items_dataset.csv"), stringsAsFactors = FALSE)
  payments_raw <- read.csv(file.path(raw_dir, "olist_order_payments_dataset.csv"), stringsAsFactors = FALSE)
  customers_raw <- read.csv(file.path(raw_dir, "olist_customers_dataset.csv"), stringsAsFactors = FALSE)
  sellers_raw <- read.csv(file.path(raw_dir, "olist_sellers_dataset.csv"), stringsAsFactors = FALSE)

  n_orders <- nrow(order_df)
  n_seller_rows <- nrow(seller_df)

  order_df$purchase_ts <- to_posix(order_df$order_purchase_timestamp)
  order_df$approved_ts <- to_posix(order_df$order_approved_at)
  order_df$delivered_ts <- to_posix(order_df$order_delivered_customer_date)
  order_df$estimated_ts <- to_posix(order_df$order_estimated_delivery_date)
  order_df$payment_difference <- order_df$total_payment_value - order_df$total_item_price - order_df$total_freight_value

  duplicate_order_id <- sum(duplicated(order_df$order_id))
  duplicate_seller_order <- sum(duplicated(paste(seller_df$seller_id, seller_df$order_id, sep = "|")))

  count_missing <- function(x) {
    if (is.numeric(x) || is.logical(x) || inherits(x, c("Date", "POSIXct"))) {
      return(sum(is.na(x)))
    }
    sum(is.na(x) | x == "", na.rm = TRUE)
  }

  missing_value_summary <- data.frame(
    dataset = rep("order_level_analysis", length(names(order_df))),
    variable = names(order_df),
    missing_count = sapply(order_df, count_missing),
    missing_percentage = sapply(order_df, function(x) pct_of(count_missing(x), n_orders)),
    stringsAsFactors = FALSE
  )

  duplicate_key_summary <- data.frame(
    dataset = c("order_level_analysis", "seller_order_analysis", "raw_orders", "raw_customers", "raw_sellers"),
    key = c("order_id", "seller_id + order_id", "order_id", "customer_id", "seller_id"),
    duplicate_count = c(
      duplicate_order_id,
      duplicate_seller_order,
      sum(duplicated(orders_raw$order_id)),
      sum(duplicated(customers_raw$customer_id)),
      sum(duplicated(sellers_raw$seller_id))
    ),
    stringsAsFactors = FALSE
  )

  missing_customer <- sum(!order_df$customer_id %in% customers_raw$customer_id)
  missing_seller_items <- sum(!items_raw$seller_id %in% sellers_raw$seller_id)
  orders_without_items <- sum(!order_df$order_id %in% items_raw$order_id)
  orders_without_payments <- sum(!order_df$order_id %in% payments_raw$order_id)

  relationship_summary <- data.frame(
    check_name = c(
      "missing_customer_relationship",
      "missing_seller_relationship_in_items",
      "orders_without_item_records",
      "orders_without_payment_records",
      "multi_seller_orders",
      "multi_seller_order_percentage"
    ),
    count = c(
      missing_customer,
      missing_seller_items,
      orders_without_items,
      orders_without_payments,
      sum(order_df$seller_count > 1, na.rm = TRUE),
      pct_of(sum(order_df$seller_count > 1, na.rm = TRUE), n_orders)
    ),
    stringsAsFactors = FALSE
  )

  date_consistency_summary <- data.frame(
    check_name = c(
      "missing_purchase_dates",
      "missing_estimated_delivery_dates",
      "missing_actual_delivery_dates",
      "actual_delivery_before_purchase",
      "estimated_delivery_before_purchase",
      "approval_before_purchase",
      "delivered_status_without_actual_delivery_date",
      "canceled_or_unavailable_with_actual_delivery_date"
    ),
    count = c(
      sum(is.na(order_df$purchase_date)),
      sum(is.na(order_df$estimated_delivery_date) | is.na(order_df$order_estimated_delivery_date)),
      sum(is.na(order_df$actual_delivery_date) | is.na(order_df$delivered_ts)),
      sum(order_df$delivered_ts < order_df$purchase_ts, na.rm = TRUE),
      sum(order_df$estimated_ts < order_df$purchase_ts, na.rm = TRUE),
      sum(order_df$approved_ts < order_df$purchase_ts, na.rm = TRUE),
      sum(order_df$order_status == "delivered" & (is.na(order_df$actual_delivery_date) | is.na(order_df$delivered_ts))),
      sum(order_df$canceled_or_unavailable == 1 & !is.na(order_df$actual_delivery_date))
    ),
    stringsAsFactors = FALSE
  )
  date_consistency_summary$percentage <- pct_of(date_consistency_summary$count, n_orders)

  monetary_quality_summary <- data.frame(
    check_name = c(
      "negative_item_prices_raw",
      "negative_freight_values_raw",
      "negative_payment_values_raw",
      "zero_payment_values_processed",
      "payment_difference_exceeds_tolerance",
      "missing_or_invalid_seller_count"
    ),
    count = c(
      sum(items_raw$price < 0, na.rm = TRUE),
      sum(items_raw$freight_value < 0, na.rm = TRUE),
      sum(payments_raw$payment_value < 0, na.rm = TRUE),
      sum(order_df$total_payment_value == 0, na.rm = TRUE),
      sum(abs(order_df$payment_difference) > PAYMENT_TOLERANCE, na.rm = TRUE),
      sum(is.na(order_df$seller_count) | order_df$seller_count < 1)
    ),
    stringsAsFactors = FALSE
  )
  monetary_quality_summary$percentage <- pct_of(monetary_quality_summary$count, n_orders)
  monetary_quality_summary$tolerance <- c(NA, NA, NA, NA, PAYMENT_TOLERANCE, NA)

  payment_outliers <- iqr_outlier_count(order_df$total_payment_value[order_df$total_payment_value > 0])
  freight_outliers <- iqr_outlier_count(order_df$total_freight_value[order_df$total_freight_value > 0])
  delivery_outliers <- iqr_outlier_count(
    order_df$actual_delivery_days[order_df$completed_order == 1 & !is.na(order_df$actual_delivery_days)]
  )

  outlier_summary <- data.frame(
    metric = c("total_payment_value", "total_freight_value", "actual_delivery_days"),
    outlier_count = c(payment_outliers$count, freight_outliers$count, delivery_outliers$count),
    upper_cutoff = c(payment_outliers$cutoff, freight_outliers$cutoff, delivery_outliers$cutoff),
    stringsAsFactors = FALSE
  )
  outlier_summary$percentage_of_relevant_rows <- c(
    pct_of(payment_outliers$count, sum(order_df$total_payment_value > 0, na.rm = TRUE)),
    pct_of(freight_outliers$count, sum(order_df$total_freight_value > 0, na.rm = TRUE)),
    pct_of(
      delivery_outliers$count,
      sum(order_df$completed_order == 1 & !is.na(order_df$actual_delivery_days), na.rm = TRUE)
    )
  )

  completed_months <- sort(unique(order_df$purchase_month[order_df$completed_order == 1 & !is.na(order_df$purchase_month)]))
  missing_months <- character(0)
  if (length(completed_months) > 1) {
    month_seq <- seq(
      from = as.Date(paste0(completed_months[1], "-01")),
      to = as.Date(paste0(completed_months[length(completed_months)], "-01")),
      by = "month"
    )
    expected_months <- format(month_seq, "%Y-%m")
    missing_months <- setdiff(expected_months, completed_months)
  }

  issue_summary <- rbind(
    make_issue(
      "order_level_rows", n_orders, n_orders, "Information",
      "Processed order-level dataset row count.", "Use as baseline for order-level percentages."
    ),
    make_issue(
      "seller_order_rows", n_seller_rows, n_seller_rows, "Information",
      "Processed seller-order dataset row count.", "Use as baseline for seller-order checks."
    ),
    make_issue(
      "duplicate_order_id_processed", duplicate_order_id, n_orders,
      if (duplicate_order_id > 0) "High" else "Information",
      "Duplicate order_id values in processed order-level table.",
      if (duplicate_order_id > 0) "Stop analysis and rebuild processed order-level dataset." else "No action required."
    ),
    make_issue(
      "duplicate_seller_order_processed", duplicate_seller_order, n_seller_rows,
      if (duplicate_seller_order > 0) "High" else "Information",
      "Duplicate seller_id + order_id combinations in processed seller-order table.",
      if (duplicate_seller_order > 0) "Stop analysis and rebuild seller-order dataset." else "No action required."
    ),
    make_issue(
      "missing_customer_relationship", missing_customer, n_orders,
      if (missing_customer > 0) "High" else "Information",
      "Orders whose customer_id is absent from the customers table.",
      "Investigate orphaned customer references before customer-level analysis."
    ),
    make_issue(
      "missing_seller_relationship", missing_seller_items, nrow(items_raw),
      if (missing_seller_items > 0) "High" else "Information",
      "Order-item records whose seller_id is absent from the sellers table.",
      "Investigate orphaned seller references before seller-level analysis."
    ),
    make_issue(
      "negative_item_prices", sum(items_raw$price < 0, na.rm = TRUE), nrow(items_raw),
      if (any(items_raw$price < 0, na.rm = TRUE)) "High" else "Information",
      "Negative item prices in raw order items.",
      "Exclude or correct negative prices before monetary analysis."
    ),
    make_issue(
      "negative_freight_values", sum(items_raw$freight_value < 0, na.rm = TRUE), nrow(items_raw),
      if (any(items_raw$freight_value < 0, na.rm = TRUE)) "High" else "Information",
      "Negative freight values in raw order items.",
      "Exclude or correct negative freight values before shipping analysis."
    ),
    make_issue(
      "negative_payment_values", sum(payments_raw$payment_value < 0, na.rm = TRUE), nrow(payments_raw),
      if (any(payments_raw$payment_value < 0, na.rm = TRUE)) "High" else "Information",
      "Negative payment values in raw payments.",
      "Review payment records before revenue analysis."
    ),
    make_issue(
      "zero_payment_values", sum(order_df$total_payment_value == 0, na.rm = TRUE), n_orders, "Warning",
      "Orders with zero total payment value.",
      "Review voucher, unpaid, or not-yet-paid orders separately from paid-order analysis."
    ),
    make_issue(
      "missing_purchase_dates", sum(is.na(order_df$purchase_date)), n_orders, "Warning",
      "Orders missing purchase dates.",
      "Exclude from time-series analysis until purchase timestamps are validated."
    ),
    make_issue(
      "missing_estimated_delivery_dates",
      sum(is.na(order_df$estimated_delivery_date) | is.na(order_df$order_estimated_delivery_date)),
      n_orders, "Warning",
      "Orders missing estimated delivery dates.",
      "Exclude from lateness modeling when estimated delivery is unavailable."
    ),
    make_issue(
      "missing_actual_delivery_dates",
      sum(is.na(order_df$actual_delivery_date) | is.na(order_df$delivered_ts)),
      n_orders, "Information",
      "Orders missing actual delivery dates, including canceled and in-transit orders.",
      "Expected for non-delivered orders; investigate only when status indicates delivery."
    ),
    make_issue(
      "actual_delivery_before_purchase",
      sum(order_df$delivered_ts < order_df$purchase_ts, na.rm = TRUE),
      n_orders, "High",
      "Actual delivery timestamp occurs before purchase timestamp.",
      "Exclude or correct these records before delivery-duration analysis."
    ),
    make_issue(
      "estimated_delivery_before_purchase",
      sum(order_df$estimated_ts < order_df$purchase_ts, na.rm = TRUE),
      n_orders, "High",
      "Estimated delivery date occurs before purchase timestamp.",
      "Review date parsing and exclude invalid records from lateness analysis."
    ),
    make_issue(
      "approval_before_purchase",
      sum(order_df$approved_ts < order_df$purchase_ts, na.rm = TRUE),
      n_orders, "High",
      "Approval timestamp occurs before purchase timestamp.",
      "Review timestamp quality before approval-to-delivery analysis."
    ),
    make_issue(
      "delivered_without_actual_delivery_date",
      sum(order_df$order_status == "delivered" & (is.na(order_df$actual_delivery_date) | is.na(order_df$delivered_ts))),
      n_orders, "High",
      "Delivered orders lacking an actual delivery date.",
      "Treat as undelivered in delivery-time metrics until delivery timestamps are resolved."
    ),
    make_issue(
      "canceled_or_unavailable_with_actual_delivery_date",
      sum(order_df$canceled_or_unavailable == 1 & !is.na(order_df$actual_delivery_date)),
      n_orders, "Warning",
      "Canceled or unavailable orders that still have an actual delivery date.",
      "Review status logic before fulfillment performance reporting."
    ),
    make_issue(
      "orders_without_item_records", orders_without_items, n_orders,
      if (orders_without_items > 0) "Warning" else "Information",
      "Processed orders without corresponding item records.",
      "Investigate item aggregation and order-item joins."
    ),
    make_issue(
      "orders_without_payment_records", orders_without_payments, n_orders,
      if (orders_without_payments > 0) "Warning" else "Information",
      "Processed orders without corresponding payment records.",
      "Investigate payment aggregation and unpaid-order handling."
    ),
    make_issue(
      "payment_difference_exceeds_tolerance",
      sum(abs(order_df$payment_difference) > PAYMENT_TOLERANCE, na.rm = TRUE),
      n_orders, "Information",
      sprintf(
        "Orders where total_payment_value differs from total_item_price + total_freight_value by more than R$ %.2f.",
        PAYMENT_TOLERANCE
      ),
      "Review separately; vouchers, discounts, interest, and split payments may explain differences and are not automatically errors."
    ),
    make_issue(
      "extreme_payment_values_iqr", payment_outliers$count,
      sum(order_df$total_payment_value > 0, na.rm = TRUE), "Information",
      sprintf("Payment values above the IQR upper fence (R$ %.2f).", payment_outliers$cutoff),
      "Monitor high-value orders; do not delete without business review."
    ),
    make_issue(
      "extreme_freight_values_iqr", freight_outliers$count,
      sum(order_df$total_freight_value > 0, na.rm = TRUE), "Information",
      sprintf("Freight values above the IQR upper fence (R$ %.2f).", freight_outliers$cutoff),
      "Review shipping cost outliers for data entry or long-distance shipments."
    ),
    make_issue(
      "extreme_delivery_durations_iqr", delivery_outliers$count,
      sum(order_df$completed_order == 1 & !is.na(order_df$actual_delivery_days), na.rm = TRUE), "Information",
      sprintf("Delivery durations above the IQR upper fence (%.2f days).", delivery_outliers$cutoff),
      "Use robust summaries or caps when reporting typical delivery times."
    ),
    make_issue(
      "missing_or_invalid_seller_count",
      sum(is.na(order_df$seller_count) | order_df$seller_count < 1),
      n_orders,
      if (any(is.na(order_df$seller_count) | order_df$seller_count < 1)) "Warning" else "Information",
      "Orders with missing or invalid seller_count.",
      "Exclude from single- versus multi-seller comparisons until seller_count is validated."
    ),
    make_issue(
      "multi_seller_orders", sum(order_df$seller_count > 1, na.rm = TRUE), n_orders, "Information",
      "Orders fulfilled by more than one seller.",
      "Interpret seller-level delivery metrics carefully because delivery outcomes are shared at order level."
    ),
    make_issue(
      "missing_months_in_time_series", length(missing_months), length(completed_months), "Warning",
      if (length(missing_months) > 0) {
        paste("Missing purchase months in completed-order time series:", paste(missing_months, collapse = ", "))
      } else {
        "No missing months detected in completed-order purchase_month sequence."
      },
      "Exclude or annotate missing months before trend comparisons."
    )
  )

  overview <- data.frame(
    metric = c(
      "order_level_rows",
      "seller_order_rows",
      "issues_detected",
      "high_severity_issues",
      "warning_severity_issues",
      "information_severity_issues",
      "payment_tolerance",
      "duplicate_order_id_processed",
      "duplicate_seller_order_processed"
    ),
    value = c(
      n_orders,
      n_seller_rows,
      nrow(issue_summary),
      sum(issue_summary$severity == "High"),
      sum(issue_summary$severity == "Warning"),
      sum(issue_summary$severity == "Information"),
      PAYMENT_TOLERANCE,
      duplicate_order_id,
      duplicate_seller_order
    ),
    stringsAsFactors = FALSE
  )

  validate_audit_outputs(order_df, seller_df, issue_summary)

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(issue_summary, file.path(output_dir, "data_quality_issue_summary.csv"), row.names = FALSE)
  write.csv(missing_value_summary, file.path(output_dir, "missing_value_summary.csv"), row.names = FALSE)
  write.csv(duplicate_key_summary, file.path(output_dir, "duplicate_key_summary.csv"), row.names = FALSE)
  write.csv(date_consistency_summary, file.path(output_dir, "date_consistency_summary.csv"), row.names = FALSE)
  write.csv(monetary_quality_summary, file.path(output_dir, "monetary_quality_summary.csv"), row.names = FALSE)
  write.csv(outlier_summary, file.path(output_dir, "outlier_summary.csv"), row.names = FALSE)
  write.csv(relationship_summary, file.path(output_dir, "relationship_summary.csv"), row.names = FALSE)
  write.csv(overview, file.path(output_dir, "data_quality_overview.csv"), row.names = FALSE)

  png(file.path(output_dir, "missing_delivery_dates_by_status.png"), width = 900, height = 500)
  missing_delivery <- is.na(order_df$actual_delivery_date) | is.na(order_df$delivered_ts)
  status_missing <- sort(tapply(missing_delivery, order_df$order_status, sum), decreasing = TRUE)
  par(mar = c(8, 4, 4, 2) + 0.1)
  barplot(
    status_missing,
    las = 2,
    col = "#6366f1",
    border = "white",
    main = "Missing Actual Delivery Dates by Order Status",
    ylab = "Order Count"
  )
  dev.off()

  cat(sprintf("[AUDIT] Issues summarized: %d\n", nrow(issue_summary)))
  cat(sprintf("[AUDIT] High severity: %d | Warning: %d\n",
              sum(issue_summary$severity == "High"),
              sum(issue_summary$severity == "Warning")))

  list(
    issue_summary = issue_summary,
    overview = overview,
    duplicate_key_summary = duplicate_key_summary,
    missing_value_summary = missing_value_summary,
    date_consistency_summary = date_consistency_summary,
    monetary_quality_summary = monetary_quality_summary,
    outlier_summary = outlier_summary,
    relationship_summary = relationship_summary,
    output_dir = output_dir
  )
}

if (sys.nframe() == 0L) {
  invisible(run_data_quality_audit())
}
