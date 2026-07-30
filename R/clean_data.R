# clean_data.R
# Purpose: Pre-aggregate order items and payments, build order-level and
# seller-order analytical tables, validate integrity, and save processed files.

source("R/validation_helpers.R")
source("R/import_data.R")

aggregate_payment_metadata <- function(payments) {
  payment_split <- split(payments, payments$order_id)

  primary_payment_type <- vapply(payment_split, function(sub_df) {
    if (nrow(sub_df) == 0) {
      return("unknown")
    }
    sub_df$payment_type[which.max(sub_df$payment_value)]
  }, character(1))

  maximum_installments <- vapply(payment_split, function(sub_df) {
    if (nrow(sub_df) == 0) {
      return(1L)
    }
    as.integer(max(sub_df$payment_installments, na.rm = TRUE))
  }, integer(1))

  payment_method_count <- vapply(payment_split, function(sub_df) {
    if (nrow(sub_df) == 0) {
      return(0L)
    }
    as.integer(length(unique(sub_df$payment_type)))
  }, integer(1))

  list(
    primary_payment_type = primary_payment_type,
    maximum_installments = maximum_installments,
    payment_method_count = payment_method_count
  )
}

add_order_delivery_features <- function(df) {
  df$purchase_timestamp <- as.POSIXct(
    df$order_purchase_timestamp,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )
  df$delivered_customer_timestamp <- as.POSIXct(
    df$order_delivered_customer_date,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )
  df$estimated_delivery_timestamp <- as.POSIXct(
    df$order_estimated_delivery_date,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )

  df$purchase_date <- as.Date(df$purchase_timestamp)
  df$purchase_month <- format(df$purchase_date, "%Y-%m")
  df$actual_delivery_date <- as.Date(df$delivered_customer_timestamp)
  df$estimated_delivery_date <- as.Date(df$estimated_delivery_timestamp)

  df$actual_delivery_days <- as.numeric(
    difftime(df$delivered_customer_timestamp, df$purchase_timestamp, units = "days")
  )
  df$estimated_delivery_days <- as.numeric(
    difftime(df$estimated_delivery_timestamp, df$purchase_timestamp, units = "days")
  )
  df$delivery_delay_days <- as.numeric(
    difftime(df$delivered_customer_timestamp, df$estimated_delivery_timestamp, units = "days")
  )

  df$late_delivery <- ifelse(
    is.na(df$delivery_delay_days),
    NA_integer_,
    as.integer(df$delivery_delay_days > 0)
  )
  df$completed_order <- as.integer(df$order_status == "delivered")
  df$canceled_or_unavailable <- as.integer(df$order_status %in% c("canceled", "unavailable"))
  df$freight_to_item_ratio <- ifelse(
    df$total_item_price > 0,
    df$total_freight_value / df$total_item_price,
    0
  )

  return(df)
}

fill_missing_order_metrics <- function(df) {
  zero_cols <- c(
    "total_item_price", "total_freight_value", "item_count",
    "unique_product_count", "seller_count", "single_seller_order",
    "total_payment_value"
  )

  for (col in zero_cols) {
    df[[col]][is.na(df[[col]])] <- 0
  }

  df$maximum_installments[is.na(df$maximum_installments)] <- 1L
  df$payment_method_count[is.na(df$payment_method_count)] <- 0L
  df$primary_payment_type[is.na(df$primary_payment_type)] <- "not_paid"

  return(df)
}

clean_and_process_data <- function(raw_dir = "data/raw", processed_dir = "data/processed") {
  cat("[CLEAN] Starting data cleaning and processing...\n")

  raw_data <- import_raw_data(raw_dir)
  orders <- raw_data$orders
  items <- raw_data$order_items
  payments <- raw_data$order_payments
  customers <- raw_data$customers

  original_order_count <- nrow(orders)
  original_payment_total <- sum(payments$payment_value, na.rm = TRUE)
  original_item_price_total <- sum(items$price, na.rm = TRUE)
  original_freight_total <- sum(items$freight_value, na.rm = TRUE)

  cat("[CLEAN] Aggregating order items by order_id...\n")
  agg_item_totals <- aggregate(
    cbind(
      total_item_price = items$price,
      total_freight_value = items$freight_value,
      item_count = rep(1L, nrow(items))
    ),
    by = list(order_id = items$order_id),
    FUN = sum,
    na.rm = TRUE
  )

  agg_item_uniques <- aggregate(
    cbind(
      unique_product_count = items$product_id,
      seller_count = items$seller_id
    ),
    by = list(order_id = items$order_id),
    FUN = function(x) length(unique(x))
  )

  agg_items <- merge(agg_item_totals, agg_item_uniques, by = "order_id", all.x = TRUE)
  agg_items$single_seller_order <- as.integer(agg_items$seller_count == 1L)

  cat("[CLEAN] Aggregating payments by order_id...\n")
  agg_payments <- aggregate(
    payment_value ~ order_id,
    data = payments,
    FUN = sum,
    na.rm = TRUE
  )
  names(agg_payments)[names(agg_payments) == "payment_value"] <- "total_payment_value"

  payment_metadata <- aggregate_payment_metadata(payments)
  agg_payments$maximum_installments <- payment_metadata$maximum_installments[agg_payments$order_id]
  agg_payments$payment_method_count <- payment_metadata$payment_method_count[agg_payments$order_id]
  agg_payments$primary_payment_type <- payment_metadata$primary_payment_type[agg_payments$order_id]

  cat("[CLEAN] Joining order-level analytical table...\n")
  orders_merged <- merge(
    orders,
    customers[, c("customer_id", "customer_city", "customer_state")],
    by = "customer_id",
    all.x = TRUE
  )
  orders_merged <- merge(orders_merged, agg_items, by = "order_id", all.x = TRUE)
  orders_merged <- merge(orders_merged, agg_payments, by = "order_id", all.x = TRUE)
  orders_merged <- fill_missing_order_metrics(orders_merged)
  orders_merged <- add_order_delivery_features(orders_merged)

  cat("[CLEAN] Building seller-order analytical table...\n")
  seller_order_df <- aggregate(
    cbind(
      seller_item_value = items$price,
      seller_freight_value = items$freight_value,
      seller_item_count = rep(1L, nrow(items))
    ),
    by = list(seller_id = items$seller_id, order_id = items$order_id),
    FUN = sum,
    na.rm = TRUE
  )

  order_info_cols <- c(
    "order_id", "order_status", "purchase_date", "estimated_delivery_date",
    "actual_delivery_date", "delivery_delay_days", "late_delivery"
  )
  seller_order_df <- merge(
    seller_order_df,
    orders_merged[, order_info_cols],
    by = "order_id",
    all.x = TRUE
  )

  seller_order_df <- seller_order_df[, c(
    "seller_id", "order_id", "seller_item_count", "seller_item_value",
    "seller_freight_value", "order_status", "purchase_date",
    "estimated_delivery_date", "actual_delivery_date", "delivery_delay_days",
    "late_delivery"
  )]

  validation_summary <- validate_data_preparation(
    order_level_df = orders_merged,
    seller_order_df = seller_order_df,
    original_order_count = original_order_count,
    original_payment_total = original_payment_total,
    original_item_price_total = original_item_price_total,
    original_freight_total = original_freight_total
  )

  dir.create(processed_dir, showWarnings = FALSE, recursive = TRUE)
  order_csv <- file.path(processed_dir, "order_level_analysis.csv")
  seller_csv <- file.path(processed_dir, "seller_order_analysis.csv")

  write.csv(orders_merged, order_csv, row.names = FALSE)
  write.csv(seller_order_df, seller_csv, row.names = FALSE)
  saveRDS(orders_merged, file.path(processed_dir, "order_level_analysis.rds"))
  saveRDS(seller_order_df, file.path(processed_dir, "seller_order_analysis.rds"))

  cat("[CLEAN] Saved processed files:\n")
  cat(sprintf("  - %s\n", order_csv))
  cat(sprintf("  - %s\n", seller_csv))
  cat("[CLEAN] Data preparation completed successfully.\n")

  return(list(
    order_level = orders_merged,
    seller_order = seller_order_df,
    validation = validation_summary
  ))
}

if (sys.nframe() == 0L) {
  invisible(clean_and_process_data())
}
