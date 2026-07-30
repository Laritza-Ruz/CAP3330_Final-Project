# import_data.R
# Purpose: Import raw Olist CSV files and validate required structure.

REQUIRED_RAW_FILES <- c(
  "olist_orders_dataset.csv",
  "olist_order_items_dataset.csv",
  "olist_order_payments_dataset.csv",
  "olist_customers_dataset.csv",
  "olist_products_dataset.csv",
  "olist_sellers_dataset.csv"
)

REQUIRED_COLUMNS <- list(
  orders = c(
    "order_id", "customer_id", "order_status", "order_purchase_timestamp",
    "order_delivered_customer_date", "order_estimated_delivery_date"
  ),
  order_items = c("order_id", "product_id", "seller_id", "price", "freight_value"),
  order_payments = c("order_id", "payment_value", "payment_type", "payment_installments"),
  customers = c("customer_id", "customer_city", "customer_state"),
  products = c("product_id"),
  sellers = c("seller_id")
)

validate_required_columns <- function(df, required_cols, dataset_label) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "[IMPORT ERROR] %s is missing required columns: %s",
      dataset_label,
      paste(missing_cols, collapse = ", ")
    ))
  }
  return(TRUE)
}

import_raw_data <- function(data_dir = "data/raw") {
  cat("[IMPORT] Importing raw Olist CSV datasets...\n")

  raw_data <- list()

  for (file_name in REQUIRED_RAW_FILES) {
    path <- file.path(data_dir, file_name)
    if (!file.exists(path)) {
      stop(sprintf("[IMPORT ERROR] Required raw file is missing: %s", path))
    }

    dataset_name <- gsub("olist_|_dataset.csv", "", file_name)
    cat(sprintf("[IMPORT] Reading %s...\n", file_name))
    df <- read.csv(path, stringsAsFactors = FALSE)
    validate_required_columns(df, REQUIRED_COLUMNS[[dataset_name]], dataset_name)
    raw_data[[dataset_name]] <- df
  }

  cat(sprintf(
    "[IMPORT] Loaded rows: orders (%d), items (%d), payments (%d), customers (%d), products (%d), sellers (%d)\n",
    nrow(raw_data$orders),
    nrow(raw_data$order_items),
    nrow(raw_data$order_payments),
    nrow(raw_data$customers),
    nrow(raw_data$products),
    nrow(raw_data$sellers)
  ))

  cat("[IMPORT] All raw datasets imported successfully.\n")
  return(raw_data)
}
