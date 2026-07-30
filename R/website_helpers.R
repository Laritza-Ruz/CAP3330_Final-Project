# website_helpers.R — UI helpers for Quarto dashboard (no statistical logic)

silent_run <- function(expr) {
  invisible(capture.output(
    suppressMessages(suppressWarnings(
      eval(substitute(expr), envir = parent.frame())
    )),
    type = c("output", "message")
  ))
}

kpi_card <- function(icon, label, value, desc = "", accent = "blue") {
  sprintf(
    paste0(
      '<div class="kpi-card kpi-accent-%s">',
      '<div class="kpi-icon"><i class="bi bi-%s"></i></div>',
      '<div class="kpi-body">',
      '<div class="kpi-value">%s</div>',
      '<div class="kpi-label">%s</div>',
      '<div class="kpi-desc">%s</div>',
      '</div></div>'
    ),
    accent,
    icon,
    value,
    label,
    desc
  )
}

kpi_row <- function(cards) {
  paste0('<div class="kpi-row">', paste(cards, collapse = ""), "</div>")
}

panel_card <- function(title, content_html, icon = NULL) {
  icon_html <- if (!is.null(icon)) {
    sprintf('<span class="panel-icon"><i class="bi bi-%s"></i></span>', icon)
  } else {
    ""
  }
  sprintf(
    '<div class="dash-panel"><div class="panel-header">%s<span>%s</span></div><div class="panel-body">%s</div></div>',
    icon_html,
    title,
    content_html
  )
}

short_id <- function(x, n = 8) {
  substr(as.character(x), 1, n)
}

format_pvalue <- function(p) {
  if (length(p) == 0 || all(is.na(p))) return("NA")
  vapply(p, function(x) {
    if (is.na(x)) return("NA")
    if (x < 0.001) return("< 0.001")
    format(signif(x, 3), scientific = FALSE, trim = TRUE)
  }, character(1), USE.NAMES = FALSE)
}

issue_label <- function(name) {
  labels <- c(
    delivered_without_actual_delivery_date = "Delivered orders missing delivery dates",
    canceled_or_unavailable_with_actual_delivery_date = "Canceled or unavailable orders with delivery dates",
    orders_without_item_records = "Orders without item records",
    orders_without_payment_records = "Orders without payment records",
    zero_payment_values = "Zero-payment orders",
    missing_months_in_time_series = "Missing month in trend series",
    missing_purchase_dates = "Orders missing purchase dates",
    missing_estimated_delivery_dates = "Orders missing estimated delivery dates",
    missing_actual_delivery_dates = "Orders missing actual delivery dates",
    actual_delivery_before_purchase = "Delivery dates before purchase",
    estimated_delivery_before_purchase = "Estimated delivery before purchase",
    approval_before_purchase = "Approval before purchase",
    payment_difference_exceeds_tolerance = "Payment totals differ from item plus freight",
    missing_customer_relationship = "Orders with missing customer records",
    missing_seller_relationship = "Items with missing seller records",
    negative_item_prices = "Negative item prices",
    negative_freight_values = "Negative freight values",
    negative_payment_values = "Negative payment values",
    missing_or_invalid_seller_count = "Orders with invalid seller count",
    duplicate_order_id_processed = "Duplicate order keys",
    duplicate_seller_order_processed = "Duplicate seller-order keys"
  )
  if (name %in% names(labels)) labels[[name]] else gsub("_", " ", name)
}

passed_check_label <- function(name) {
  labels <- c(
    duplicate_order_id_processed = "No duplicate order keys",
    duplicate_seller_order_processed = "No duplicate seller-order keys",
    missing_customer_relationship = "No orphaned customers",
    missing_seller_relationship = "No orphaned sellers",
    negative_item_prices = "No negative item prices",
    negative_freight_values = "No negative freight values",
    negative_payment_values = "No negative payments",
    actual_delivery_before_purchase = "No delivery dates before purchase",
    estimated_delivery_before_purchase = "No estimated delivery before purchase",
    approval_before_purchase = "No approval before purchase",
    missing_purchase_dates = "No missing purchase dates",
    missing_estimated_delivery_dates = "No missing estimated-delivery dates"
  )
  labels[[name]]
}

predictor_label <- function(term) {
  labels <- c(
    seller_count = "Seller count",
    log1p_total_freight_value = "Freight value (log)",
    item_count = "Item count",
    log1p_total_item_price = "Order value (log)",
    estimated_delivery_days = "Estimated delivery days",
    maximum_installments = "Maximum installments",
    total_payment_value = "Order value",
    total_freight_value = "Freight value"
  )
  vapply(term, function(x) {
    if (x %in% names(labels)) labels[[x]] else gsub("_", " ", x)
  }, character(1), USE.NAMES = FALSE)
}
