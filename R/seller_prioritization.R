# seller_prioritization.R
# Purpose: Rank sellers for operational review using delivery performance and exposure.

source("R/validation_helpers.R")

MIN_SELLER_ORDERS <- 20L
MIN_VALID_DELIVERED_ORDERS <- 20L

SCORE_WEIGHTS <- c(
  late_delivery_rate = 0.40,
  canceled_or_unavailable_rate = 0.25,
  positive_median_delay = 0.20,
  order_volume_exposure = 0.15
)

normalize_vector <- function(x) {
  x <- as.numeric(x)
  if (length(x) == 0) {
    return(x)
  }
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    return(rep(0, length(x)))
  }
  (x - rng[1]) / (rng[2] - rng[1])
}

cap_upper_quantile <- function(x, prob = 0.99) {
  x <- as.numeric(x)
  upper <- as.numeric(quantile(x, probs = prob, na.rm = TRUE))
  pmin(x, upper)
}

prepare_seller_order_data <- function(so_df) {
  if (!"canceled_or_unavailable" %in% names(so_df) && "order_status" %in% names(so_df)) {
    so_df$canceled_or_unavailable <- as.integer(so_df$order_status %in% c("canceled", "unavailable"))
  }
  if (!"completed_order" %in% names(so_df) && "order_status" %in% names(so_df)) {
    so_df$completed_order <- as.integer(so_df$order_status == "delivered")
  }
  so_df
}

validate_seller_prioritization_inputs <- function(so_df, min_orders) {
  required <- c(
    "seller_id", "order_id", "order_status", "late_delivery",
    "delivery_delay_days", "seller_freight_value", "seller_item_value"
  )
  for (var in required) {
    validate_variable_exists(so_df, var)
  }

  composite_key <- paste(so_df$seller_id, so_df$order_id, sep = "|")
  if (any(duplicated(composite_key))) {
    stop("[VALIDATION ERROR] seller_id + order_id is not unique in seller-order dataset.")
  }

  if (min_orders < 1) {
    stop("[VALIDATION ERROR] Minimum seller-order threshold must be at least 1.")
  }

  TRUE
}

aggregate_seller_metrics <- function(so_df) {
  seller_ids <- sort(unique(so_df$seller_id))

  aggregate_by_seller <- function(values, ids, FUN) {
    out <- tapply(values, ids, FUN)
    as.numeric(out[seller_ids])
  }

  delivered_mask <- so_df$completed_order == 1 & !is.na(so_df$late_delivery)
  delay_mask <- so_df$completed_order == 1 & !is.na(so_df$delivery_delay_days)

  positive_delay <- pmax(so_df$delivery_delay_days, 0)

  late_values <- ifelse(delivered_mask, so_df$late_delivery, NA_real_)

  seller_summary <- data.frame(
    seller_id = seller_ids,
    seller_order_count = aggregate_by_seller(rep(1, nrow(so_df)), so_df$seller_id, sum),
    completed_order_count = aggregate_by_seller(so_df$completed_order, so_df$seller_id, sum),
    canceled_or_unavailable_count = aggregate_by_seller(so_df$canceled_or_unavailable, so_df$seller_id, sum),
    late_delivery_count = aggregate_by_seller(
      late_values,
      so_df$seller_id,
      function(x) sum(x, na.rm = TRUE)
    ),
    delivered_with_late_flag_count = aggregate_by_seller(
      as.integer(delivered_mask),
      so_df$seller_id,
      sum
    ),
    total_item_value = aggregate_by_seller(so_df$seller_item_value, so_df$seller_id, sum),
    stringsAsFactors = FALSE
  )

  median_delay <- tapply(
    positive_delay[delay_mask],
    so_df$seller_id[delay_mask],
    median,
    na.rm = TRUE
  )
  median_freight <- tapply(
    so_df$seller_freight_value,
    so_df$seller_id,
    median,
    na.rm = TRUE
  )

  seller_summary$median_delivery_delay_days <- as.numeric(median_delay[seller_summary$seller_id])
  seller_summary$median_seller_freight_value <- as.numeric(median_freight[seller_summary$seller_id])
  seller_summary$median_delivery_delay_days[is.na(seller_summary$median_delivery_delay_days)] <- 0
  seller_summary$median_seller_freight_value[is.na(seller_summary$median_seller_freight_value)] <- 0

  seller_summary$valid_delivery_count <- seller_summary$delivered_with_late_flag_count
  seller_summary$late_delivery_rate <- ifelse(
    seller_summary$valid_delivery_count > 0,
    seller_summary$late_delivery_count / seller_summary$valid_delivery_count,
    0
  )
  seller_summary$canceled_or_unavailable_rate <-
    seller_summary$canceled_or_unavailable_count / seller_summary$seller_order_count

  seller_summary
}

document_multiseller_handling <- function(so_df, order_df = NULL) {
  unique_orders <- length(unique(so_df$order_id))
  multi_seller_rows <- 0
  multi_seller_orders <- 0

  if (!is.null(order_df) && "seller_count" %in% names(order_df)) {
    order_lookup <- order_df[, c("order_id", "seller_count")]
    merged <- merge(so_df[, c("seller_id", "order_id")], order_lookup, by = "order_id", all.x = TRUE)
    multi_seller_rows <- sum(merged$seller_count > 1, na.rm = TRUE)
    multi_seller_orders <- length(unique(merged$order_id[merged$seller_count > 1 & !is.na(merged$seller_count)]))
  } else {
    order_counts <- tapply(so_df$seller_id, so_df$order_id, function(x) length(unique(x)))
    multi_seller_orders <- sum(order_counts > 1)
    multi_seller_rows <- sum(order_counts[order_counts > 1])
  }

  c(
    sprintf(
      "Analysis uses seller-order grain (one row per seller_id + order_id); %d seller-order records span %d unique orders.",
      nrow(so_df), unique_orders
    ),
    sprintf(
      "Multi-seller orders affect %d seller-order rows across %d orders; delivery outcomes are order-level and shared across sellers fulfilling the same order.",
      multi_seller_rows, multi_seller_orders
    ),
    "The score does not assign full multi-seller order blame to one seller; each seller is evaluated on their seller-order records with shared delivery outcomes."
  )
}

validate_seller_scores <- function(ranked_sellers) {
  if (any(duplicated(ranked_sellers$seller_id))) {
    stop("[VALIDATION ERROR] Duplicate seller_id rows found in final ranking.")
  }

  rate_cols <- c("late_delivery_rate", "canceled_or_unavailable_rate")
  for (col in rate_cols) {
    if (any(ranked_sellers[[col]] < 0 | ranked_sellers[[col]] > 1, na.rm = TRUE)) {
      stop(sprintf("[VALIDATION ERROR] '%s' contains values outside [0, 1].", col))
    }
  }

  if (any(!is.finite(ranked_sellers$priority_score))) {
    stop("[VALIDATION ERROR] Final prioritization score contains missing or infinite values.")
  }

  top_seller <- ranked_sellers$seller_id[which.max(ranked_sellers$priority_score)]
  if (ranked_sellers$priority_score[ranked_sellers$seller_id == top_seller] <
      max(ranked_sellers$priority_score, na.rm = TRUE)) {
    stop("[VALIDATION ERROR] Highest score does not correspond to top-ranked seller.")
  }

  TRUE
}

compute_priority_scores <- function(eligible_sellers) {
  eligible_sellers$late_rate_norm <- normalize_vector(eligible_sellers$late_delivery_rate)
  eligible_sellers$canceled_rate_norm <- normalize_vector(eligible_sellers$canceled_or_unavailable_rate)

  delay_for_score <- cap_upper_quantile(eligible_sellers$median_delivery_delay_days, prob = 0.99)
  eligible_sellers$positive_delay_norm <- normalize_vector(delay_for_score)

  volume_for_score <- log1p(eligible_sellers$seller_order_count)
  eligible_sellers$volume_exposure_norm <- normalize_vector(volume_for_score)

  eligible_sellers$priority_score <-
    SCORE_WEIGHTS["late_delivery_rate"] * eligible_sellers$late_rate_norm +
    SCORE_WEIGHTS["canceled_or_unavailable_rate"] * eligible_sellers$canceled_rate_norm +
    SCORE_WEIGHTS["positive_median_delay"] * eligible_sellers$positive_delay_norm +
    SCORE_WEIGHTS["order_volume_exposure"] * eligible_sellers$volume_exposure_norm

  eligible_sellers$score_late_component <- SCORE_WEIGHTS["late_delivery_rate"] * eligible_sellers$late_rate_norm
  eligible_sellers$score_cancel_component <- SCORE_WEIGHTS["canceled_or_unavailable_rate"] * eligible_sellers$canceled_rate_norm
  eligible_sellers$score_delay_component <- SCORE_WEIGHTS["positive_median_delay"] * eligible_sellers$positive_delay_norm
  eligible_sellers$score_volume_component <- SCORE_WEIGHTS["order_volume_exposure"] * eligible_sellers$volume_exposure_norm

  eligible_sellers
}

run_seller_prioritization <- function(
    seller_data_path = "data/processed/seller_order_analysis.rds",
    order_data_path = "data/processed/order_level_analysis.rds",
    min_orders = MIN_SELLER_ORDERS,
    output_dir = "output/seller",
    top_n = 10) {
  cat("[SELLER] Loading seller-order dataset...\n")

  if (!file.exists(seller_data_path)) {
    stop(sprintf("Seller-order dataset not found at %s. Run clean_data.R first.", seller_data_path))
  }

  so_df <- readRDS(seller_data_path)
  so_df <- prepare_seller_order_data(so_df)
  validate_seller_prioritization_inputs(so_df, min_orders)

  order_df <- NULL
  if (file.exists(order_data_path)) {
    order_df <- readRDS(order_data_path)
  }

  multiseller_notes <- document_multiseller_handling(so_df, order_df)
  seller_summary <- aggregate_seller_metrics(so_df)

  total_sellers <- nrow(seller_summary)
  eligible_mask <- seller_summary$seller_order_count >= min_orders &
    seller_summary$valid_delivery_count >= MIN_VALID_DELIVERED_ORDERS
  eligible_sellers <- seller_summary[eligible_mask, , drop = FALSE]
  excluded_sellers <- seller_summary[!eligible_mask, , drop = FALSE]
  excluded_insufficient_records <- sum(seller_summary$seller_order_count < min_orders)
  excluded_insufficient_delivered <- sum(
    seller_summary$seller_order_count >= min_orders &
      seller_summary$valid_delivery_count < MIN_VALID_DELIVERED_ORDERS
  )

  if (nrow(eligible_sellers) == 0) {
    stop("[VALIDATION ERROR] No sellers meet the minimum seller-order and delivered-order thresholds.")
  }

  scored_sellers <- compute_priority_scores(eligible_sellers)
  ranked_sellers <- scored_sellers[order(scored_sellers$priority_score, decreasing = TRUE), ]
  ranked_sellers$rank <- seq_len(nrow(ranked_sellers))
  rownames(ranked_sellers) <- NULL

  validate_seller_scores(ranked_sellers)

  top_sellers <- head(ranked_sellers, top_n)

  formula_text <- paste0(
    "priority_score = ",
    paste(
      sprintf(
        "%.0f%% * %s_norm",
        100 * SCORE_WEIGHTS["late_delivery_rate"],
        "late_delivery_rate"
      ),
      sprintf(
        "%.0f%% * %s_norm",
        100 * SCORE_WEIGHTS["canceled_or_unavailable_rate"],
        "canceled_or_unavailable_rate"
      ),
      sprintf(
        "%.0f%% * %s_norm",
        100 * SCORE_WEIGHTS["positive_median_delay"],
        "positive_median_delay"
      ),
      sprintf(
        "%.0f%% * %s_norm",
        100 * SCORE_WEIGHTS["order_volume_exposure"],
        "log1p(order_volume)"
      ),
      sep = " + "
    )
  )

  interpretation <- paste(
    "Sellers with higher prioritization scores combine elevated late-delivery rates,",
    "cancellation/unavailable exposure, longer positive delivery delays, and meaningful order volume.",
    "Operations managers should review top-ranked sellers first, recognizing that shared order-level",
    "delivery outcomes in multi-seller orders may reflect logistics factors outside a single seller's control."
  )

  limitation <- paste(
    "Delivery outcomes are measured at the order level and may be shared across sellers in multi-seller orders.",
    "The score indicates operational review priority based on observed associations, not proven seller-caused delays."
  )

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  ranking_cols <- c(
    "rank", "seller_id", "seller_order_count", "valid_delivery_count", "late_delivery_count",
    "completed_order_count", "canceled_or_unavailable_count", "late_delivery_rate",
    "canceled_or_unavailable_rate", "median_delivery_delay_days", "median_seller_freight_value",
    "total_item_value", "priority_score"
  )
  write.csv(
    ranked_sellers[, ranking_cols],
    file.path(output_dir, "seller_priority_ranking.csv"),
    row.names = FALSE
  )

  component_cols <- c(
    "rank", "seller_id", "priority_score",
    "late_delivery_rate", "late_rate_norm", "score_late_component",
    "canceled_or_unavailable_rate", "canceled_rate_norm", "score_cancel_component",
    "median_delivery_delay_days", "positive_delay_norm", "score_delay_component",
    "seller_order_count", "volume_exposure_norm", "score_volume_component"
  )
  write.csv(
    ranked_sellers[, component_cols],
    file.path(output_dir, "seller_priority_components.csv"),
    row.names = FALSE
  )

  eligibility_summary <- data.frame(
    metric = c(
      "total_sellers_in_dataset",
      "sellers_eligible",
      "sellers_excluded_total",
      "sellers_excluded_insufficient_seller_order_records",
      "sellers_excluded_insufficient_valid_delivered_orders",
      "minimum_seller_order_threshold",
      "minimum_valid_delivered_order_threshold",
      "eligibility_rule",
      "prioritization_formula",
      "score_min",
      "score_max",
      "multiseller_handling_note_1",
      "multiseller_handling_note_2",
      "multiseller_handling_note_3"
    ),
    value = c(
      total_sellers,
      nrow(eligible_sellers),
      nrow(excluded_sellers),
      excluded_insufficient_records,
      excluded_insufficient_delivered,
      min_orders,
      MIN_VALID_DELIVERED_ORDERS,
      "seller_order_count >= 20 AND valid_delivery_count >= 20",
      formula_text,
      min(ranked_sellers$priority_score),
      max(ranked_sellers$priority_score),
      multiseller_notes[1],
      multiseller_notes[2],
      multiseller_notes[3]
    ),
    stringsAsFactors = FALSE
  )
  write.csv(
    eligibility_summary,
    file.path(output_dir, "seller_eligibility_summary.csv"),
    row.names = FALSE
  )

  top_plot_df <- top_sellers[nrow(top_sellers):1, , drop = FALSE]
  png(file.path(output_dir, "top_seller_priority_plot.png"), width = 900, height = 550)
  par(mar = c(5, 10, 4, 2) + 0.1)
  barplot(
    top_plot_df$priority_score,
    horiz = TRUE,
    names.arg = substr(top_plot_df$seller_id, 1, 8),
    col = "#ef4444",
    border = "white",
    main = "Top Sellers for Operational Review",
    xlab = "Prioritization Score (Higher = Higher Review Priority)",
    las = 1
  )
  dev.off()

  cat(sprintf("[SELLER] Total sellers analyzed: %d\n", total_sellers))
  cat(sprintf("[SELLER] Eligible sellers: %d | Excluded: %d\n", nrow(eligible_sellers), nrow(excluded_sellers)))
  cat(sprintf("[SELLER] Score range: %.4f to %.4f\n",
              min(ranked_sellers$priority_score), max(ranked_sellers$priority_score)))

  list(
    total_sellers_analyzed = total_sellers,
    eligible_sellers = nrow(eligible_sellers),
    excluded_sellers = nrow(excluded_sellers),
    min_orders_threshold = min_orders,
    prioritization_formula = formula_text,
    ranked_sellers = ranked_sellers,
    top_sellers = top_sellers,
    score_range = range(ranked_sellers$priority_score),
    multiseller_notes = multiseller_notes,
    interpretation = interpretation,
    limitation = limitation,
    output_dir = output_dir
  )
}

if (sys.nframe() == 0L) {
  invisible(run_seller_prioritization())
}
