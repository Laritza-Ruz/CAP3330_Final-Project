# compare_deliveries.R
# Purpose: Compare actual_delivery_days between single-seller and multi-seller orders.

source("R/validation_helpers.R")

GROUP_LABELS <- c(
  "0" = "Multi-seller order",
  "1" = "Single-seller order"
)

calc_skewness <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3 || sd(x) == 0) {
    return(0)
  }
  m <- mean(x)
  s <- sd(x)
  mean((x - m)^3) / (s^3)
}

calc_iqr <- function(x) {
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  unname(q[2] - q[1])
}

validate_compare_inputs <- function(df_sub) {
  validate_variable_exists(df_sub, "single_seller_order")
  validate_variable_exists(df_sub, "actual_delivery_days")
  validate_variable_type(df_sub, "actual_delivery_days", "numeric")
  validate_two_groups(df_sub, "single_seller_order")

  n_single <- sum(df_sub$single_seller_order == 1)
  n_multi <- sum(df_sub$single_seller_order == 0)
  if (n_single < 10 || n_multi < 10) {
    stop("[VALIDATION ERROR] Insufficient observations in one or both groups for comparison.")
  }

  return(TRUE)
}

select_comparison_test <- function(group_single, group_multi) {
  skew_single <- calc_skewness(group_single)
  skew_multi <- calc_skewness(group_multi)

  iqr_single <- calc_iqr(group_single)
  iqr_multi <- calc_iqr(group_multi)
  outlier_single <- sum(group_single > quantile(group_single, 0.75, na.rm = TRUE) + 1.5 * iqr_single)
  outlier_multi <- sum(group_multi > quantile(group_multi, 0.75, na.rm = TRUE) + 1.5 * iqr_multi)
  outlier_rate <- (outlier_single + outlier_multi) / (length(group_single) + length(group_multi))

  highly_skewed <- max(abs(skew_single), abs(skew_multi)) > 1
  use_wilcoxon <- highly_skewed || outlier_rate > 0.05

  selection_reason <- if (use_wilcoxon) {
    sprintf(
      paste(
        "Wilcoxon rank-sum test selected because delivery times are right-skewed",
        "(skew single = %.2f, multi = %.2f) with %.2f%% tail outliers.",
        "This test compares delivery-time distributions and is more appropriate than a mean-based",
        "t-test when extreme delivery delays dominate the upper tail."
      ),
      skew_single, skew_multi, 100 * outlier_rate
    )
  } else {
    "Welch t-test selected because group distributions are moderately symmetric with limited tail outliers, supporting mean comparison."
  }

  list(
    method = if (use_wilcoxon) "wilcoxon" else "welch",
    selection_reason = selection_reason,
    skew_single = skew_single,
    skew_multi = skew_multi,
    outlier_rate = outlier_rate
  )
}

run_compare_deliveries <- function(
    data_path = "data/processed/order_level_analysis.rds",
    output_dir = "output",
    plot_dir = "images") {
  cat("[COMPARE] Loading processed order-level dataset...\n")

  if (!file.exists(data_path)) {
    stop(sprintf("Processed dataset not found at %s. Run clean_data.R first.", data_path))
  }

  df <- readRDS(data_path)

  excluded <- list(
    missing_seller_count = sum(is.na(df$seller_count)),
    seller_count_below_one = sum(!is.na(df$seller_count) & df$seller_count < 1),
    missing_delivery_days = sum(
      df$completed_order == 1 &
        (is.na(df$actual_delivery_days) |
           is.na(df$actual_delivery_date) |
           df$order_delivered_customer_date == "")
    ),
    negative_delivery_days = sum(
      !is.na(df$actual_delivery_days) & df$actual_delivery_days < 0
    ),
    canceled_or_unavailable = sum(df$canceled_or_unavailable == 1, na.rm = TRUE),
    not_delivered = sum(df$completed_order != 1, na.rm = TRUE)
  )

  df_sub <- df[
    !is.na(df$seller_count) &
      df$seller_count >= 1 &
      !is.na(df$actual_delivery_days) &
      df$actual_delivery_days >= 0 &
      df$completed_order == 1 &
      df$canceled_or_unavailable == 0,
  ]

  validate_compare_inputs(df_sub)

  group_single <- df_sub$actual_delivery_days[df_sub$single_seller_order == 1]
  group_multi <- df_sub$actual_delivery_days[df_sub$single_seller_order == 0]

  n_single <- length(group_single)
  n_multi <- length(group_multi)

  group_summary <- data.frame(
    group = c("Single-seller order", "Multi-seller order"),
    sample_size = c(n_single, n_multi),
    mean_days = c(mean(group_single), mean(group_multi)),
    median_days = c(median(group_single), median(group_multi)),
    sd_days = c(sd(group_single), sd(group_multi)),
    iqr_days = c(calc_iqr(group_single), calc_iqr(group_multi)),
    stringsAsFactors = FALSE
  )

  test_choice <- select_comparison_test(group_single, group_multi)

  if (test_choice$method == "welch") {
    test_res <- t.test(group_multi, group_single, var.equal = FALSE)
    est_diff <- unname(diff(test_res$estimate))
    conf_int <- test_res$conf.int
    test_statistic <- unname(test_res$statistic)
    p_value <- test_res$p.value
    test_name <- "Welch two-sample t-test"
    estimate_label <- "Estimated mean difference (multi - single), days"
  } else {
    test_res <- wilcox.test(
      group_multi,
      group_single,
      conf.int = TRUE,
      exact = FALSE,
      alternative = "two.sided"
    )
    est_diff <- unname(test_res$estimate)
    conf_int <- test_res$conf.int
    test_statistic <- unname(test_res$statistic)
    p_value <- test_res$p.value
    test_name <- "Wilcoxon rank-sum test"
    estimate_label <- "Hodges-Lehmann estimate of location shift (multi - single), days"
  }

  faster_group <- if (median(group_multi) > median(group_single)) {
    "Multi-seller orders delivered slower (higher median delivery time)."
  } else if (median(group_multi) < median(group_single)) {
    "Single-seller orders delivered slower (higher median delivery time)."
  } else {
    "Both groups share the same median delivery time."
  }

  median_diff <- median(group_multi) - median(group_single)
  practical_note <- if (abs(median_diff) < 1) {
    "The median difference is less than one day, which is likely too small for major operational action despite statistical significance at large sample sizes."
  } else if (abs(median_diff) < 3) {
    "The median difference is modest and may matter for SLA planning but is not a large operational gap on its own."
  } else {
    "The median difference appears large enough to review fulfillment routing and seller coordination policies."
  }

  cannot_conclude <- paste(
    "This analysis cannot prove that multiple sellers directly cause longer delivery.",
    "Seller count may correlate with order complexity, product mix, distance, or seller location."
  )

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

  write.csv(group_summary, file.path(output_dir, "compare_group_summary.csv"), row.names = FALSE)
  write.csv(
    data.frame(metric = names(excluded), excluded_count = unname(unlist(excluded))),
    file.path(output_dir, "compare_exclusion_counts.csv"),
    row.names = FALSE
  )

  df_sub$delivery_group <- factor(
    ifelse(df_sub$single_seller_order == 1, GROUP_LABELS[["1"]], GROUP_LABELS[["0"]]),
    levels = c(GROUP_LABELS[["1"]], GROUP_LABELS[["0"]])
  )

  png(file.path(plot_dir, "delivery_comparison_boxplot.png"), width = 800, height = 500)
  par(mar = c(8, 4, 4, 2) + 0.1)
  boxplot(
    actual_delivery_days ~ delivery_group,
    data = df_sub,
    col = c("#3b82f6", "#f59e0b"),
    border = "#1e293b",
    main = "Delivery Time Comparison\nSingle-Seller vs. Multi-Seller Orders",
    xlab = "",
    ylab = "Actual Delivery Days",
    ylim = c(0, 40),
    las = 2
  )
  mtext("Delivery group", side = 1, line = 6)
  dev.off()

  cat(sprintf("[COMPARE] Single-seller n = %d; Multi-seller n = %d\n", n_single, n_multi))
  cat(sprintf("[COMPARE] Selected test: %s\n", test_name))
  cat(sprintf("[COMPARE] p-value = %.4e\n", p_value))

  list(
    n_single = n_single,
    n_multi = n_multi,
    mean_single = group_summary$mean_days[1],
    median_single = group_summary$median_days[1],
    sd_single = group_summary$sd_days[1],
    iqr_single = group_summary$iqr_days[1],
    mean_multi = group_summary$mean_days[2],
    median_multi = group_summary$median_days[2],
    sd_multi = group_summary$sd_days[2],
    iqr_multi = group_summary$iqr_days[2],
    test_name = test_name,
    test_method = test_choice$method,
    selection_reason = test_choice$selection_reason,
    estimate_label = estimate_label,
    estimated_difference = est_diff,
    conf_int = conf_int,
    test_statistic = test_statistic,
    p_value = p_value,
    faster_group = faster_group,
    median_difference = median_diff,
    practical_note = practical_note,
    cannot_conclude = cannot_conclude,
    excluded = excluded,
    group_summary = group_summary,
    boxplot_path = file.path(plot_dir, "delivery_comparison_boxplot.png")
  )
}

if (sys.nframe() == 0L) {
  invisible(run_compare_deliveries())
}
