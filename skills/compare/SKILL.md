# Compare Single-Seller and Multi-Seller Orders

## Purpose

Determine whether multi-seller orders take longer to deliver than single-seller orders by comparing `actual_delivery_days` across the two seller-count groups.

## When to Use

Use this skill when evaluating fulfillment complexity, seller coordination efficiency, or delivery SLAs for orders fulfilled by one versus multiple sellers.

## Required Inputs

* Processed order-level dataset: `data/processed/order_level_analysis.rds`
* Valid rows with non-missing `seller_count`, non-missing non-negative `actual_delivery_days`, delivered status, and no canceled/unavailable status

## Files Used

* `data/processed/order_level_analysis.rds`
* `R/compare_deliveries.R`
* `R/validation_helpers.R`
* `output/compare_group_summary.csv`
* `output/compare_exclusion_counts.csv`
* `images/delivery_comparison_boxplot.png`

## Method

* Filter to analysis-ready delivered orders with valid seller counts and delivery durations.
* Label groups as **Single-seller order** and **Multi-seller order** using `single_seller_order`.
* Compute sample size, mean, median, SD, and IQR for each group.
* Inspect skewness and outlier rates to choose either a **Welch t-test** or **Wilcoxon rank-sum test**.
* Report estimated difference, 95% confidence interval when supported, test statistic, p-value, and a boxplot comparison.

## Procedure

1. Run `source("R/compare_deliveries.R")`.
2. Execute `res <- run_compare_deliveries("data/processed/order_level_analysis.rds")`.
3. Confirm both `single_seller_order` and `actual_delivery_days` exist and pass validation.
4. Review exclusion counts, group summaries, selected test, and saved outputs.
5. If either group has fewer than 10 observations, stop with a validation error.
6. If invalid negative delivery durations are encountered during filtering, exclude them and report the exclusion count.
7. Do not rebuild the processed dataset unless a confirmed schema error is found.

## Validation

* Both required variables must exist.
* Outcome variable must be numeric.
* Grouping variable must produce exactly two valid groups: `0` = multi-seller, `1` = single-seller.
* Both groups must contain enough observations for testing.
* Missing values and invalid delivery durations must be excluded consistently with documented counts.
* Do not choose a test only because it yields significance; document why the selected test is defensible.

## Output

Return a list containing:

* sample size, mean, median, SD, and IQR for each group
* selected test name and selection reason
* estimated difference and 95% confidence interval when available
* test statistic and p-value
* faster group statement, practical meaningfulness note, and non-causal disclaimer
* saved comparison table, exclusion table, and boxplot path

Stop after returning these outputs. Do not run predictive modeling or additional skills in the same step.

## Interpretation

* **p-value below 0.05:** evidence of a statistically detectable group difference in the metric targeted by the selected test.
* **Median difference:** practical delivery-time gap between groups.
* **Wilcoxon result:** estimates a location shift between delivery-time distributions; do not describe it as a difference in means.
* **Welch result:** estimates a difference in mean delivery times.

## Limitation

Seller count may be associated with order complexity, product mix, shipping distance, or seller geography. This observational comparison cannot prove that multiple sellers directly cause longer delivery. Order-level delivery time also aggregates seller-level effects when multiple merchants participate in one order.
