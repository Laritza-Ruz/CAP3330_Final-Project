# Explore Order Performance

## Purpose

Calculate baseline descriptive statistics and visualizations for order value, freight cost, delivery time, order status, and late-delivery patterns using the processed order-level dataset.

## When to Use

Use this skill at the start of an operational review, when establishing fulfillment and pricing benchmarks, or before running comparative or predictive skills that depend on clean order-level metrics.

## Required Inputs

* Processed order-level dataset: `data/processed/order_level_analysis.rds`
* Optional override paths for output directories

## Files Used

* `data/processed/order_level_analysis.rds`
* `R/explore_orders.R`
* `R/validation_helpers.R`
* `output/explore_summary_table.csv`
* `output/explore_order_status_table.csv`
* `output/explore_missing_counts.csv`
* `images/order_value_distribution.png`
* `images/late_delivery_distribution.png`
* `images/delivery_days_distribution.png`

## Method

* Load the processed order-level dataset with one row per `order_id`.
* Validate required variables, numeric types, and minimum non-missing observations.
* Compute counts, percentages, means, medians, standard deviations, five-number summaries, late-delivery rates, missing-value counts, and unique-value counts.
* Generate at least two R base graphics: payment-value distribution and late-delivery/status distribution.
* Save summary tables to `output/` and plots to `images/`.

## Procedure

1. Run `source("R/explore_orders.R")`.
2. Execute `res <- run_explore_orders("data/processed/order_level_analysis.rds")`.
3. Confirm the script validates required variables: `total_payment_value`, `total_freight_value`, `actual_delivery_days`, `late_delivery`, `completed_order`, `canceled_or_unavailable`, and `order_status`.
4. Review returned summary values and saved files in `output/` and `images/`.
5. If validation fails or fewer than 10 valid delivered orders exist, stop and return the validation error message.
6. If negative monetary or delivery values are present, continue but surface the warning messages in the response.

## Validation

* Required variables must exist in the processed dataset.
* Monetary and delivery variables must be numeric.
* At least 10 non-missing payment values and 10 delivered orders with valid delivery dates must be available.
* Date-dependent summaries require valid purchase dates.
* Negative monetary or delivery values must trigger warnings.
* Do not rebuild the processed dataset unless a confirmed schema error is found.

## Output

Return a list containing:

* total order count and delivered/canceled counts with percentages
* payment mean, median, SD, and five-number summary
* freight mean and median
* delivery mean and median for delivered orders
* late-delivery count and rate
* missing-value and unique-value counts
* saved summary tables and plot paths

Stop after returning these outputs. Do not run comparative or predictive skills in the same step.

## Interpretation

* **Median payment value:** typical order size for SLA and promotion planning.
* **Median freight value:** typical shipping cost burden per order.
* **Late-delivery rate:** service-quality baseline among delivered orders.
* **Order status mix:** operational health snapshot across fulfillment outcomes.

## Limitation

Payment and delivery metrics are right-skewed, so means can be distorted by extreme orders. Delivery metrics exclude canceled, unavailable, and undelivered orders, which introduces survival bias toward successful shipments. Review scores are unavailable and must not be simulated.
