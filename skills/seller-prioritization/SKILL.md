# Seller Prioritization

## Purpose

Rank sellers for operational review using late-delivery performance, cancellation/unavailable exposure, positive delivery delay, and order-volume exposure at the seller-order grain.

## When to Use

Use this skill when deciding which sellers to audit first for fulfillment performance or cancellation risk.

## Required Inputs

* `data/processed/seller_order_analysis.rds`
* Optional: `data/processed/order_level_analysis.rds` for multi-seller documentation
* Minimum thresholds: 20 seller-order records and 20 valid delivered orders per seller

## Files Used

* `R/seller_prioritization.R`
* `R/validation_helpers.R`
* `additional-skills.qmd`
* `output/seller/seller_priority_ranking.csv`
* `output/seller/seller_priority_components.csv`
* `output/seller/seller_eligibility_summary.csv`
* `output/seller/top_seller_priority_plot.png`

## Method

* Aggregate seller-order records by `seller_id`
* Compute seller order count, valid delivery count, late delivery count, rates, median delay, freight, and item value totals
* Apply eligibility filters
* Normalize components and combine with weights:
  * 40% late-delivery rate
  * 25% canceled/unavailable rate
  * 20% positive median delivery delay
  * 15% log1p order-volume exposure
* Rank descending by `priority_score`

## Procedure

1. Run `source("R/seller_prioritization.R")`.
2. Execute `res <- run_seller_prioritization("data/processed/seller_order_analysis.rds")`.
3. Confirm eligibility counts and formula documentation.
4. Review top-ranked sellers and component values.
5. Render the Seller Prioritization section in `additional-skills.qmd`.
6. Stop with a validation error if no sellers meet both eligibility thresholds.

## Validation

* `seller_id` must exist
* `seller_id + order_id` must be unique
* Minimum seller-order and valid delivered thresholds must be enforced
* Rates must remain between 0 and 1
* No missing or infinite final scores
* No duplicate seller rows in ranking
* Highest score must correspond to highest review priority

## Output

Return eligible/excluded seller counts, ranked table, component table, eligibility summary, plot path, and multi-seller handling notes.

## Interpretation

* **Higher priority score:** review sooner
* **Late-delivery rate:** share of valid delivered seller-orders that were late
* **Volume exposure:** larger sellers can affect more customers even at moderate rates

## Limitation

Delivery outcomes are order-level and may be shared across sellers in multi-seller orders. Logistics factors outside seller control can influence the same delivery timestamp. The score indicates review priority, not proven seller-caused delays.
