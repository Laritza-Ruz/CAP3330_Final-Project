# Monthly Sales Trend Analysis

## Purpose

Summarize monthly completed-order volume and total payment value, compute month-over-month changes, and identify peak and low months after excluding incomplete boundary periods.

## When to Use

Use this skill for monthly operational reporting, staffing forecasts, and revenue trend reviews.

## Required Inputs

* `data/processed/order_level_analysis.rds`
* Completed orders with valid `purchase_date`, `purchase_month`, and nonnegative `total_payment_value`

## Files Used

* `R/trend_analysis.R`
* `R/validation_helpers.R`
* `additional-skills.qmd`
* `output/trend/monthly_trend_summary.csv`
* `output/trend/trend_key_results.csv`
* `output/trend/monthly_order_trend.png`
* `output/trend/monthly_payment_trend.png`

## Method

* Aggregate by `purchase_month`
* Compute completed order count, total/average/median payment, and month-over-month percentage changes
* Inspect first and last months for incomplete tracking windows
* Exclude incomplete boundary and low-volume months from peak/low and MoM headline comparisons
* Plot monthly order and payment trends

## Procedure

1. Run `source("R/trend_analysis.R")`.
2. Execute `res <- run_trend_analysis("data/processed/order_level_analysis.rds")`.
3. Review date range, exclusions, peak/low months, and largest MoM increase/decrease.
4. Confirm outputs saved under `output/trend/`.
5. Render the Monthly Sales Trend section in `additional-skills.qmd`.
6. Stop with a warning if fewer than three distinct months remain after filtering.

## Validation

* Required variables must exist
* `purchase_month` must be valid
* Payment values must be numeric and nonnegative
* No duplicate month rows after aggregation
* MoM calculations must avoid division by zero
* Negative payment values must trigger warnings

## Output

Return date range, completed orders analyzed, total payment analyzed, monthly summary table, peak/low months, largest MoM changes, exclusion notes, and plot paths.

## Interpretation

* **Peak month:** highest complete-month order or payment volume
* **MoM increase/decrease:** short-term growth or contraction
* **November 2017 peak:** may reflect seasonal demand, but the dataset does not prove cause

## Limitation

Incomplete months (`2016-09`, `2016-12`) and missing `2016-11` must be excluded or annotated. The dataset covers a limited historical window, so long-run seasonality cannot be fully established.
