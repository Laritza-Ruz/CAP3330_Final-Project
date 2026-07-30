# Data Quality Audit

## Purpose

Programmatically audit processed and raw Olist datasets for missing values, duplicate keys, date inconsistencies, monetary anomalies, outliers, and relationship integrity before analysis or reporting.

## When to Use

Use this skill before modeling, before rendering validation pages, and whenever operational metrics look unexpectedly distorted.

## Required Inputs

* `data/processed/order_level_analysis.rds`
* `data/processed/seller_order_analysis.rds`
* Raw Olist CSV files in `data/raw/` when source-table verification is required

## Files Used

* `R/data_quality_audit.R`
* `R/validation_helpers.R`
* `validation-limitations.qmd`
* `output/data_quality/data_quality_issue_summary.csv`
* `output/data_quality/missing_value_summary.csv`
* `output/data_quality/duplicate_key_summary.csv`
* `output/data_quality/date_consistency_summary.csv`
* `output/data_quality/monetary_quality_summary.csv`
* `output/data_quality/outlier_summary.csv`
* `output/data_quality/relationship_summary.csv`
* `output/data_quality/data_quality_overview.csv`
* `output/data_quality/missing_delivery_dates_by_status.png`

## Method

* Measure dataset dimensions and missingness
* Check duplicate keys in processed and raw tables
* Audit date, monetary, relationship, outlier, and time-series consistency rules
* Classify issues as Information, Warning, or High
* Save structured summaries without modifying processed data

## Procedure

1. Run `source("R/data_quality_audit.R")`.
2. Execute `res <- run_data_quality_audit()`.
3. Review issue summary, duplicate keys, missing dates, monetary checks, outliers, and relationships.
4. Confirm outputs saved under `output/data_quality/`.
5. Render `validation-limitations.qmd`.
6. Stop with a validation error if processed duplicate keys are found or if issue counts exceed dataset sizes.

## Validation

* Confirm `order_id` uniqueness in order-level data
* Confirm `seller_id + order_id` uniqueness in seller-order data
* Ensure percentages are finite and correctly calculated
* Ensure no required output contains infinite values
* Do not modify processed datasets during audit

## Output

Return issue summary table, duplicate-key results, missing-data findings, date/monetary/outlier/relationship summaries, overview metrics, and visualization path.

## Interpretation

* **High:** logical inconsistencies requiring action before metric reporting
* **Warning:** review recommended, context-dependent severity
* **Information:** expected patterns or monitoring items
* **Missing delivery dates (~2.98%)** are often expected for non-delivered orders

## Limitation

The audit detects structural and logical problems but cannot verify real-world delivery accuracy, customer satisfaction, or business-process correctness for payment differences caused by vouchers or discounts.
