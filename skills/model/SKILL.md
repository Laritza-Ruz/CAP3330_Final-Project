# Predict Late Delivery

## Purpose

Train and evaluate a logistic regression model that estimates late-delivery risk from order-level operational, payment, and timing variables.

## When to Use

Use this skill when screening orders for delivery risk, reviewing predictor associations with lateness, or preparing operational monitoring rules. Do not use it as an automatic penalty or cancellation engine without human review.

## Required Inputs

* `data/processed/order_level_analysis.rds`
* Delivered orders with valid estimated and actual delivery dates
* Nonmissing `late_delivery` outcome
* Complete predictor values after documented exclusions

## Files Used

* `R/model_low_reviews.R`
* `R/validation_helpers.R`
* `model.qmd`
* `output/model/model_metrics.csv`
* `output/model/confusion_matrix.csv`
* `output/model/coefficient_table.csv`
* `output/model/odds_ratios.csv`
* `output/model/prediction_summary.csv`
* `output/model/roc_curve.csv`
* `output/model/late_delivery_glm.rds`

## Method

* Binary outcome: `late_delivery`
* Predictors: monetary log1p transforms when justified, item count, seller count, estimated delivery days, installments, purchase month, payment type
* Stratified 80/20 train/test split with fixed seed
* `glm(..., family = binomial)`
* Training-only threshold selection when class imbalance makes 0.50 ineffective
* Test-set confusion matrix, accuracy, sensitivity, specificity, precision, F1, ROC AUC

## Procedure

1. Run `source("R/model_low_reviews.R")`.
2. Execute `res <- train_predictive_model("data/processed/order_level_analysis.rds")`.
3. Confirm analysis population filters, class balance, and threshold reason.
4. Review test metrics, odds ratios, and saved files in `output/model/`.
5. Render `model.qmd` for the website page.
6. Stop and return a warning if outcome does not contain two classes or if required predictors are missing.

## Validation

* Required variables must exist
* Outcome must contain exactly two classes
* Predictors must have valid types and no infinite transformed values
* Threshold must be selected on training data only
* Test data must not be used for fitting or threshold tuning
* Warn that accuracy is misleading under class imbalance and precision is low

## Output

Return model formula, train/test sizes, threshold, confusion matrix, performance metrics, odds-ratio table, top predictors, and output file paths.

## Interpretation

* **Higher predicted probability:** higher estimated late-delivery risk
* **Sensitivity:** share of late orders flagged
* **Precision:** share of flagged orders that are truly late
* **Odds ratios:** associative effects; log-transformed monetary terms are not one-real increases
* **Association does not prove causation**

## Limitation

Late deliveries are uncommon, so many flagged orders will be false alarms. The model excludes canceled and undelivered orders and omits spatial routing detail. It is a risk-screening tool, not proof of causal delay drivers.
