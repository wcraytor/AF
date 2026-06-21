# Author

**William Bert Craytor**
Contact: wbcraytor@valuation-engineer.com

## About this work

This repository presents a **comparative valuation analysis** of a single
residential subject property estimated with three independent regression
engines:

- **earthUI** — Multivariate Adaptive Regression Splines (MARS), via
  [`earth`](https://CRAN.R-project.org/package=earth)
- **glmnetUI** — elastic-net regularized regression, via
  [`glmnet`](https://CRAN.R-project.org/package=glmnet)
- **mgcvUI** — generalized additive models (GAM), via
  [`mgcv`](https://CRAN.R-project.org/package=mgcv)

All three are interactive Shiny applications I developed for regression-based
real-estate appraisal modeling, sharing a common project/coordination core
(`valengrCore`). The comparative paper (`Comparative_Analysis.html`) and the
three full per-model reports were generated from those tools.

## Data confidentiality

The models were fit to **real, unaltered MLS data**. Only the underlying
spreadsheets are withheld from this public repository, because they tie records
to specific **addresses, parcel numbers, and MLS/transaction IDs**. The
published reports and plots contain aggregate model output only — no addresses,
parcel numbers, or transaction IDs.

## Acknowledgements

Analysis assembled with assistance from Claude Code (Anthropic).
