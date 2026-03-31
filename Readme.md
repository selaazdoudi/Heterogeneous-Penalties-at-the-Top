# Heterogeneous Penalties at the Top

Replication and extension of Bach et al. (2018) on the U.S. residual gender wage gap.

**Authors:** Salma El Aazdoudi · Rafael Mourouvin · Maria Micaela Linares Gomez from Ecole Polytechnique (X-ENSAE)

---

## Overview

This repository contains the code and materials for the paper **Heterogeneous Penalties at the Top: Glass Ceilings, Hours Convexity, and Motherhood Selection Among Highly Educated Women in the U.S.**

The project starts froms the main findings of Bach et al. (2018) and extends them by studying three structural mechanisms behind wage penalties among highly educated White non-Hispanic women.

---

## Main Findings

- **Glass ceiling** — The wage penalty concentrates at the top of the earnings distribution, with the top decile facing an average gap of 11.79%
- **Hours convexity** — Labour markets reward extreme hours convexly, but women working 70+ hours face an additional gender penalty of ~20.9% that wipes out the market bonus entirely
- **Motherhood selection** — A small motherhood wage premium of 2–5% among full-time workers reflects positive selection into employment, not a causal return to motherhood

---

## Repository Structure

```
data/                   Dataset
Replication code/       Replication code
Extension code/
├── Extension 1/        Heterogeneity and the Glass Ceiling (Causal Forest)
├── Extension 2/        The Flexibility Price and Convex Returns to Hours (DML-PLR)
└── Extension 3/        Motherhood Premium or Selection Artefact? (DML-IRM)
figures/                Figures used in the paper and presentation
tables/                 Regression tables and outputs
presentation/           Slides and presentation materials
report/                 Main paper and appendix
```

---

## Methods

| Method | Used in |
|--------|---------|
| Double Post-Lasso | Replication + Extension 1 |
| Causal Forest | Extension 1 — Glass Ceiling |
| Double Machine Learning — Partially Linear Regression | Extension 2 — Hours Convexity |
| DML Interactive Regression Model (DML-IRM) | Extension 3 — Motherhood |
| Propensity Score Weighting | Extension 3 — Motherhood |
| Cross-Fitting | Extensions 2 & 3 |

---

## Data

**2016 American Community Survey (ACS)**, restricted to full-time, year-round employed individuals aged 25 to 65.

---

## Reference

Bach, P., Chernozhukov, V., Kurz, M. S., & Spindler, M. (2018). *Closing the U.S. Gender Wage Gap Requires Understanding Its Heterogeneity*. arXiv preprint.
