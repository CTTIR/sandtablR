# sandtablR <img src="man/figures/logo.png" align="right" height="139" alt="sandtablR logo" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/cttir/sandtablR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cttir/sandtablR/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/license-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

A framework for mass-casualty incident tabletop **exercise simulation**,
**decision-path analysis** and **machine learning**, with an interactive
**Shiny workbench**.

Generate a casualty cohort, run it across a board, capture the decisions that
were made, then model and explain them.

## Installation

```r
# install.packages("remotes")
remotes::install_github("cttir/sandtablR")
```

Only a handful of packages are required. Every model engine, explainer and
sequence backend is optional and guarded at the point of use, so the package
installs and checks with none of them present.

## Quick start

```r
library(sandtablR)

co  <- sdt_synthetic_cohort(n = 40, seed = 1)
sc  <- sdt_scenario(co, resources = list(or = 3, icu_imc = 6, ward = 20))
run <- sdt_simulate(sc, replications = 5, seed = 1)

summary(run, reference = TRUE)   # end of game, against the published values
sdt_decision_paths(run)          # full pathway per casualty per replication
sdt_decision_consistency(run)    # how repeatably each casualty was handled

labelled <- sdt_add_decisions(co, seed = 2)   # simulated participant calls
board <- sdt_workflowset(labelled, target = "called_triage")

sdt_run_app()   # the interactive workbench
```

## What is in it

**A data model.** A casualty is a patient card; an ordered set of them is a
cohort; a cohort plus board capacities is a scenario. Cohort validation is
vectorised over columns, so a bad import names every offending row and column
at once instead of aborting on the first.

**A simulation engine.** A discrete-event model over the board, routing each
casualty by sighting category and therapy concept. It records a per-area event
log, which is what makes full decision paths available downstream. The default
horizon and board capacities were calibrated so the end-of-game distribution
reproduces the published one; `sdt_reference_scenario()` builds that board, and
the test suite re-runs the comparison rather than asserting it.

**Decision-path analysis.** Pathways as state sequences, with optimal-matching
distances, clustering into characteristic routes, and consistency scored as
distance between a casualty's replications rather than agreement on one label.

**A model bench.** One interface over interpretable engines (`glmnet`, a
decision tree) and black-box ones (`ranger`, `xgboost`), all trained through
the same recipe and scored on the same held-out split, so the price of
interpretability is a number rather than an opinion.

**Explainability.** Permutation importance, dependence profiles and
per-casualty SHAP attributions via DALEX, all model-agnostic, so a tree and a
forest can be compared on one footing.

**A workbench.** Seven panels covering the whole workflow, plus a one-click
self-contained HTML report of the session. The app's modules are ordinary
package functions in `R/`, not scripts under `inst/`, so they are checked,
linted, covered and driven headlessly by the test suite.

## A note on what to predict

The card's true sighting category follows from life points by construction, so
every engine scores near 1 on it and a leaderboard says nothing. What is worth
modelling is what a *participant* called. `sdt_add_decisions()` generates those
calls using the published per-category hit rates (88% red, 59% yellow, 71%
green), which turns the bench into a real comparison. See
`vignette("model-bench")`.

## Authors

Raban Heller and Gerhard Achatz.

## Provenance

The domain model follows the TDSC tabletop exercise (Achatz et al. 2023,
*Eur J Trauma Emerg Surg* 49(2):607–617,
[doi:10.1007/s00068-022-02217-1](https://doi.org/10.1007/s00068-022-02217-1)).
`sandtablR` is a generic re-implementation built from the published
specification and general knowledge. It is not TDSC course software and
reproduces none of the course's materials. G. Achatz is a developer of the
TDSC course and an author of the evaluation the package calibrates against.

`sdt_reference()` keeps the values attributed to that publication separate
from `sandtablR`'s own modelling choices, so a calibration test cannot quietly
check the model against parameters we picked ourselves.
