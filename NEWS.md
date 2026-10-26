# sandtablR 0.0.0.9000

First development version.

## Check and portability fixes

* The documentation and NAMESPACE are regenerated. The transformer engine
  (`sdt_transformer()` and its fit and predict plumbing) is now documented,
  and `sdt_read_gold_standard()`, `sdt_apply_gold_standard()`,
  `sdt_triage_accuracy()`, `sdt_fit_triage_error()`, `sdt_simulate_triage()`
  and `sdt_error_fidelity()` are exported, as their documentation already
  declared.

## Realignment with the source publication

The reference distributions were re-read against the open-access paper and
extended from four pinned quantities to the full published set. Several
package defaults that contradicted the patient card reproduced in the source's
Fig. 1 were corrected. These are breaking changes; the package is pre-1.0 and
the previous values were never published ones.

* **Life points now run 1–6**, the scale printed on the version 2.0 patient
  card, rather than 1–12. Band boundaries move with them.
* `sdt_therapy_levels()` gains `"none"`, the card's "no operation" option. A
  casualty carrying it is never routed to theatre.
* New `sdt_diagnostics()` vocabulary: `"efast"`, `"xray"`, `"ct"`. X-ray was
  absent, though the source reports its share.
* `sdt_board_areas()` gains `"green_area"`, the board's green treatment area.
  Green casualties are cared for there rather than being sent to a ward bed.
* `sdt_reference()` gains `version`, serving both evaluated versions of the
  exercise, and a `design` part carrying the exercise as specified: 90 minutes,
  20 patient cards, four card rounds, the team roles, and the nine-area board
  with its layout.
* `summary.game_run()` and `sdt_plot_end_state()` gain `version`, and now fold
  the board onto the **eight** locations the source reports rather than
  comparing three and lumping the rest. The two foldings that are modelling
  choices rather than published facts are recorded in
  `sdt_reference("defaults")$end_state_map`.
* Casualties arrive in four tranches at fixed points in the game, as the game
  leader plays the cards, rather than as a smooth stream. A smooth stream
  empties the screening area long before the horizon and cannot reproduce the
  published share standing there at the end.
* The engine was recalibrated against all eight published locations by
  `data-raw/calibrate.R`, a re-runnable parallel search. The previous
  calibration matched three locations while holding roughly twice the
  published share of casualties in the emergency department; the error was
  inside the cell that absorbed it.

### Bugs this surfaced

* `summary.game_run()` dropped casualties that generated no events, shrinking
  the denominator and making the source's "not yet seen" category unreachable.
  Every casualty is now accounted for in every replication.
* `sdt_decision_paths()` dropped the same casualties. They now carry a
  single-step `"not_seen"` path.
* A casualty queueing for a screening slot recorded no event, so it counted as
  never seen. The screening area is now entered on arrival, before the seize,
  which is also what makes the recorded time there include the wait.
* `sdt_simulate(horizon = Inf)` produced `NaN` arrival times.
* A cohort smaller than the number of arrival tranches aborted.

## The board, drawn

* New `sdt_plot_board()` draws the nine board areas laid out as in the
  source's Fig. 1, with every casualty as a point in the area it had reached
  at a given moment. Pass `interactive = TRUE` for \pkg{ggiraph} tooltips
  carrying the patient card.

## The data model

* `sdt_patient_card()`, `sdt_cohort()` and `sdt_scenario()` form the data
  model: a casualty, an ordered set of them, and a board to run them across.
* `sdt_cohort()` returns a validated tibble subclass. Validation is vectorised
  over columns, so a bad import reports every offending row and column at once
  instead of aborting on the first.
* Controlled vocabularies for sighting category, therapy concept, board area,
  injury region and sex, all from one source of truth.
* `format()` methods for `patient_card`, `scenario`, `game_run` and
  `sdt_model`, with `print()` delegating to them so the two cannot drift.
* `as_tibble()` and `autoplot()` methods for a scenario;
  `sdt_as_patient_cards()` for the reverse direction.

## Reference distributions

* `sdt_reference()` returns the marginals the package calibrates against,
  loaded lazily from `inst/extdata` and cached.
* The list deliberately separates `published` (attributed to Achatz et al.
  2023) from `defaults` (modelling choices made where the source pins no
  value). A calibration test that mixed the two would be checking the model
  against its own assumptions.
* `sdt_reference_scenario()` and `sdt_reference_horizon()` build the board the
  engine was calibrated on, so the calibration claim can be re-run.

## Synthetic data

* `sdt_synthetic_cohort()` gains a **simstudy** backend drawing correlated
  age, injury severity and life points, with the sighting category derived
  from acuity by rule. The base backend is the always-available reference
  implementation and is pinned by a golden test at seed 1.
* `sdt_synthesise()` produces a releasable replica of a real seed cohort with
  **synthpop**'s sequential CART models. Identifiers are regenerated rather
  than synthesised, since they carry row identity rather than structure.
* `sdt_synthesis_utility()` scores a replica against its seed per variable, so
  it can be judged before release.
* `sdt_add_decisions()` simulates the category a participant assigns, using
  the published per-category hit rates, with miscalls weighted toward
  neighbouring categories.

## Simulation

* `sdt_simulate()` routes casualties by sighting category and therapy concept
  rather than sending everyone through theatre, with diagnostics, theatre and
  critical-care probabilities from the reference defaults.
* It records a **per-area event log** built from monitored entry attributes.
  Release timestamps would have misplaced every casualty still being treated
  when the clock stopped, which is precisely the population the end-of-game
  distribution is about.
* `events` carries both `duration` and `service_time`. They differ whenever a
  casualty is finished but waiting for a bed downstream, and conflating them
  made theatre times look like whatever the downstream bottleneck was.
* Three dispositions are terminal, and that is what reproduces the published
  end state: critical care is where a casualty stays, a ward bed is the end of
  the line, and a casualty worked up but not taken to theatre is still held in
  the emergency department. Routing them onward put roughly 70% of every
  cohort in the ward against a published 35%.
* `summary()` on a `game_run` reports the end-of-game location distribution,
  optionally beside the published reference.
* Replications can run concurrently via **future.apply** behind
  `parallel = FALSE`.

## Decision paths

* `sdt_decision_paths()` returns the full ordered pathway per casualty per
  replication, in long or state-sequence-wide form.
* `sdt_decision_consistency()` is rebuilt on distance rather than modal
  agreement. The previous measure compared each casualty's single recorded
  area with itself and was therefore always exactly 1. It now offers mean
  pairwise edit distance across replications, or normalised longitudinal
  entropy. A single path scores `NA`, not a perfect 1: there is nothing to
  compare it with.
* `sdt_seqdef()` bridges to **TraMineR** over a fixed board-area alphabet, so
  runs stay comparable; `sdt_seq_cluster()` groups pathways by
  optimal-matching distance and partitioning around medoids.
* `sdt_plot_sequences()` draws index and state-distribution views in
  **ggplot2** rather than base graphics, so they theme like every other plot.

## Machine learning

* `sdt_fit()` is a full **tidymodels** path: preprocessing recipe, stratified
  resamples, tuning grid, and a final fit on a held-out split. The split and
  resamples are kept, so reported numbers can be traced to what produced them.
* `sdt_workflowset()` screens every installed engine onto one leaderboard and
  names what it skipped.
* New `sdt_engines()`, `sdt_metric_set()`, `sdt_metrics()`,
  `sdt_predictions()`, `sdt_roc()` and `sdt_consistency_score()`.
* Models can target the participant's call as well as the card's true value.
  The true category follows from life points by construction, so every engine
  scores near 1 on it and the glass-box versus black-box comparison is empty.

## Explainability

* `sdt_importance()`, `sdt_profile()` and `sdt_shap()` wrap **DALEX**, all
  returning **ggplot2** objects and all degrading with an actionable message
  when DALEX is absent.

## The workbench

* `sdt_run_app()` launches a six-panel workbench: scenario, simulate,
  annotate, model lab, explain and sequences, plus a one-click self-contained
  HTML report of the session.
* The app's modules are ordinary package functions in `R/` rather than scripts
  under `inst/`. Code under `inst/` is invisible to `R CMD check`, lintr and
  covr, so an app kept there is the one part of a package nothing verifies.
  Every module is now driven headlessly by `shiny::testServer()`.
* Annotation edits are validated against the vocabularies and written back
  into the shared scenario; an invalid edit is refused and explained.
* Every optional package is guarded, and each panel falls back to a static
  plot or a plain table rather than failing.

## Fixes to the initial scaffold

* `sdt_simulate()` passed `at =` to `simmer::add_generator()` as a named
  argument rather than as `at()`, so no simulation ran at all.
* Two `cli` format strings would have aborted at format time: one pluralising
  with no quantity, one interpolating a dot-prefixed expression that cli reads
  as a style name. Both sat on error paths, where they would not have been
  noticed. Static tests now guard both patterns, along with line length,
  `library()` calls and bare `T`/`F`.
* `ventilated` is logical, and recipes counts logicals as neither nominal nor
  numeric, so it slipped past both selectors and reached glmnet and xgboost as
  a column they could not put in a numeric matrix.
* `tune()` markers must appear literally in a model specification: passed
  through a helper they evaluate to the same object, but parsnip inspects the
  expression, so the parameter set came back empty and the placeholder reached
  the engine.
* `DALEX::loss_cross_entropy()` looks the observed class up by name in the
  probability matrix, and tidymodels names those columns `.pred_red`; every
  lookup was out of bounds until the prefix was stripped.
* Annotating a row updated the scenario, which fired the observer that reset
  the annotation count, so the "rows annotated" badge always read zero.
