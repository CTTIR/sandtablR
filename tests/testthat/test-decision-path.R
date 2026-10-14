skip_if_not_installed("simmer")

paths_run <- function(n = 25, replications = 4) {
  co <- sdt_synthetic_cohort(n = n, seed = 1)
  sc <- sdt_scenario(co, resources = list(or = 2, icu_imc = 5, ward = 20))
  sdt_simulate(sc, replications = replications, seed = 1, verbose = FALSE)
}

test_that("sdt_decision_paths() returns the long event sequence", {
  run <- paths_run()
  long <- sdt_decision_paths(run)
  expect_s3_class(long, "tbl_df")
  expect_named(
    long,
    c("replication", "id", "step", "area", "start", "end", "duration")
  )
  expect_true(all(long$area %in% sdt_board_areas()))
})

test_that("decision paths are full sequences, not a single state", {
  # The whole point of rebuilding this on the event log: a red casualty
  # passes through several areas, and the path has to show all of them.
  run <- paths_run()
  long <- sdt_decision_paths(run)
  per_case <- table(paste(long$replication, long$id))
  expect_gt(max(per_case), 3L)
  # Everyone who reached the board starts at screening. A casualty whose card
  # the game never got to carries the single state "not_seen" instead.
  expect_setequal(unique(long$area[long$step == 1L]),
                  intersect(c("triage", "not_seen"),
                            unique(long$area[long$step == 1L])))
  expect_true(all(long$area[long$step == 1L] %in% c("triage", "not_seen")))
})

test_that("sdt_decision_paths(format = 'wide') pads to a rectangle", {
  run <- paths_run()
  wide <- sdt_decision_paths(run, format = "wide")
  step_cols <- grep("^step_", names(wide), value = TRUE)
  expect_gt(length(step_cols), 1L)
  expect_identical(step_cols, paste0("step_", seq_along(step_cols)))
  expect_true(all(wide$step_1 %in% c("triage", "not_seen")))
  # Shorter paths are padded with NA, never recycled.
  expect_true(anyNA(wide[[utils::tail(step_cols, 1L)]]))
})

test_that("wide and long describe the same paths", {
  run <- paths_run(n = 10, replications = 2)
  long <- sdt_decision_paths(run)
  wide <- sdt_decision_paths(run, format = "wide")
  step_cols <- grep("^step_", names(wide), value = TRUE)
  for (i in seq_len(nrow(wide))) {
    hit <- long$replication == wide$replication[[i]] & long$id == wide$id[[i]]
    from_long <- long$area[hit][order(long$step[hit])]
    from_wide <- unlist(wide[i, step_cols], use.names = FALSE)
    expect_identical(from_long, from_wide[!is.na(from_wide)])
  }
})

test_that("sdt_decision_paths() rejects a bad format", {
  expect_error(sdt_decision_paths(paths_run(n = 5, replications = 1), "sideways"))
})

test_that("an empty run yields empty paths of the right shape", {
  co <- sdt_cohort(sdt_synthetic_cohort(n = 3, seed = 1)[0, ])
  run <- sdt_simulate(sdt_scenario(co), replications = 1, verbose = FALSE)
  expect_identical(nrow(sdt_decision_paths(run)), 0L)
  expect_identical(nrow(sdt_decision_paths(run, format = "wide")), 0L)
})


# Consistency -----------------------------------------------------------------

test_that("sdt_decision_consistency() returns one row per casualty", {
  run <- paths_run()
  out <- sdt_decision_consistency(run)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("id", "n_paths", "n_distinct", "consistency"))
  expect_setequal(out$id, run$scenario$cohort$id)
  expect_identical(nrow(out), nrow(run$scenario$cohort))
  expect_true(all(out$n_paths == 4L))
})

test_that("consistency is 1 exactly when every path is identical", {
  run <- paths_run()
  out <- sdt_decision_consistency(run)
  same <- out$n_distinct == 1L
  skip_if(!any(same), "no casualty was routed identically every time")
  expect_true(all(out$consistency[same] == 1))
  if (any(!same)) {
    expect_true(all(out$consistency[!same] < 1))
  }
})

test_that("consistency scores lie in the unit interval", {
  out <- sdt_decision_consistency(paths_run())
  expect_true(all(out$consistency >= 0 & out$consistency <= 1))
  ent <- sdt_decision_consistency(paths_run(), method = "entropy")
  expect_true(all(ent$consistency >= 0 & ent$consistency <= 1))
})

test_that("a single replication leaves OM consistency undefined", {
  # There is nothing to compare a lone path with, and reporting that as
  # perfect agreement would be a lie.
  out <- sdt_decision_consistency(paths_run(n = 10, replications = 1))
  expect_true(all(is.na(out$consistency)))
  expect_true(all(out$n_paths == 1L))
})

test_that("entropy consistency works with a single replication", {
  out <- sdt_decision_consistency(paths_run(n = 10, replications = 1),
                                  method = "entropy")
  expect_false(any(is.na(out$consistency)))
})

test_that("a casualty parked in one area is maximally consistent by entropy", {
  co <- sdt_synthetic_cohort(n = 20, seed = 2)
  co$triage <- "green"
  sc <- sdt_scenario(co, resources = list(ward = 20, triage = 20))
  run <- sdt_simulate(sc, replications = 2, seed = 1, verbose = FALSE)
  out <- sdt_decision_consistency(run, method = "entropy")
  # Greens go triage -> ward and then sit there, so nearly all their time is
  # in one state and entropy is close to zero.
  expect_gt(mean(out$consistency), 0.7)
})

test_that("sdt_decision_consistency() handles an empty run", {
  co <- sdt_cohort(sdt_synthetic_cohort(n = 3, seed = 1)[0, ])
  run <- sdt_simulate(sdt_scenario(co), replications = 2, verbose = FALSE)
  for (m in c("om", "entropy")) {
    out <- sdt_decision_consistency(run, method = m)
    expect_identical(nrow(out), 0L)
    expect_named(out, c("id", "n_paths", "n_distinct", "consistency"))
  }
})

test_that("sdt_decision_consistency() rejects an unknown method", {
  expect_error(sdt_decision_consistency(paths_run(n = 5, replications = 2),
                                        method = "vibes"))
})

test_that(".mean_pairwise_distance() behaves at the edges", {
  expect_true(is.na(.mean_pairwise_distance("ABC")))
  expect_identical(.mean_pairwise_distance(c("ABC", "ABC")), 0)
  expect_equal(.mean_pairwise_distance(c("ABC", "ABD")), 1 / 3)
})


# TraMineR bridge -------------------------------------------------------------

test_that("sdt_seqdef() builds a state-sequence object over the full alphabet", {
  skip_if_not_installed("TraMineR")
  run <- paths_run()
  seqs <- sdt_seqdef(run)
  expect_s3_class(seqs, "stslist")
  expect_identical(nrow(seqs), nrow(sdt_decision_paths(run, format = "wide")))
  # Fixed alphabet, so runs stay comparable even if an area is never visited.
  expect_setequal(TraMineR::alphabet(seqs), sdt_board_areas())
  expect_length(attr(seqs, "casualty"), nrow(seqs))
})

test_that("sdt_seqdef() can restrict to one replication", {
  skip_if_not_installed("TraMineR")
  run <- paths_run()
  seqs <- sdt_seqdef(run, replication = 2L)
  expect_true(all(attr(seqs, "replication") == 2L))
  expect_identical(nrow(seqs), nrow(run$scenario$cohort))
})

test_that("sdt_seqdef() errors when there is nothing to convert", {
  skip_if_not_installed("TraMineR")
  co <- sdt_cohort(sdt_synthetic_cohort(n = 3, seed = 1)[0, ])
  run <- sdt_simulate(sdt_scenario(co), replications = 1, verbose = FALSE)
  expect_error(sdt_seqdef(run), "No decision paths")
})

test_that("sdt_seq_cluster() assigns every sequence to a cluster", {
  skip_if_not_installed("TraMineR")
  skip_if_not_installed("WeightedCluster")
  run <- paths_run()
  cl <- sdt_seq_cluster(run, k = 3L)
  expect_named(cl, c("replication", "id", "cluster", "medoid"))
  expect_identical(nrow(cl), nrow(sdt_decision_paths(run, format = "wide")))
  expect_setequal(sort(unique(cl$cluster)), 1:3)
  expect_identical(sum(cl$medoid), 3L)
})

test_that("sdt_seq_cluster() refuses more clusters than sequences", {
  skip_if_not_installed("TraMineR")
  skip_if_not_installed("WeightedCluster")
  run <- paths_run(n = 4, replications = 1)
  expect_error(sdt_seq_cluster(run, k = 99L), "must not exceed")
})


# Plots -----------------------------------------------------------------------

test_that("sequence plots return ggplot objects", {
  run <- paths_run()
  expect_s3_class(sdt_plot_sequences(run), "ggplot")
  expect_s3_class(sdt_plot_sequences(run, type = "distribution"), "ggplot")
  expect_s3_class(sdt_plot_sequences(run, replication = 1L), "ggplot")
})

test_that("sequence plots facet by cluster when given one", {
  skip_if_not_installed("TraMineR")
  skip_if_not_installed("WeightedCluster")
  run <- paths_run()
  cl <- sdt_seq_cluster(run, k = 2L)
  p <- sdt_plot_sequences(run, clusters = cl)
  expect_s3_class(p, "ggplot")
})

test_that("sequence plots degrade gracefully on an empty run", {
  co <- sdt_cohort(sdt_synthetic_cohort(n = 3, seed = 1)[0, ])
  run <- sdt_simulate(sdt_scenario(co), replications = 1, verbose = FALSE)
  expect_s3_class(sdt_plot_sequences(run), "ggplot")
})
