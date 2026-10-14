test_that("sdt_synthetic_cohort() is reproducible and well-typed", {
  a <- sdt_synthetic_cohort(n = 30, seed = 7)
  b <- sdt_synthetic_cohort(n = 30, seed = 7)
  expect_identical(a, b)
  expect_s3_class(a, "cohort")
  expect_s3_class(a, "tbl_df")
  expect_equal(nrow(a), 30)
  expect_setequal(
    names(a),
    c("id", "age", "sex", "life_points", "ventilated",
      "injury_region", "triage", "therapy", "arrival_order")
  )
  expect_true(all(a$triage %in% sdt_triage_levels()))
  expect_true(all(a$therapy %in% sdt_therapy_levels()))
})

test_that("sdt_synthetic_cohort() restores the caller's random stream", {
  set.seed(99)
  before <- stats::runif(1)
  set.seed(99)
  invisible(sdt_synthetic_cohort(n = 5, seed = 1))
  expect_identical(stats::runif(1), before)
})

test_that("sdt_synthetic_cohort() validates n", {
  expect_error(sdt_synthetic_cohort(n = 0), "positive whole number")
  expect_error(sdt_synthetic_cohort(n = 2.5), "positive whole number")
})

test_that("sdt_synthetic_cohort() validates triage_prob", {
  expect_error(
    sdt_synthetic_cohort(n = 5, triage_prob = c(0.5, 0.5)),
    "named numeric vector"
  )
  expect_error(
    sdt_synthetic_cohort(n = 5, triage_prob = c(red = 0.5, purple = 0.5)),
    "sighting categories"
  )
  expect_error(
    sdt_synthetic_cohort(n = 5, triage_prob = c(red = 0, green = 0)),
    "sum to more than zero"
  )
})

test_that("triage_prob is normalised rather than required to sum to one", {
  a <- sdt_synthetic_cohort(
    n = 40, triage_prob = c(red = 2, yellow = 1, green = 1), seed = 11
  )
  b <- sdt_synthetic_cohort(
    n = 40, triage_prob = c(red = 0.5, yellow = 0.25, green = 0.25), seed = 11
  )
  expect_identical(a$triage, b$triage)
})

test_that("sdt_synthetic_cohort() rejects an unknown backend", {
  expect_error(sdt_synthetic_cohort(n = 5, backend = "magic"))
})

# The base backend is the reference implementation. Its behaviour is frozen:
# these are the exact values for seed = 1, and reordering the random draws
# inside .cohort_base() will break them. That is the point of the test.
test_that("the base backend is behaviourally frozen at seed = 1", {
  co <- sdt_synthetic_cohort(n = 10, seed = 1)
  expect_identical(
    co$age,
    c(41L, 42L, 56L, 54L, 51L, 56L, 54L, 43L, 12L, 51L)
  )
  expect_identical(
    co$sex,
    c("m", "f", "m", "m", "m", "m", "m", "m", "f", "m")
  )
  expect_identical(
    co$life_points,
    c(1L, 1L, 5L, 3L, 1L, 4L, 4L, 6L, 6L, 1L)
  )
  expect_identical(
    co$ventilated,
    c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_identical(
    co$injury_region,
    c("head", "abdomen", "abdomen", "extremity", "abdomen", "head",
      "extremity", "multiple", "head", "head")
  )
  expect_identical(
    co$triage,
    c("red", "red", "green", "yellow", "red", "yellow", "yellow",
      "green", "green", "red")
  )
  expect_identical(
    co$therapy,
    c("DCS", "TASC", "none", "TASC", "TASC", "TASC", "TASC", "none",
      "TASC", "TASC")
  )
})

test_that("life points respect the band implied by the sighting category", {
  co <- sdt_synthetic_cohort(n = 300, seed = 12)
  bands <- sdt_reference("defaults")$life_bands
  for (level in sdt_triage_levels()) {
    got <- co$life_points[co$triage == level]
    expect_true(all(got %in% bands[[level]]), info = level)
  }
})

test_that("only red casualties arrive ventilated", {
  co <- sdt_synthetic_cohort(n = 300, seed = 13)
  expect_true(all(co$triage[co$ventilated] == "red"))
})

test_that("the realised triage mix tracks the published marginals", {
  co <- sdt_synthetic_cohort(n = 4000, seed = 14)
  observed <- prop.table(table(factor(co$triage, levels = sdt_triage_levels())))
  expected <- sdt_reference("published")$triage_mix[sdt_triage_levels()]
  # Absolute, not relative: a 2-point drift on a 0.24 marginal is sampling
  # noise, but expect_equal()'s relative tolerance would call it an 8% error.
  expect_lt(max(abs(as.numeric(observed) - as.numeric(expected))), 0.03)
})

test_that("the simstudy backend produces a valid, correlated cohort", {
  skip_if_not_installed("simstudy")
  co <- sdt_synthetic_cohort(n = 400, seed = 21, backend = "simstudy")
  expect_s3_class(co, "cohort")
  expect_true("iss" %in% names(co))
  expect_true(all(co$iss >= 1L & co$iss <= 75L))
  # The correlation chain is what distinguishes this backend: severity must
  # actually predict life points, else it is the base generator with extra
  # ceremony.
  expect_lt(stats::cor(co$iss, co$life_points), -0.3)
  expect_gt(stats::cor(co$age, co$iss), 0.05)
})

test_that("the simstudy backend hits the requested triage mix", {
  skip_if_not_installed("simstudy")
  co <- sdt_synthetic_cohort(n = 100, seed = 22, backend = "simstudy")
  mix <- sdt_reference("published")$triage_mix[sdt_triage_levels()]
  observed <- table(factor(co$triage, levels = sdt_triage_levels()))
  expect_identical(sum(as.integer(observed)), 100L)
  expect_equal(as.numeric(observed) / 100, as.numeric(mix), tolerance = 0.02)
})

test_that("the simstudy backend keeps the category monotone in acuity", {
  skip_if_not_installed("simstudy")
  co <- sdt_synthetic_cohort(n = 300, seed = 23, backend = "simstudy")
  means <- tapply(co$life_points, co$triage, mean)
  expect_lt(means[["red"]], means[["yellow"]])
  expect_lt(means[["yellow"]], means[["green"]])
})

test_that("the simstudy backend is reproducible", {
  skip_if_not_installed("simstudy")
  a <- sdt_synthetic_cohort(n = 50, seed = 24, backend = "simstudy")
  b <- sdt_synthetic_cohort(n = 50, seed = 24, backend = "simstudy")
  expect_identical(a, b)
})


# Synthesis from a seed cohort ------------------------------------------------

test_that("sdt_synthesise() returns a valid cohort of the requested size", {
  skip_if_not_installed("synthpop")
  seed_cohort <- sdt_synthetic_cohort(n = 80, seed = 31)
  replica <- sdt_synthesise(seed_cohort, n = 40, seed = 32)
  expect_s3_class(replica, "cohort")
  expect_identical(nrow(replica), 40L)
  expect_invisible(validate_cohort(replica))
})

test_that("sdt_synthesise() regenerates identifiers rather than copying them", {
  skip_if_not_installed("synthpop")
  seed_cohort <- sdt_synthetic_cohort(n = 60, seed = 33)
  seed_cohort$id <- paste0("REAL-", seq_len(60))
  replica <- sdt_synthesise(seed_cohort, n = 20, seed = 34)
  expect_false(any(grepl("REAL", replica$id, fixed = TRUE)))
  expect_identical(replica$arrival_order, seq_len(20))
})

test_that("sdt_synthesise() defaults to the seed cohort's size", {
  skip_if_not_installed("synthpop")
  seed_cohort <- sdt_synthetic_cohort(n = 40, seed = 35)
  expect_identical(nrow(sdt_synthesise(seed_cohort, seed = 36)), 40L)
})

test_that("sdt_synthesise() is reproducible", {
  skip_if_not_installed("synthpop")
  seed_cohort <- sdt_synthetic_cohort(n = 50, seed = 37)
  a <- sdt_synthesise(seed_cohort, n = 25, seed = 38)
  b <- sdt_synthesise(seed_cohort, n = 25, seed = 38)
  expect_identical(a, b)
})

test_that("sdt_synthesise() rejects an empty seed cohort", {
  skip_if_not_installed("synthpop")
  empty <- sdt_synthetic_cohort(n = 3, seed = 1)[0, ]
  expect_error(sdt_synthesise(empty), "at least one row")
})

test_that("sdt_synthesis_utility() scores a replica against its seed", {
  a <- sdt_synthetic_cohort(n = 200, seed = 41)
  b <- sdt_synthetic_cohort(n = 200, seed = 42)
  u <- sdt_synthesis_utility(a, b)
  expect_s3_class(u, "tbl_df")
  expect_named(u, c("variable", "type", "statistic", "value", "flag"))
  expect_type(u$flag, "logical")
  expect_false(any(c("id", "arrival_order") %in% u$variable))
  expect_true(all(u$value >= 0))
})

test_that("sdt_synthesis_utility() scores a cohort against itself as perfect", {
  a <- sdt_synthetic_cohort(n = 120, seed = 43)
  u <- sdt_synthesis_utility(a, a)
  expect_true(all(u$value < 1e-8))
  expect_false(any(u$flag))
})

test_that("sdt_synthesis_utility() flags a divergent replica", {
  a <- sdt_synthetic_cohort(
    n = 200, triage_prob = c(red = 0.9, yellow = 0.05, green = 0.05), seed = 44
  )
  b <- sdt_synthetic_cohort(
    n = 200, triage_prob = c(red = 0.05, yellow = 0.05, green = 0.9), seed = 45
  )
  u <- sdt_synthesis_utility(a, b)
  expect_true(u$flag[u$variable == "triage"])
})

test_that("sdt_synthesis_utility() returns the empty shape when nothing is shared", {
  u <- sdt_synthesis_utility(tibble::tibble(a = 1), tibble::tibble(b = 2))
  expect_identical(nrow(u), 0L)
  expect_named(u, c("variable", "type", "statistic", "value", "flag"))
})
