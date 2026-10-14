test_that("sdt_reference() returns the parts it advertises", {
  expect_named(
    sdt_reference(),
    c("published", "versions", "design", "defaults", "meta")
  )
  expect_type(sdt_reference("published"), "list")
  expect_type(sdt_reference("design"), "list")
  expect_type(sdt_reference("defaults"), "list")
  expect_identical(sdt_reference("meta")$doi, "10.1007/s00068-022-02217-1")
})

test_that("sdt_reference() rejects an unknown part", {
  expect_error(sdt_reference("nope"))
})

test_that("sdt_reference() serves both evaluated versions", {
  expect_identical(
    sdt_reference("published"), sdt_reference("published", version = "2.0")
  )
  v1 <- sdt_reference("published", version = "1.0")
  v2 <- sdt_reference("published", version = "2.0")
  expect_named(v1, names(v2))
  # The two versions are genuinely different data, not the same list twice.
  expect_false(identical(v1$end_state, v2$end_state))
  expect_false(identical(v1$triage_hit, v2$triage_hit))
  expect_error(sdt_reference("published", version = "3.0"))
})

test_that("published marginals are proper probability vectors", {
  for (version in c("2.0", "1.0")) {
    pub <- sdt_reference("published", version = version)
    for (nm in c("triage_mix", "therapy_mix", "diagnostics_mix", "end_state")) {
      p <- pub[[nm]]
      expect_true(all(p >= 0), info = paste(version, nm))
      # The source prints these rounded to one or two figures, so they do not
      # sum to exactly 1. Normalising them here would make the package's
      # "published" values something the paper does not actually say; the
      # rounding is carried instead, and bounded.
      expect_equal(sum(p), 1, tolerance = 0.03, info = paste(version, nm))
    }
    expect_named(pub$triage_mix, sdt_triage_levels())
    expect_named(pub$diagnostics_mix, sdt_diagnostics())
    # The therapy mix is over procedures performed, so it has no "none" cell.
    expect_named(pub$therapy_mix, setdiff(sdt_therapy_levels(), "none"))
    expect_named(pub$end_state, names(sdt_reference("published")$end_state))
  }
})

test_that("published counts agree with the proportions they came from", {
  for (version in c("2.0", "1.0")) {
    pub <- sdt_reference("published", version = version)
    expect_identical(sum(pub$therapy_n), pub$therapy_total)
    expect_equal(
      unname(pub$therapy_n / pub$therapy_total), unname(pub$therapy_mix),
      tolerance = 0.01, info = version
    )
    expect_identical(sum(pub$casualties_per_category), 20L)
    expect_true(all(pub$triage_hit >= pub$triage_hit_min))
    expect_true(all(pub$triage_hit <= pub$triage_hit_max))
  }
})

test_that("the design records the exercise as published", {
  d <- sdt_reference("design")
  expect_identical(d$duration, 90)
  expect_identical(d$n_cards, 20L)
  expect_identical(d$card_rounds, 4L)
  expect_identical(nrow(d$board), 9L)
  # Every board position in the figure maps to a board area the package knows.
  # The areas the figure does *not* name are the three the published board has
  # no cell for: the two terminal states real exercises reach, and the
  # off-board one.
  expect_true(all(d$board$area %in% sdt_board_areas()))
  expect_setequal(
    setdiff(sdt_board_areas(), d$board$area),
    c("morgue", "external", "not_seen")
  )
  expect_identical(d$life_points, 1:6)
})

test_that("published and default values stay in separate compartments", {
  # The calibration tests are only meaningful if what they check against is
  # attributable to the source rather than to our own modelling choices.
  expect_false(any(names(sdt_reference("published")) %in%
                     names(sdt_reference("defaults"))))
})

test_that("defaults cover every board area and therapy concept", {
  def <- sdt_reference("defaults")
  expect_setequal(names(def$service_times), sdt_board_areas())
  expect_setequal(names(def$or_multiplier), sdt_therapy_levels())
  expect_setequal(names(def$life_bands), sdt_triage_levels())
  expect_setequal(names(def$therapy_given_triage), sdt_triage_levels())
  expect_setequal(names(def$diagnostics), sdt_diagnostics())
  expect_setequal(names(def$observation), sdt_triage_levels())
  expect_setequal(names(def$p_icu_conservative), sdt_triage_levels())
  for (level in sdt_triage_levels()) {
    expect_equal(sum(def$therapy_given_triage[[level]]), 1, tolerance = 1e-8)
    expect_setequal(
      names(def$therapy_given_triage[[level]]), sdt_therapy_levels()
    )
  }
})

test_that("life bands cover the published life-point scale exactly", {
  bands <- sdt_reference("defaults")$life_bands
  expect_setequal(unlist(bands, use.names = FALSE),
                  sdt_reference("design")$life_points)
  # Lower life points are more critical, so the bands must not interleave.
  expect_true(max(bands$red) < min(bands$yellow))
  expect_true(max(bands$yellow) < min(bands$green))
})

test_that("every board area folds onto a published reporting location", {
  map <- sdt_reference("defaults")$end_state_map
  expect_setequal(names(map), sdt_board_areas())
  expect_true(all(map %in% names(sdt_reference("published")$end_state)))
})

test_that("the arrival schedule fits inside the game", {
  # A tranche that starts after the horizon would generate casualties the game
  # can never reach, which is a different thing from the casualties it runs out
  # of time for, and would quietly inflate "not yet seen".
  arr <- sdt_reference("defaults")$arrivals
  expect_length(arr$tranche_starts, sdt_reference("design")$card_rounds)
  expect_identical(arr$tranche_starts[[1L]], 0)
  expect_false(is.unsorted(arr$tranche_starts))
  expect_lt(max(arr$tranche_starts) + arr$tranche_spread, 1)
})

test_that("the reference is cached after the first read", {
  saved <- .sdt_cache$reference
  withr::defer(assign("reference", saved, envir = .sdt_cache))
  rm(list = "reference", envir = .sdt_cache)
  expect_false(exists("reference", envir = .sdt_cache, inherits = FALSE))
  first <- .tdsc_reference()
  expect_true(exists("reference", envir = .sdt_cache, inherits = FALSE))
  expect_identical(first, .tdsc_reference())
})
