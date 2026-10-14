test_that("sdt_add_decisions() adds the call columns and stays a cohort", {
  co <- sdt_synthetic_cohort(n = 50, seed = 1)
  out <- sdt_add_decisions(co, seed = 2)
  expect_s3_class(out, "cohort")
  expect_true(all(c("called_triage", "called_therapy") %in% names(out)))
  expect_true(all(out$called_triage %in% sdt_triage_levels()))
  expect_true(all(out$called_therapy %in% sdt_therapy_levels()))
  expect_identical(nrow(out), nrow(co))
})

test_that("sdt_add_decisions() is reproducible", {
  co <- sdt_synthetic_cohort(n = 40, seed = 1)
  expect_identical(sdt_add_decisions(co, seed = 3),
                   sdt_add_decisions(co, seed = 3))
})

test_that("realised hit rates match the published ones", {
  co <- sdt_add_decisions(sdt_synthetic_cohort(n = 4000, seed = 1), seed = 2)
  observed <- tapply(co$triage == co$called_triage, co$triage, mean)
  published <- sdt_reference("published")$triage_hit
  for (level in sdt_triage_levels()) {
    expect_lt(abs(observed[[level]] - published[[level]]), 0.04)
  }
})

test_that("a perfect participant reproduces the truth exactly", {
  co <- sdt_synthetic_cohort(n = 60, seed = 4)
  perfect <- c(red = 1, yellow = 1, green = 1)
  out <- sdt_add_decisions(co, hit = perfect, therapy_hit = 1, seed = 1)
  expect_identical(out$called_triage, out$triage)
  expect_identical(out$called_therapy, out$therapy)
})

test_that("a participant who never gets it right never agrees", {
  co <- sdt_synthetic_cohort(n = 60, seed = 4)
  never <- c(red = 0, yellow = 0, green = 0)
  out <- sdt_add_decisions(co, hit = never, therapy_hit = 0, seed = 1)
  expect_false(any(out$called_triage == out$triage))
  expect_false(any(out$called_therapy == out$therapy))
})

test_that("triage miscalls favour the neighbouring category", {
  # Confusing red with yellow is common; confusing red with green is not.
  co <- sdt_synthetic_cohort(n = 3000, seed = 5)
  out <- sdt_add_decisions(co, hit = c(red = 0, yellow = 1, green = 1),
                           seed = 6)
  wrong <- out$called_triage[out$triage == "red"]
  expect_gt(mean(wrong == "yellow"), mean(wrong == "green"))
})

test_that("sdt_add_decisions() validates its arguments", {
  co <- sdt_synthetic_cohort(n = 10, seed = 1)
  expect_error(sdt_add_decisions(co, hit = c(0.5, 0.5)), "named numeric")
  expect_error(sdt_add_decisions(co, hit = c(red = 2)), "named numeric")
  expect_error(sdt_add_decisions(co, hit = c(purple = 0.5)), "Unknown")
  expect_error(sdt_add_decisions(co, therapy_hit = 5), "single probability")
  expect_error(sdt_add_decisions(co, therapy_hit = "high"),
               "single probability")
})

test_that("sdt_add_decisions() handles a zero-row cohort", {
  empty <- sdt_cohort(sdt_synthetic_cohort(n = 3, seed = 1)[0, ])
  out <- sdt_add_decisions(empty)
  expect_identical(nrow(out), 0L)
  expect_true(all(c("called_triage", "called_therapy") %in% names(out)))
})

test_that("calls agree with the truth well above chance but not perfectly", {
  skip_if_not_installed("yardstick")
  co <- sdt_add_decisions(sdt_synthetic_cohort(n = 1500, seed = 7), seed = 8)
  kappa <- sdt_consistency_score(co$triage, co$called_triage)$.estimate
  expect_gt(kappa, 0.4)
  expect_lt(kappa, 0.95)
})
