test_that("sdt_patient_card() constructs and validates", {
  p <- sdt_patient_card("P01", 34, "m", 4L, TRUE, "thorax", "red", "DCS", 1L)
  expect_s3_class(p, "patient_card")
  expect_invisible(validate_patient_card(p))
})

test_that("sdt_patient_card() rejects bad levels", {
  expect_error(
    sdt_patient_card("P1", 30, "m", 4L, FALSE, "head", "purple", "DCS", 1L),
    "triage"
  )
  expect_error(
    sdt_patient_card("P1", 30, "m", 4L, FALSE, "head", "red", "FOO", 1L),
    "therapy"
  )
})
