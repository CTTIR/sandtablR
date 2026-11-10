## Submission

This is a new submission of sandtablR 0.1.0.

## Test environments

* local: macOS (aarch64-apple-darwin), R 4.6.1, `R CMD check --as-cran
  --no-manual` on the built tarball with all suggested packages installed.
  The PDF manual was built locally with TinyTeX (TeX Live 2026) via
  `R CMD Rd2pdf` without errors; the check run itself used `--no-manual`.
* GitHub Actions (r-lib/actions v2, `--as-cran`):
  macOS-latest (release), windows-latest (release),
  ubuntu-latest (devel, release, oldrel-1).

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Raban Heller <raban.heller@uni-ulm.de>'
  New submission

  This is the first submission of the package.

## Notes for the CRAN team

* All modelling, explanation and sequence-analysis packages are in Suggests
  and are used conditionally; examples and tests skip the corresponding
  parts when a package is not installed.
* The transformer engine needs 'keras3' with a Python backend only for
  fitting. Examples build the model specification without Python, and the
  fitting tests are skipped when no working backend is available.
* Parallel code uses at most two workers in tests. The browser-driven
  workbench test is skipped on CRAN.
* The two workbooks in `inst/extdata` are small synthetic files used by the
  examples; they contain no personal data.
