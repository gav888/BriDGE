# Contributing to BriDGE

Thank you for your interest in improving BriDGE!

## Reporting bugs

Please open a [GitHub issue](https://github.com/gav888/BriDGE/issues)
using the bug report template. The most useful reports include:

- a minimal reproducible example (ideally using
  [`bridge_generate_data()`](https://gav888.github.io/BriDGE/reference/bridge_generate_data.md))
- the full error message or unexpected output
- the output of
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html)

## Suggesting features

Open an issue with the feature request template. Check the existing
[roadmap
issues](https://github.com/gav888/BriDGE/issues?q=is%3Aissue+label%3Aroadmap)
first — the feature may already be planned.

## Pull requests

1.  Fork the repo and create a branch from `main`
    (`feature/short-description` or `bugfix/short-description`).

2.  Make your changes. Please:

    - follow the existing code style,
    - add or update tests in `tests/testthat/`,
    - update documentation (roxygen comments; run
      `devtools::document()`),
    - add a `NEWS.md` entry for user-visible changes.

3.  Verify locally before submitting:

    ``` r

    devtools::document()
    devtools::test()
    devtools::check()   # should give 0 errors, 0 warnings, 0 notes
    ```

4.  Open a pull request against `main` describing the change and its
    motivation. CI (R-CMD-check on macOS and Ubuntu) must pass.

## Scope notes

BriDGE implements the protocol from Veltri & Banerjee (in press,
*Behavior Research Methods*). Methodological extensions (new estimators,
new discovery constraints) are welcome but should stay consistent with
the paper’s framework — open an issue to discuss before investing
significant effort.

## Code of conduct

Be respectful and constructive. Scientific disagreement is welcome;
personal hostility is not.
