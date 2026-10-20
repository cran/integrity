# integrity 1.0.3

* Changed item 2.1 to a two-sided Wald-Wolfowitz runs test, aligning it with
  the published IPD Integrity Tool. Small or unbalanced category counts use
  exact conditional p-values; larger counts use a normal approximation.
* Item 2.1 detailed output now reports observed and expected runs, missing
  counts, p-values, and test methods instead of adjacent-pair counts. This
  changes the columns in `detail_tables[["2.1"]]`.
* Item 2.1 now sorts by the nominated randomisation date, retaining row order
  for ties. It warns and falls back to row order if no usable dates are
  available. Partly missing dates are excluded from this item with a warning.
* Clarified omission of missing baseline values and interpretation of
  untestable variables for item 2.1; recorded the ordering and date exclusions
  in the detailed output.

# integrity 1.0.2

* Added baseline-pattern plots for item 1.1, with a caution when no usable
  randomisation date is supplied.
* Gave terminal-digit plots independent y-axis scales for each variable.
* Changed allocation plots to show cumulative participant counts.
* Preserved continuous types for numeric baseline imbalance checks and
  reported untestable binary baseline checks as skipped.
* Fixed handling of all-missing columns and single-participant rare outcomes.
* Declared the cardx dependency needed for gtsummary statistical tests.
* Updated the vignette with download and adaptation instructions and loading
  through the installed package.
* Corrected Anna Lene Seidler's name and updated author ordering.
* Added regression checks and excluded saved vignette workspaces from builds.

# integrity 1.0.1

* Added a `NEWS.md` file to track changes to the package.

