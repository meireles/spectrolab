## Test environments
* local macOS (darwin 25.5.0), R 4.6.0
* TODO before submitting: re-run rhub_check() on
- linux Ubuntu (R-devel)
- macos intel, (R-devel)
- macos arm64 (R-devel)
- Windows Server (R-devel)

## R CMD check results
0 errors | 0 warnings | 1 note

* checking installed package size ... NOTE
    installed size is  6.8Mb
    sub-directories of 1Mb or more:
      doc       3.7Mb
      extdata   1.9Mb

  `doc` holds six vignettes with worked examples on real instrument files.
  `extdata` holds the small SVC/ASD/PSR example files that those vignettes and
  the tests read, which is the only way to exercise the multi-vendor readers.

## Reverse dependencies
* There are no reverse dependencies as per devtools::revdep_check()

## Notes for this release
This release changes numerical results for multi-detector data:
`match_sensors()` was rewritten around a single algorithm (tapered gain match,
cut, crossfaded seam) and validated band by band against instrument-processed
reference files shipped in the package. `resample()` and duplicate band labels
also changed behaviour. See NEWS.md for the full list; each behaviour change is
tagged there.
