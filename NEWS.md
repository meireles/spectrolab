# spectrolab 0.0.20 (development)

## major
* **[BUG FIX]** `match_sensors()` gain-matched *every* detector junction, which
  made the `"svc"` preset worse than doing no matching at all. Validated against
  the vendor-reprocessed files shipped in
  `inst/extdata/svc_raw_and_overlap_matched_serbin/` (the same 14 scans raw and
  after SVC's own overlap matching), RMSE against the vendor's output:

  | | overall | det 1 | det 2 | det 3 |
  |---|---|---|---|---|
  | `method = "cut"` (join only, no gain) | 0.00771 | 0.01098 | 0.00218 | 0 |
  | `method = "svc"`, before | 0.01395 | 0.00197 | **0.02741** | 0 |
  | `method = "svc"`, now | **0.00116** | **0.00053** | 0.00218 | 0 |

  The SWIR1/SWIR2 crossover near 1900 nm sits in the deep water band at the edge
  of both detectors' sensitivity: the factor estimated there came out at
  0.63--0.84 across the reference set, was silently floored to the 0.8 `clamp`,
  and was then ramped across the whole of detector 2. SVC's own header records
  two removals but only one matching zone
  (`Overlap: Remove @ 970,1901, Matching Type: Radiance @ 976 - 1010`), and its
  output leaves detectors 2 and 3 identical to the raw file. Four changes:
  - `splice_config()` gains `gain_at` (`"all"`, `"first"`, or junction indices);
    the `"svc"` preset now matches only the VNIR/SWIR1 junction. This restores,
    and states outright, the `iter = 1` rule that 0.0.19 had in the legacy path
    and that 0.0.20 removed as if it were a bug.
  - `clamp` is now a plausibility **gate**, not a floor: a factor outside the
    range leaves that junction uncorrected and warns, instead of quietly
    applying a known-bad correction.
  - `splice_config()` gains `window_inset` (default 0.10). The auto-detected
    matching window is now inset off both ends of the overlap, where detector
    response rolls off; on SVC data this reproduces the vendor's own 976--1010 nm
    zone from a 971.8--1016.6 nm overlap, and cuts the detector-1 residual by
    almost 4x.
  - The legacy (`method = NULL`/`"scale"`) path had the same regression, ramping
    detector 3 by a factor starting at 1.50 (RMSE there 0 -> 0.01915). The 0.0.19
    guard is restored: with a physical detector overlap only the first junction is
    matched; with no overlap (already-joined data split by `splice_at`) every
    junction is, as before.
* **[BEHAVIOR CHANGE]** `match_sensors()` now trims the left detector at
  `splice_at` itself rather than at the right detector's first wavelength. A
  band the left sensor recorded between the two (970.8 nm on the SVC reference
  data) used to survive, so a file spliced at 970 came back with 983 bands where
  the vendor produces 982. Output now matches the vendor grid exactly.
* `sensor_info()` gains `match_lo`/`match_hi`, the vendor's magnitude-matching
  window, parsed from the SVC `factors=` header's dash pair. `match_sensors()`
  uses it in preference to the auto-detected window when a file records one.
  (Raw files saved as `Matching Type: None` do not, so the inset default above is
  what does the work in the common case.)
* **[BEHAVIOR CHANGE]** `resample()` now uses an overlap-integral model instead
  of a point-sampled Gaussian kernel. Each source band is treated as a boxcar of
  width equal to its own FWHM (defaulting to the midpoint rule from band
  spacing), and each destination band as a Gaussian response; the weight is the
  Gaussian mass falling inside each source boxcar (`pnorm` difference). Carrying
  the source band width as a wavelength-interval weight removes a bias the old
  "delta function" kernel showed at detector-boundary spacing jumps on
  non-uniform grids -- up to ~2% at those transitions (~0.01% median elsewhere),
  validated against quadrature-weighted SRF convolution and SpectralPython's
  `BandResampler`. New optional `src_fwhm` argument lets power users supply known
  instrument bandpass widths. Instead of silently trimming out-of-range bands,
  destination bands whose covered response falls below `coverage_min` (default
  0.5) are returned as `NA` with a single warning.
* `make_fwhm()` dropped its `k`/k-means quantization path, which used a randomly
  initialized `stats::kmeans` and was therefore nondeterministic run-to-run (the
  old exported default `k = 3` meant `make_fwhm` changed each call). It now
  returns full-detail, deterministic FWHM. Its default source-FWHM derivation
  also switched to the midpoint rule, matching `resample()`.

* `Ops.spectra` (`+`, `-`, `*`, `/`, `^` between two spectra) previously kept
  metadata from the left-hand side only, silently dropping the right-hand
  side's metadata even when they differed. It now keeps metadata when both
  sides agree and clears it (with a warning) when they don't -- the same
  rule already used for sample names. Unary `-x`/`+x` (e.g. `-spec`), which
  previously errored ("argument e2 is missing"), are now supported too.
* **[BREAKING]** Minimum R version raised to 4.3 (from 4.0), needed for `%*%.spectra`
  (see below) -- R 4.3 made `%*%` properly S3-generic in both argument positions.
* Fixed several base-R generics that silently misbehaved on `spectra` objects
  instead of erroring or working correctly:
  - `length(x)` returned 4 (the object's internal slot count) instead of the
    number of samples.
  - `is.na(x)` checked the 4 internal slots (always `FALSE`) instead of the
    value matrix, so `any(is.na(x))` could never detect real missing data.
  - `abs()`/`sqrt()`/`log()`/`round()` etc. either silently returned garbage
    or errored outright; a `Math` group generic now covers them (`cumsum`/
    `cumprod`/`cummax`/`cummin` are applied row-wise, since their default
    method would otherwise flatten the value matrix to a vector).
  - `c(s1, s2)` and `rbind(s1, s2)` silently degraded to a plain `list`/
    `matrix` instead of combining the spectra; both now wrap `combine()`.
* Added `%*%.spectra`: `spectra %*% y` or `y %*% spectra` now works (returns
  a plain matrix, not a `spectra`, since the result's shape/meaning varies).
  The previous attempt was abandoned in 2016 because `%*%` wasn't
  S3-dispatchable from the right-hand side; R 4.3 fixed that.
* Added Savitzky-Golay smoothing (`smooth_sgolay`, or `smooth(method = "sgolay")`)
  and spectral derivatives (`deriv_spectra`). Both require the `signal` package
  (now in Suggests).
* Added two-band normalized-difference spectral indices: `make_spectral_index()`
  builds a two-band index function, and `spectral_index` is a list of built-in
  ones (`spectral_index$ndvi`, `spectral_index$pri`). NDVI/PRI are intentionally
  not their own top-level exports, to keep the namespace small.
* Added a tidy/long bridge: `to_long()` (dependency-free), `as_tibble.spectra`
  (requires `tibble`), and `autoplot.spectra` (requires `ggplot2`). `tibble`
  and `ggplot2` are now in Suggests.
* Duplicate band labels are now allowed and preserved. Previously the constructor
  silently nudged duplicate wavelengths (e.g. 600 -> 600.0012) to force
  uniqueness, which altered the data and was buggy for 3+ identical values. Bands
  now behave like sample names: selecting a duplicated label returns all matching
  bands and emits a message. The sensor-splice trimmer was made positional so
  duplicates cannot corrupt a join. **This may change results for scripts that
  relied on the old nudged band values; the vast majority of data (unique bands)
  is unaffected.**

## bug fixes

* **[BEHAVIOR CHANGE]** `match_sensors()` (the default, legacy "scale" algorithm)
  only applied the *first* junction's correction whenever the data had a real
  detector overlap and more than one junction. On a 3-detector spectrum the far
  sensor came out bit-identical to applying no gain at all. On the bundled
  `Acer_example` SVC data that left 25% of the bands uncorrected and the
  SWIR1/SWIR2 step at 0.0167 instead of 0.0067. Every junction is now corrected.
  **Results change for any 3-sensor data spliced with the default method.**
* `match_sensors()` no longer silently discards a `fixed_sensor` it cannot
  honour (3 sensors force `fixed_sensor = 2`); it warns. An out-of-range value
  now errors clearly instead of dying with "argument is of length zero".
* The splice engine's automatic crossover window was computed from the band
  spacing of the two detector segments *concatenated*, so it measured the jump
  between them rather than the spacing within one. The window came out around
  22000 nm instead of ~22 nm, making every "local" mean in the multiplicative,
  `ssd` and `mean_diff` gain estimators a whole-spectrum mean.
* `resample()` now validates `new_bands`. Unsorted, duplicated, non-finite or
  empty destination grids were accepted and produced a `spectra` that violated
  the strictly-increasing invariant, failing much later with a misleading
  "match sensor overlap first" message.
* `resample()` now carries `sensor_info` provenance through, which also keeps it
  alive across `smooth(method = "gaussian")`.
* `meta(x, "no_such_label")` was completely silent when the object had no
  metadata columns -- including under `quiet = FALSE`, documented as a hard
  error. It now warns (or errors) consistently.
* `combine()` warns when one side is vector-normalized and the other is not,
  instead of silently returning an object that mixes two y scales.
* `Ops.spectra` compared band labels with an exact `!=` while `combine()` used
  `all.equal()`, so a floating-point difference of 1e-12 errored in one and
  passed in the other. Both now use `all.equal()`.
* The `spectra` constructor no longer turns non-numeric input into a matrix of
  `NA`s behind a bare coercion warning; it errors and points at `name_idx` /
  `meta_idxs`. `NA`s already present in numeric input are still preserved.
* `smooth_spline()` forked `parallel::detectCores() - 1L` workers unconditionally
  (127 processes on a large host, and above the two-core ceiling CRAN checks run
  under). It gained a `cores` argument defaulting to `getOption("mc.cores", 2L)`
  and never forks more workers than there are spectra.
* The "gap between bands is too wide" warning used `paste(sep = ",")` instead of
  `collapse`, printing bands 898, 599 and 8 as the single token `8985998`.
* `quantile()` no longer tags its result with a `spec_quantile` class that
  nothing dispatched on and that the first `[` dropped anyway.
* Removed dead code: the unused `i_sensor_info_cols` constant, a no-op `x = x`
  self-assignment and an immediately-overwritten `bands()` call in
  `match_sensors()`, an unreachable `||` clause in `i_match_label()`, and a
  `NULL` no-op branch in `resample()`. `combine()`'s type error named a
  parameter (`b`) that does not exist.

# spectrolab 0.0.19 (2025-01-07)

## major
* resampling using a gaussian model and the fwhm of each band is now the default


# spectrolab 0.0.18 (2023-02-10)

## major
* fixed bug that rescaled spectra when reading .sed files.

# spectrolab 0.0.17 (2022-07-12)

## major
* read spectra can now read the new .sed format.

# spectrolab 0.0.16 (2021-09-26)

## major
* Fixed bug in read_spectra, where the meatadata was not being read if the spectra had different bands.

# spectrolab 0.0.15 (2021-09-14)

## major
* Fixed bug in read_spectra, where the date meatadata was being read from the wrong field.

# spectrolab 0.0.14 (2021-08-09)

## major
* Fixed major bug in read_spectra. The metadata from target and reference where swapped.
* Now requires R >= 4.0

## minor
* Updated citation
* Updated intro vignette

# spectrolab 0.0.13 (2021-08-05)

## major
* read_spectra now reads metadata from .sig and .sed files
* read_spectra guesses the file format automatically

# spectrolab 0.0.12 (2021-06-01)

## major
* Added custom read_spectra function for ASD
* Removed dependency from prospectr
* Added the function guess_splice_at

## minor
* Updated match_sensors vignette

# spectrolab 0.0.11 (2021-05-18)

## major
* Fixed bug in the match_sensors function 

## minor
* Created a vignette for the match_sensors function

# spectrolab 0.0.10 (2020-10-08)

## major
* Breaking backwards compatibility! 
* as.spectra is now as_spectra
* deprecated ratio.spectra
* removed dependency from devtools

## minor
* refactored resample, normalize, print, getter & setter
* updated vignette

# spectrolab 0.0.9 (2020-05-21)

## major
* Breaking backwards compatibility! 
* wavelengths function is now called bands
* reflectance function is now called value

# spectrolab 0.0.8 (2018-10-28)

## major
* switched to usethis::use_package from devtools deprecated version
* generic "smooth" function has its own interface and the default now
  calls stats::smooth internally. Work in progress.

# spectrolab 0.0.7 (2018-07-24)

## major
* Fixed major bug. Spectrum replacement (e.g. spec_a[i, ] = spec_b[1, ]) only
  replaced the value and not the other information (names, metadata, etc.)
## minor
* `quantile` now allows the user to choose names for the spectra

# spectrolab 0.0.6 (2018-06-19)

## major
* *NOT backwards compatible!*. Now, as.spectra() does not assume by default that
  the first column of a matrix or data.frame contains the names for the spectra.
  Users must explicitly use the `name_idx` argument to pick the correct column
* *NOT backwards compatible!*. `enforce01` functions and attribute deprecated.

## minor
* fixed bug in subset_by, where n_min was matching sample names instead of indices.

# spectrolab 0.0.5 (2018-05-31)

## major
* Fixed bug in "value" setter, where the object was not being returned
* The parser inside read_spectra now finds the spectral data using tags instead
  of assuming that the data starts at a specific line

## minor
* added unit tests for the read_spectra function
* cleaned up several minor issues

# spectrolab 0.0.4 (2017-12-13)

## major
* sample names in the spectra class are now required to be char. Names coercible to numeric will have a prefix added to them by default. This may break existing code!

## minor
* updated README
* refactored the internal i_is_index function
* added unit tests for i_is_index function

# spectrolab 0.0.3 (2017-11-15)

## minor
* fixed index duplication in the _match sensor_ vignette.
* added CRAN installation instructions to readme.md
* internal spline smoothing now requires the parallel package

# spectrolab 0.0.2 (2017-09-14)

* Initial release
