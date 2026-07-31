# spectrolab 0.0.20 (development)

## major
* **[BREAKING]** `match_sensors()` is now the single public way to splice
  detectors, and it runs a single algorithm. `splice_config()` (and its `print`
  method) is no longer exported, and the `method` / `config` arguments are gone
  along with the `"svc"`, `"naturaspec"`, `"asd"`, `"cut"`, `"ramp"`,
  `"concatenate"` and `"scale"` presets they selected. The signature is back to
  the 0.0.19 one: `match_sensors(x, splice_at, fixed_sensor, interpolate_wvl)`.
  Code that passed `method` or `config` (introduced during 0.0.20 development,
  never released) must drop those arguments; the default path now does more than
  any preset did.
* **[BEHAVIOR CHANGE]** The splice algorithm itself was rewritten, and validated
  band by band against the vendor-reprocessed files shipped in
  `inst/extdata/svc_raw_and_overlap_matched_serbin/` -- the same 14 SVC scans
  raw and after the instrument's own overlap matching. RMSE against the vendor's
  output, in reflectance units:

  | | overall | det 1 | det 2 | det 3 |
  |---|---|---|---|---|
  | join only, no matching | 0.00771 | 0.01098 | 0.00218 | 0 |
  | the previous algorithm (whole-sensor scalar + straight ramp) | 0.01121 | 0.01604 | 0.00218 | 0 |
  | **now** | **0.00013** | **0.00015** | **0.00014** | ~0 |

  It joins in three steps -- gain match, cut, crossfade -- and each was derived
  from that comparison:
  - The gain is applied **tapered**: full at the junction, fading to no
    correction at the far end of the corrected detector, over the detector's
    whole wavelength extent. The taper is mildly convex (a power of 1.3 on the
    wavelength fraction), which is what the vendor's own correction does --
    fitting its output/input ratio gives 1.308 +/- 0.005 across all 14 scans,
    and sweeping the exponent against RMSE independently bottoms out at 1.3.
    A straight ramp costs about 4x overall (0.00013 -> 0.00059).
  - The gain **amplitude is solved through the taper** rather than read off as a
    ratio of window means. Inside the matching window the taper is already at
    ~96% of full, so a plain ratio under-corrects by ~0.2%; solving costs
    nothing and is worth 4x on detector 1.
  - The seam is **crossfaded**: across the crossover window the output is a
    weighted mix of both detectors, so no step is left where they meet. A bare
    cut leaves a ~10% step at the SVC junction; this leaves 0.3%, and the
    vendor's own output has 0.2%. Worth 8x overall.
  - Only the **first** junction is gain-matched when the detectors physically
    overlap. The far crossover of a 3-detector instrument sits near 1900 nm,
    inside the deep water band where both detectors are at the edge of their
    sensitivity: the factor estimated there is noise (0.63--0.84 across the
    reference set), and applying it ramps a large error across a whole detector
    (RMSE 0.00013 -> 0.02263). SVC's own header makes the same call -- it records
    two removals but only one matching zone. With no overlap (already-joined
    data split by `splice_at`) every junction is matched, as before.
  - The plausibility check on the gain is a **gate, not a floor**: a factor
    outside the plausible range leaves that junction uncorrected, and unblended,
    with a warning, instead of quietly applying a known-bad correction. The range
    is a wide, unit-agnostic sanity band, so radiance splices (which legitimately
    need factors near 1.3) are not refused.
  - `interpolate_wvl` now applies only when the detectors do *not* overlap. When
    they do, the crossover window is the overlap itself, inset 10% off each end
    where detector response rolls off -- on SVC that turns a 971.8--1016.6 nm
    overlap into 976.3--1012.1 nm, essentially the vendor's own 976--1010 zone.
* **[BEHAVIOR CHANGE]** `match_sensors()` now trims the left detector at
  `splice_at` itself rather than at the right detector's first wavelength. A
  band the left sensor recorded between the two (970.8 nm on the SVC reference
  data) used to survive, so a file spliced at 970 came back with 983 bands where
  the vendor produces 982. Output now matches the vendor grid exactly.
* `sensor_info()` gains `match_lo`/`match_hi`, the vendor's magnitude-matching
  window, parsed from the SVC `factors=` header's dash pair. `match_sensors()`
  falls back to it when there is no visible overlap to measure a window from.
  (A visible overlap is the better estimate, and measurably so: using the
  recorded zone instead costs 0.00013 -> 0.00032.)
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

* `match_sensors()` no longer silently discards a `fixed_sensor` it cannot
  honour (3 sensors force `fixed_sensor = 2`); it warns. An out-of-range value
  now errors clearly instead of dying with "argument is of length zero".
* `match_sensors()` no longer drops a band when a splice point falls between two
  bands of already-joined data. The segment boundary was `max(which(w <= splice_at))`
  and the trim then excluded that column from both sides, so the band vanished
  silently.
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
