# spectrolab 0.0.20 (development)

## major
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

## major
* Added minimal provenance: `quantity()`/`quantity<-` (e.g. "reflectance",
  "radiance") and `wavelength_unit()`/`wavelength_unit<-` (default "nm").
  `read_spectra()` sets both from its `type` argument; they carry through
  subsetting, `apply_by_band`, `aggregate`, `resample`, and `match_sensors`.
  `combine()` and the arithmetic operators (`+`, `-`, etc.) keep the value
  when both sides agree and clear it (with a warning) when they don't, rather
  than silently keeping one side. `deriv_spectra()` and `continuum_removal()`
  clear `quantity` since their output is no longer raw reflectance/radiance.
  `print.spectra` shows `quantity` when known.
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
* Added `continuum_removal()`, which divides each spectrum by its upper convex
  hull (the continuum).
* Added two-band normalized-difference spectral indices: `make_spectral_index()`
  builds a two-band index function, and `spectral_index` is a list of built-in
  ones (`spectral_index$ndvi`, `spectral_index$pri`); `spectral_indices()`
  computes several at once. NDVI/PRI are intentionally not their own top-level
  exports, to keep the namespace small.
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
