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
