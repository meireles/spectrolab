library("testthat")
library("spectrolab")

################################################################################
# Validation of the SVC splice against the vendor's own overlap-matched output.
#
# inst/extdata/svc_raw_and_overlap_matched_serbin/ ships the same 14 scans twice:
# SVC_Files/     -- raw, "[Overlap: Preserve, Matching Type: None]"
# SVC_Files_moc/ -- the identical scans reprocessed by SVC's software,
#                   "[Overlap: Remove @ 970,1901, Matching Type: Radiance @ 976 - 1010]"
#
# That makes the _moc set ground truth: whatever match_sensors(method = "svc")
# produces from the raw files should look like it. These tests exist because it
# once did not -- 0.0.20 gain-matched EVERY junction, including the SWIR1/SWIR2
# crossover at 1901 nm that SVC only removes and never matches. The factor there
# is estimated from ~4 bands of ~0.04 reflectance in the 1900 nm water band, came
# out at 0.63-0.84, was silently floored to the 0.8 clamp, and was then ramped
# across the whole of detector 2. The preset ended up further from the vendor
# than doing no matching at all.
#
# The load-bearing assertion is therefore comparative: "svc" must beat "cut".
################################################################################

svc_raw_dir = function(){
    system.file("extdata", "svc_raw_and_overlap_matched_serbin", "SVC_Files",
                package = "spectrolab")
}
svc_moc_dir = function(){
    system.file("extdata", "svc_raw_and_overlap_matched_serbin", "SVC_Files_moc",
                package = "spectrolab")
}

read_quiet = function(d){
    suppressWarnings(suppressMessages(read_spectra(d, format = "sig")))
}

## RMSE of a matched object against the vendor's, over a wavelength range.
## Bands are compared on the vendor's grid; the caller checks grid equality
## separately so a mismatch shows up as its own failure, not as a bad number.
rmse_vs = function(fit, vendor, lo = -Inf, hi = Inf){
    fb  = round(bands(fit), 4)
    vb  = round(bands(vendor), 4)
    k   = match(vb, fb)
    sel = vb >= lo & vb <= hi
    d   = value(fit)[, k[sel], drop = FALSE] - value(vendor)[, sel, drop = FALSE]
    sqrt(mean(d^2))
}


test_that("the fixture pair is what these tests assume it is", {
    skip_if(svc_raw_dir() == "" || svc_moc_dir() == "", "serbin extdata not installed")

    raw = read_quiet(svc_raw_dir())
    moc = read_quiet(svc_moc_dir())

    ## raw still carries the physical detector overlap; moc is already joined
    expect_false(spectrolab:::i_is_increasing(bands(raw)))
    expect_true(spectrolab:::i_is_increasing(bands(moc)))
    expect_equal(unname(nrow(raw)), unname(nrow(moc)))

    ## the vendor recorded both what it removed and where it matched
    si = sensor_info(moc)
    expect_true(all(si$splice_1 == 970 & si$splice_2 == 1901))
    expect_true(all(si$match_lo == 976 & si$match_hi == 1010))
})


test_that("method = 'svc' reproduces the vendor's band grid exactly", {
    skip_if(svc_raw_dir() == "" || svc_moc_dir() == "", "serbin extdata not installed")

    raw = read_quiet(svc_raw_dir())
    moc = read_quiet(svc_moc_dir())
    fit = suppressWarnings(suppressMessages(
        match_sensors(raw, splice_at = c(970, 1901), method = "svc")))

    ## "Remove @ 970" means the left detector stops BELOW 970, so the 970.8 nm
    ## point it also sampled is deleted. Trimming at the right sensor's first
    ## wavelength instead used to keep it -- 983 bands against the vendor's 982.
    expect_equal(bands(fit), bands(moc))
})


test_that("method = 'svc' lands much closer to the vendor than not matching", {
    skip_if(svc_raw_dir() == "" || svc_moc_dir() == "", "serbin extdata not installed")

    raw = read_quiet(svc_raw_dir())
    moc = read_quiet(svc_moc_dir())

    cut = suppressWarnings(suppressMessages(
        match_sensors(raw, splice_at = c(970, 1901), method = "cut")))
    svc = suppressWarnings(suppressMessages(
        match_sensors(raw, splice_at = c(970, 1901), method = "svc")))

    e_cut = rmse_vs(cut, moc)
    e_svc = rmse_vs(svc, moc)

    ## The regression guard: matching must be an improvement over the bare join.
    expect_lt(e_svc, e_cut)

    ## It is in fact a large improvement -- a generous bound that still fails
    ## loudly if the gain is applied at the wrong junction or with a bad window.
    expect_lt(e_svc, e_cut / 3)
    expect_lt(e_svc, 0.003)
})


test_that("method = 'svc' corrects detector 1 and leaves 2 and 3 alone", {
    skip_if(svc_raw_dir() == "" || svc_moc_dir() == "", "serbin extdata not installed")

    raw = read_quiet(svc_raw_dir())
    moc = read_quiet(svc_moc_dir())

    cut = suppressWarnings(suppressMessages(
        match_sensors(raw, splice_at = c(970, 1901), method = "cut")))
    svc = suppressWarnings(suppressMessages(
        match_sensors(raw, splice_at = c(970, 1901), method = "svc")))

    ## Detector 1 (below the first splice) is where the whole correction lives.
    expect_lt(rmse_vs(svc, moc, hi = 970), rmse_vs(cut, moc, hi = 970) / 3)

    ## Detectors 2 and 3 must come through the join untouched: SVC removes the
    ## 1901 nm overlap but does not magnitude-match across it, and its output is
    ## identical to the raw file there. This is the assertion that fails if
    ## gain_at ever goes back to "all".
    b  = bands(svc)
    up = b >= 970
    expect_equal(value(svc)[, up], value(cut)[, up])
})


test_that("the 1901 nm junction factor is implausible, and is refused", {
    skip_if(svc_raw_dir() == "", "serbin extdata not installed")

    raw = read_quiet(svc_raw_dir())
    b   = bands(raw)
    v   = value(raw)

    ## Reconstruct the factor the engine would have computed at the SWIR1/SWIR2
    ## overlap, to document WHY that junction is excluded rather than just that
    ## it is.
    bounds = spectrolab:::i_find_sensor_overlap_bounds(b)
    s2 = seq.int(bounds[["begin", 2]], bounds[["end", 2]])
    s3 = seq.int(bounds[["begin", 3]], bounds[["end", 3]])
    lo = max(min(b[s2]), min(b[s3]))
    hi = min(max(b[s2]), max(b[s3]))

    lf  = s2[b[s2] >= lo & b[s2] <= hi]
    rf  = s3[b[s3] >= lo & b[s3] <= hi]
    fac = rowMeans(v[, rf, drop = FALSE]) / rowMeans(v[, lf, drop = FALSE])

    ## A handful of bands, all in the 1900 nm water band, at very low signal.
    expect_lt(length(c(lf, rf)), 12)
    expect_lt(mean(v[, c(lf, rf)]), 0.10)

    ## and the resulting factors are nowhere near a real detector gain
    expect_true(all(fac < 0.85))

    ## 13 of the 14 fall outside SVC's plausible range, so the clamp was not
    ## quietly trimming an almost-right factor -- it was hiding an estimate with
    ## no signal behind it. (The one that squeaks in at 0.837 is no better; it is
    ## excluded by gain_at, not by the range check.)
    svc = spectrolab:::i_splice_preset("svc")
    ok  = vapply(fac, spectrolab:::i_gain_is_plausible, logical(1), clamp = svc$clamp)
    expect_gte(sum(!ok), 13)
})


test_that("an implausible factor warns and leaves the data uncorrected", {
    ## Two sensors overlapping over 950-1000 nm, where the right one reads a
    ## tenth of the left: a factor of ~0.1, far outside any plausible range.
    l_wl = seq(400, 1000, 5)
    r_wl = seq(950, 1500, 5)
    syn  = spectra(matrix(c(rep(0.5, length(l_wl)), rep(0.05, length(r_wl))), nrow = 1),
                   c(l_wl, r_wl), "a")

    cfg = splice_config(gain_type = "multiplicative", reference = "right",
                        clamp = c(0.8, 1.2), join = "cut")

    expect_warning(out <- suppressMessages(match_sensors(syn, splice_at = 975, config = cfg)),
                   "outside the plausible range")

    ## Refused, not floored to 0.8: the left sensor keeps its original values.
    ref = suppressMessages(match_sensors(syn, splice_at = 975, method = "cut"))
    expect_equal(value(out), value(ref))
})


test_that("legacy match_sensors matches only the first junction when sensors overlap", {
    skip_if(svc_raw_dir() == "", "serbin extdata not installed")

    raw = read_quiet(svc_raw_dir())
    sp  = c(970, 1901)

    leg = suppressWarnings(suppressMessages(match_sensors(raw, splice_at = sp)))
    cut = suppressWarnings(suppressMessages(match_sensors(raw, splice_at = sp, method = "cut")))

    b = bands(leg)
    ## detector 1 is rescaled ...
    expect_false(isTRUE(all.equal(value(leg)[, b < 970], value(cut)[, b < 970])))
    ## ... and everything above the first splice is left as joined. Looping over
    ## every junction here scaled detector 3 by a ramp starting at 1.50.
    expect_equal(value(leg)[, b >= 970], value(cut)[, b >= 970])
})
