################################################################################
# Sensor / detector-splice provenance  (Phase 1 of the match_sensors redesign)
#
# WHY THIS EXISTS
# ---------------
# A full-range spectroradiometer stitches several detector segments (VNIR/Si +
# SWIR1 + SWIR2) into one spectrum, and the three vendors spectrolab supports do
# *structurally different* things at that join (see ai_reviews/MATCH_SENSORS_PLAN.md):
#
#   * SVC  (.sig): by default DELETES the overlapping points (a clean edge, no
#                  blend); optionally rescales the VIS side by a single factor.
#                  Crucially, the .sig header records EXACTLY what was done.
#   * Spectral Evolution / PSR (.sed): blends with a linear ramp; the per-unit
#                  anchor wavelengths are NOT stored in the .sed file.
#   * ASD  (.asd): the vendor algorithm is not recoverable, but the file header
#                  does store the two splice wavelengths (per file).
#
# The single biggest correctness lever is therefore NOT the blend math but
# reading what the file already tells us. This module captures that vendor
# provenance at read time and carries it on the `spectra` object (as the
# "sensor_info" attribute) so that match_sensors() can (a) recognise data that is
# already spliced and leave it alone, and (b) use vendor-recorded splice points
# instead of guessing.
#
# The information is captured as a data.frame with ONE ROW PER SAMPLE (a spectra
# can hold many files, each with its own header), using a single canonical column
# schema across instruments so the record can be row-bound by combine() and
# row-subset by `[`.
#
# NOTE ON SCOPE: this is the read-layer phase. The actual vendor-faithful
# splicing engine (presets, ramp/cut/concatenate, gain estimators) is the next
# phase and will *consume* the provenance captured here.
################################################################################

## Canonical column schema for the per-sample sensor_info record -- keeping every
## instrument on the same columns (NA where a field does not apply) is what lets
## combine() rbind two records and `[` subset one. The schema lives in
## i_new_sensor_info() below, which is the single place that builds it:
##
##   instrument  vendor tag ("svc", "psr", "asd")
##   splice_1    first  detector join wavelength (nm)
##   splice_2    second detector join wavelength (nm)
##   match_lo    low  edge of the vendor's magnitude-matching zone (nm)
##   match_hi    high edge of the vendor's magnitude-matching zone (nm)
##
## match_lo/match_hi are the window over which the vendor computed its detector
## matching factor. They are NOT the splice points: SVC records them separately
## ("Remove @ 970,1901" vs "Matching Type: Radiance @ 976 - 1010") because a
## matching zone is deliberately inset from the overlap edges, where detector
## response rolls off and a ratio of the two sensors is unreliable. match_sensors
## consumes them when present -- see i_match_window_from_provenance().


#' Build an empty canonical sensor_info data.frame
#'
#' \code{i_new_sensor_info} returns an \code{n}-row data.frame with the canonical
#' \code{sensor_info} columns, all filled with the appropriate NA, plus the
#' \code{instrument} label. Fillers then overwrite the columns they know about.
#'
#' @param instrument character vendor tag ("svc", "psr", "asd")
#' @param n number of rows (samples)
#' @return data.frame with \code{n} rows and the canonical columns
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_new_sensor_info = function(instrument, n){
    df = data.frame(instrument = rep(as.character(instrument), length.out = n),
                    splice_1   = rep(NA_real_, n),
                    splice_2   = rep(NA_real_, n),
                    match_lo   = rep(NA_real_, n),
                    match_hi   = rep(NA_real_, n),
                    stringsAsFactors = FALSE)
    rownames(df) = NULL
    df
}


#' Parse the detector-splice wavelengths from an SVC `.sig` "factors=" line
#'
#' \code{i_parse_svc_overlap} extracts the two detector-crossover wavelengths
#' from the single most information-rich line in an SVC header, e.g.
#'
#' \preformatted{
#' factors= 0.795, 0.848, 1.000 [Overlap: Remove @ 970,1901, Matching Type: Radiance @ 976 - 1010 / NIR-SWIR On]
#' }
#'
#' The crossovers are the COMMA pair ("@ 970,1901"); the DASH pair
#' ("@ 976 - 1010") is the magnitude-matching zone. Both are captured. When a
#' file has been reprocessed the line can carry more than one bracketed record;
#' only the FIRST (applied) record is parsed.
#'
#' @param line a single character string (one header line), or NA
#' @return a named list with \code{splice_1}, \code{splice_2}, \code{match_lo}
#'         and \code{match_hi} (NA where absent)
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_parse_svc_overlap = function(line){

    out = list(splice_1 = NA_real_, splice_2 = NA_real_,
               match_lo = NA_real_, match_hi = NA_real_)

    if(length(line) != 1 || is.na(line) || !grepl("factors=", line, fixed = TRUE)){
        return(out)
    }

    ## Applied record = FIRST bracketed block
    blk = regmatches(line, regexpr("\\[[^]]*\\]", line))
    if(length(blk) != 1){
        return(out)
    }

    ## Splice crossovers are recorded as a COMMA pair ("@ 970,1901"); the
    ## matching zone as a DASH pair ("@ 976 - 1010"). That comma-vs-dash
    ## distinction is what lets us pull the crossovers out of the same block.
    sp = regmatches(blk, regexpr("@[[:space:]]*[0-9.]+[[:space:]]*,[[:space:]]*[0-9.]+", blk))
    if(length(sp) == 1){
        v = as.numeric(regmatches(sp, gregexpr("[0-9.]+", sp))[[1]])
        out$splice_1 = v[1]
        out$splice_2 = v[2]
    }

    mz = regmatches(blk, regexpr("@[[:space:]]*[0-9.]+[[:space:]]*-[[:space:]]*[0-9.]+", blk))
    if(length(mz) == 1){
        v = sort(as.numeric(regmatches(mz, gregexpr("[0-9.]+", mz))[[1]]))
        out$match_lo = v[1]
        out$match_hi = v[2]
    }

    out
}


#' Read SVC overlap/matching provenance for a set of `.sig` files
#'
#' \code{i_svc_sensor_info} scans the header of each file for the \code{factors=}
#' line and returns a canonical \code{sensor_info} data.frame (one row per file,
#' in the order given). This is cheap (a handful of header lines per file) and is
#' run unconditionally for SVC reads, independently of \code{extract_metadata}.
#'
#' @param file_paths character vector of `.sig` paths
#' @param max_header_lines how many header lines to scan (defaults to 40)
#' @return canonical sensor_info data.frame, \code{length(file_paths)} rows
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_svc_sensor_info = function(file_paths, max_header_lines = 40){
    n  = length(file_paths)
    df = i_new_sensor_info("svc", n)

    for(i in seq_len(n)){
        f_lines = trimws(readLines(file_paths[i], n = max_header_lines, warn = FALSE))
        line    = f_lines[grep("^factors=", f_lines)]
        p       = i_parse_svc_overlap(if(length(line) >= 1) line[1] else NA_character_)
        for(nm in names(p)){
            df[i, nm] = p[[nm]]
        }
    }
    df
}


#' Get the sensor / detector-splice provenance of a spectra object
#'
#' \code{sensor_info} returns the per-sample record captured at read time about
#' detector splicing: the vendor (\code{instrument}) and the two detector-splice
#' wavelengths (\code{splice_1}, \code{splice_2}), when the file records them. It
#' is captured by \code{\link{read_spectra}} and used by
#' \code{\link{match_sensors}} to find splice points without guessing.
#'
#' This provenance is \strong{read-only}: it is captured at read time and there
#' is deliberately no \code{sensor_info<-} setter (unlike \code{\link{bands}} or
#' \code{\link{meta}}). It is carried automatically through subsetting and
#' \code{\link{combine}}.
#'
#' @param x a spectra object
#' @return a data.frame with one row per sample (see the package's sensor_info
#'         columns), or \code{NULL} if no provenance was captured (e.g. objects
#'         built by hand, or read before this information was recorded)
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' dir_path = system.file("extdata", "Acer_example", package = "spectrolab")
#' spec     = read_spectra(dir_path, format = "sig")
#' sensor_info(spec)
sensor_info = function(x){
    attr(x, "sensor_info")
}


#' Recover a single set of splice points from sensor provenance
#'
#' \code{i_splice_from_provenance} returns the (unique) splice wavelengths
#' recorded across the samples of a sensor_info record, or NULL if none were
#' recorded (which is the case for SVC files saved in Preserve mode and for PSR
#' `.sed` files, whose headers do not carry crossover wavelengths).
#'
#' @param si a sensor_info data.frame, or NULL
#' @return numeric vector of splice wavelengths, or NULL
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_splice_from_provenance = function(si){
    if(is.null(si) || nrow(si) == 0){
        return(NULL)
    }

    sp = unique(stats::na.omit(c(si[["splice_1"]], si[["splice_2"]])))
    if(length(sp) == 0){
        return(NULL)
    }
    sort(as.numeric(sp))
}


#' Recover the vendor's magnitude-matching window from sensor provenance
#'
#' \code{i_match_window_from_provenance} returns the \code{c(lo, hi)} window over
#' which the instrument computed its detector-matching factor, when the file
#' recorded one (SVC's "Matching Type: ... @ 976 - 1010"). Returns NULL when the
#' record is absent or the samples disagree --- notably, SVC files saved in
#' "Preserve / Matching Type: None" mode (the usual raw case) carry no window, so
#' callers must fall back to the auto-detected one.
#'
#' @param si a sensor_info data.frame, or NULL
#' @return numeric \code{c(lo, hi)}, or NULL
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_match_window_from_provenance = function(si){
    if(is.null(si) || nrow(si) == 0 ||
       is.null(si[["match_lo"]]) || is.null(si[["match_hi"]])){
        return(NULL)
    }

    lo = unique(stats::na.omit(si[["match_lo"]]))
    hi = unique(stats::na.omit(si[["match_hi"]]))

    ## One window or nothing: samples read from files with different matching
    ## zones must not be silently collapsed into one.
    if(length(lo) != 1 || length(hi) != 1 || !is.finite(lo) || !is.finite(hi) || lo >= hi){
        return(NULL)
    }

    c(as.numeric(lo), as.numeric(hi))
}
