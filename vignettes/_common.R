## -----------------------------------------------------------------------------
## Shared visual language for the spectrolab vignettes.
##
## Sourced by each vignette's setup chunk with `source("_common.R")`. This is
## vignette-only styling -- NOT part of the package API. Uses base grDevices and
## RColorBrewer (an Imports dependency); ggplot2 bits are guarded.
##
## Palette provenance: a CVD-validated categorical set (adjacent colorblind
## delta-E >= 12), a single-hue blue sequential ramp, and a blue<->red diverging
## ramp. See VIGNETTE_PLAN.md.
## -----------------------------------------------------------------------------

## ---- knitr / figure defaults ------------------------------------------------
## dpi = 150 (no fig.retina): crisp for HTML while keeping the built vignettes
## within CRAN's ~5 MB package limit. fig.retina = 2 renders every figure at 2x
## (4x the pixels) and pushed the tarball to ~8 MB, so it is deliberately off.
knitr::opts_chunk$set(
    collapse   = TRUE,
    comment    = "#>",
    fig.align  = "center",
    dpi        = 150,
    fig.width  = 7,
    fig.height = 4.2,
    out.width  = "100%",
    dev        = "png"
)

## ---- categorical palette (fixed order; never cycle past 8) ------------------
spec_pal = c("#2a78d6", "#1baf7a", "#eda100", "#008300",
             "#4a3aa7", "#e34948", "#e87ba4", "#eb6834")

## sequential (magnitude) and diverging (signed, e.g. PLSR coefficients)
spec_seq = grDevices::colorRampPalette(c("#cde2fb", "#256abf", "#0d366b"))
spec_div = grDevices::colorRampPalette(c("#2a78d6", "#f0efec", "#e34948"))

## ink / chrome
spec_ink   = "#0b0b0b"
spec_muted = "#898781"
spec_grid  = "#e1e0d9"
spec_other = "#b0b0b0"    # neutral fill for a folded "Other" group

## ---- spectral-region tints for plot_regions() -------------------------------
## Order matches default_spec_regions(): VIS, NIR, SWIR1, SWIR2. Kept very light
## so the data reads clearly on top.
spec_region_tints = c(
    VIS   = grDevices::adjustcolor("#2a78d6", 0.08),
    NIR   = grDevices::adjustcolor("#1baf7a", 0.08),
    SWIR1 = grDevices::adjustcolor("#eda100", 0.08),
    SWIR2 = grDevices::adjustcolor("#eb6834", 0.08)
)

## ---- grouping -> colors -----------------------------------------------------
## Resolve a per-sample grouping vector. `by` defaults to names(x) (how
## spec_matrix_example carries species); it also accepts a meta column name, or
## a vector/factor of length nrow(x).
i_group_vec = function(x, by = NULL){
    if(is.null(by)){
        return(as.character(names(x)))
    }
    if(is.character(by) && length(by) == 1L && by %in% names(meta(x))){
        return(as.character(meta(x)[[by]]))
    }
    as.character(by)
}

## Map a grouping to spec_pal, capping at `max_groups` (rest -> "Other"). Returns
## list(levels, pal = named color vector, cols = per-sample colors).
group_palette = function(x, by = NULL, max_groups = 8L){
    g   = i_group_vec(x, by)
    lev = unique(g)

    if(length(lev) > max_groups){
        keep = lev[seq_len(max_groups - 1L)]
        g    = ifelse(g %in% keep, g, "Other")
        lev  = c(keep, "Other")
    }

    pal            = stats::setNames(rep(spec_other, length(lev)), lev)
    non_other      = lev[lev != "Other"]
    pal[non_other] = spec_pal[seq_along(non_other)]

    list(levels = lev, pal = pal, cols = unname(pal[g]))
}

## Convenience: a per-sample color vector for plot.spectra(col = ...) / matplot.
species_cols = function(x, by = NULL, max_groups = 8L){
    group_palette(x, by, max_groups)$cols
}

## ---- ggplot2 theme (guarded) ------------------------------------------------
theme_spectra = function(base_size = 12){
    if(!requireNamespace("ggplot2", quietly = TRUE)){
        stop("ggplot2 is needed for theme_spectra().")
    }
    ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(color = spec_grid, linewidth = 0.3),
            axis.title       = ggplot2::element_text(color = spec_ink),
            plot.title       = ggplot2::element_text(color = spec_ink, face = "bold"),
            legend.position  = "right"
        )
}
