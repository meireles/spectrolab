## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse   = TRUE,
    comment    = "#>",
    fig.align  = "center",
    fig.retina = 2,
    dpi        = 150
)


## ----message = FALSE----------------------------------------------------------
library(spectrolab)
spec = as_spectra(spec_matrix_example, name_idx = 1)


## -----------------------------------------------------------------------------
long = to_long(spec)
dim(long)     # one row per sample x band: 50 samples * 2101 bands
head(long)


## -----------------------------------------------------------------------------
long_no_meta = to_long(spec, metadata = FALSE)
names(long_no_meta)


## ----eval = requireNamespace("tibble", quietly = TRUE)------------------------
tbl = tibble::as_tibble(spec)
tbl


## ----eval = requireNamespace("ggplot2", quietly = TRUE), fig.height = 3.5, fig.width = 6----
ggplot2::autoplot(spec) + ggplot2::theme_minimal()


## ----eval = requireNamespace("ggplot2", quietly = TRUE), fig.height = 3.5, fig.width = 6----
long = to_long(spec)

ggplot2::ggplot(long, ggplot2::aes(x = band, y = value, group = sample_name, color = sample_name)) +
    ggplot2::geom_line(show.legend = FALSE, alpha = 0.5) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "wavelength (nm)", y = "reflectance")

