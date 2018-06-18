library(PEcAnRTM)
library(devtools)
library(spectrolab)

n = 50
params = matrix( c(rnorm(n, 1.2, 0.05),
                     rnorm(n, 30.0, 1),
                     rnorm(n, 10.0, 0.1),
                     rnorm(n, 0.015, 0.001),
                     rnorm(n, 0.009, 0.00001)
                     ), nrow = n)

f = function(x) {
    y = PEcAnRTM::prospect(x, version = "5", include.wl = FALSE)
    y[ , 1]
}

refl           = t(apply(params, 1, f))
colnames(refl) = 400:2500
names          = paste("species", 1:10, sep = "_")
sampl_names    = sample(names, size = n, replace = TRUE)

spec_matrix_example = cbind(species = sampl_names, refl)

spec_with_jump               = as.spectra(spec_matrix_example, name_idx = 1)[1, ]
spec_with_jump[ , 400:970]   = spec_with_jump[ , 400:970]   * 1.08
spec_with_jump[ , 1910:2500] = spec_with_jump[ , 1910:2500] * 1.5

devtools::use_data(spec_matrix_example, overwrite = TRUE)
devtools::use_data(spec_with_jump, overwrite = TRUE)
