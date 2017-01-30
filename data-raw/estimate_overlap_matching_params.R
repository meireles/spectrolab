library("spectrolab")
library("distr")

path_input = "data-raw/svc_raw_and_overlap_matched/"
spectra    = read_spectra(path_input, "sig")

spectra$`994`[ spectra$`994` < 0.8 ]

f = apply(reflectance(spectra$`994`), 1, function(x){all(x > 0.8)})

p = normalize(spectra$`994`[ f, ])

