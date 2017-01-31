library("spectrolab")

path_input  = "data-raw/svc_raw_and_overlap_matched"
spectra     = read_spectra(path_input, "sig")

spectra_moc = spectra$`994`
spectra_raw = spectra$`1024`

all_greater_than = function(s, m){
    apply(reflectance(s), 1, function(x){ all(x > m) })
}

spectra_moc = spectra_moc[ ! all_greater_than(spectra_moc, 0.8), ]
spectra_raw = spectra_raw[ ! all_greater_than(spectra_raw, 0.8), ]


plot(spectra_moc, xlim = c(920, 1080))
plot(spectra_raw, add = T, col = "red")

