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

x = spectra_raw


#
# i = 1:5
# plot(spectra_moc[i , ], xlim = c(920, 1080))
# plot(spectra_raw[i , ], add = T, col = "red")
#
# w1 = wavelengths(spectra_raw)[1:512]
# w2 = wavelengths(spectra_raw)[513:768]
# w3 = wavelengths(spectra_raw)[769:1024]
#
# #plot(spectra_moc[i , ], xlim = c(1880, 1920), type = "p", pch = 1)
# plot(spectra_moc[i , ], xlim = c(970, 1020), type = "p", pch = 16, ylim = c(0.4, 0.6))   #ylim = c(0.45, 0.48)
# plot(spectra_raw[i , w1], add = T, col = "red", type = "p", pch = 2)
# plot(spectra_raw[i , w2], add = T, col = "blue", type = "p", pch = 3)
#
#
# s = cummin(rev(wavelengths(spectra_raw)))
# p = which(rev(rev(wavelengths(spectra_raw)) - s) > 0)

