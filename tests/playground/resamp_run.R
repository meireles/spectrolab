# library("cluster")
#
# s_raw = read_spectra("inst/extdata/svc_raw_and_overlap_matched_serbin/SVC_Files_moc/")
# s     = as.matrix(s_raw)[1, ]
# b     = bands(s_raw)
# n     = seq(400, 2400, 1)
# f     = 4
#
# r1     = make_fwhm(old_wavelengths  = b,
#                   old_sensor_fwhm   = estimate_fwhm_from_band_diff(b, 3),
#                   new_wavelengths   = n,
#                   new_sensor_fwhm   = f,
#                   effect_old_sensor = 0)
#
# r2     = make_fwhm(old_wavelengths   = b,
#                    old_sensor_fwhm   = estimate_fwhm_from_band_diff(b, 3),
#                    new_wavelengths   = n,
#                    new_sensor_fwhm   = f,
#                    effect_old_sensor = 0.5)
#
# r3     = make_fwhm(old_wavelengths   = b,
#                    old_sensor_fwhm   = estimate_fwhm_from_band_diff(b, 3),
#                    new_wavelengths   = n,
#                    new_sensor_fwhm   = f,
#                    effect_old_sensor = 1)
#
#
# r_sc1 = resample_fwhm(wavelengths = b, reflectance = s, new_wavelengths = n, fwhm = r1)
# r_sc2 = resample_fwhm(wavelengths = b, reflectance = s, new_wavelengths = n, fwhm = r2)
# r_sc3 = resample_fwhm(wavelengths = b, reflectance = s, new_wavelengths = n, fwhm = r3)
#
#
# lwd = 0.1
# plot(b, s, type = "l", lwd = lwd, col = "black")
#
# lines(n, r_sc1, col = "orange", lwd = lwd)
# #lines(n, r_sc2, col = "purple", lwd = lwd)
# #lines(n, r_sc3, col = "blue", lwd = lwd)
