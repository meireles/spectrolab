# estimate_fwhm_from_band_diff = function(bands, k = 0){
#     wave_diff = diff(bands)
#     wave_diff = c(wave_diff[1], wave_diff)
#     if(k == 0){
#         fwhm = wave_diff
#     } else {
#         clust = kmeans(wave_diff, k)
#         fwhm  = clust$centers[clust$cluster, 1]
#     }
#     names(fwhm) = bands
#     return(fwhm)
# }
#
# make_fwhm = function(old_wavelengths,
#                      old_sensor_fwhm,
#                      new_wavelengths,
#                      new_sensor_fwhm,
#                      effect_old_sensor){
#
#     # Standard deviation from FWHM
#     sigma0 = new_sensor_fwhm / (2 * sqrt(2 * log(2)))
#
#     # Resample the OLD sensor FWHM to the resolution of the NEW sensor
#     fwhm_old_new  = mapply(function(l, s){
#         k0 = dnorm(old_wavelengths, mean = l, sd = s)
#         sum(old_sensor_fwhm * k0) / sum(k0)
#     }, l = new_wavelengths, s = sigma0)
#
#     # Find a compromise between the provided new_sensor_fwhm and the
#     # fwhm_old_new estimates
#     fwhm_old_new + ( (1 - effect_old_sensor) * ( new_sensor_fwhm - fwhm_old_new) )
# }
#
# resample_spectrum = function(wavelengths,
#                              reflectance,
#                              new_wavelengths,
#                              fwhm) {
#
#     # Standard deviation from FWHM
#     sigma = fwhm / (2 * sqrt(2 * log(2)))
#
#     # Resample reflectance to new wavelengths
#     resampled_reflectance = mapply(function(l, s){
#         k = dnorm(wavelengths, mean = l, sd = s)
#         sum(reflectance * k) / sum(k)
#     }, l = new_wavelengths, s = sigma)
#
#     return(resampled_reflectance)
# }
#
