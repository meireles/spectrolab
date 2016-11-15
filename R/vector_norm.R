##############################################
###### Vector normalisation #################
##### 10-20-2016 ############################

##### Normalise spectra ############
normalize.spectra = function(x) {
  x <- as.matrix(x)
  magnitudes <- sqrt(rowSums(x*x))
  return(trans_spec <- as.data.frame(x/magnitudes))
}
