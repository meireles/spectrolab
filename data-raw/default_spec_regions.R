library(devtools)

default_spec_regions = cbind("VIS"   = c(begin = 400,  end = 700),
                             "NIR"   = c(begin = 800,  end = 1300),
                             "SWIR1" = c(begin = 1550, end = 1800),
                             "SWIR2" = c(begin = 2000, end = 2400))

devtools::use_data(default_spec_regions, overwrite = TRUE)
