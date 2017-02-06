# Introduction to spectrolab
Jose Eduardo Meireles and Anna K. Schweiger  

The goal of `spectrolab` is to provide 1) functions to read, process and visualize data from portable spectrometers as well as 2) a common interface that other packages can build on. The package introduces a `spectra` S3 class and implements various methods to read spectral files, access and subset data, perform tasks such as vector normalization, smoothing, and sensor overlap matching, plotting spectra.

The source code can be found on our [GitHub repository](https://github.com/annakat/spectrolab). Please report any bugs and ask us your questions through the [issue tracker](https://github.com/annakat/spectrolab/issues).


## Installing and loading `spectrolab`

The best way to get `spectrolab` is to install it directly from GitHub. You will need the `devtools` package to do it.


```r
library("devtools")
install_github("annakat/spectrolab")
```

Assuming that everything went smoothly, you should be able to load `spectrolab` like any other package.


```r
library("spectrolab")
```

## Reading spectra

There are two ways to get spectra into R: 1) converting a matrix or data.frame to `spectra` or 2) reading spectra from raw data files (formats: SVC's `sig`, Spectral Evolution's `sed` and ASD's `asd`). Here are a couple examples:

### Create spectra from a matrix or data.frame

If you already have your spectra in a matrix or data frame (e.g. when you read your data from a .csv file), you can use the function `as.spectra()` to convert it to a `spectra` object. The matrix **must** have samples in rows and wavelengths in columns. The header of the wavelengths columns must be (numeric) wavelength labels. You also should declare which column has the sample names (which are mandatory) using the `name_idx` argument. If other columns are present (other than sample name and reflectances), their indices must be passed to `as.spectra` as the `meta_idxs` argument.

Here is an example using a dataset matrix named `spec_matrix_meta.csv` provided by the package.


```r
dir_path = system.file("extdata/spec_matrix_meta.csv", package = "spectrolab")
spec_csv = read.csv(dir_path, check.names = F)

# The sample names are in column 3. Columns 1 and 2 are metadata
achillea_spec = as.spectra(spec_csv, name_idx = 3, meta_idxs = c(1,2) )

# And now you have a spectra object with sample names and metadata...
achillea_spec
```

```
## spectra object 
## number of samples: 10 
## wavelengths: 400 to 2400 (2001 bands)
## metadata (2): ident, ssp
```


### Reading spectra: example with SVC's `.sig` files

You shoud use the function `read_spectra()` to read raw spectra files. You can pass a vector of filenames to `read_spectra()`, but it is usually easier to pass the path to the folder where your data are.


```r
# `dir_path` is the directory where our example datasets live
dir_path = system.file("extdata", "Acer_example", package = "spectrolab")

# Read .sig files
acer_spectra = read_spectra(path = dir_path, format = "sig")
```

It may be the case that file names of undesirable spectra where flagged in the field, e.g with "_WR" (white reference) and "_BAD" (bad measurements). You can avoid importing such files by passing those flags to the argument `exclude_if_matches` in `read_spectra()`.


```r
# use the `exclude_if_matches` argument to excluded flagged files
acer_spectra = read_spectra(path = dir_path, format = "sig", exclude_if_matches = c("BAD","WR"))
```

Flagging "unusual" measurements during data collection speeds up the dataset cleanup process. However, you can also exclude bad spectra and outliers by subsetting your spectra object, as shown in the section **Subsetting spectra** further below.

## Inspecting and querying spectra

You can check out your spectra object in several ways. For instance, You may want to know how many spectra and how many bands are in there, retrieve the filenames. Of course you will need to plot the data, but that topic gets its own section further down.


```r
# Simply print the object
acer_spectra
```

```
## spectra object 
## number of samples: 7 
## wavelengths: 340.5 to 2522.8 (1024 bands, **overlap not matched**)
## metadata: none
```

```r
# Get the dataset's dimensions
dim(acer_spectra)
```

```
##     n_samples n_wavelengths 
##             7          1024
```

`spectrolab` also lets you access the individual components of the `spectra`. This is done with the functions `names()` for sample names, `wavelengths()` for wavelength labels, `reflectance()` for the reflectance matrix, and `meta()` for the associated metadata (in case you have any).



```r
# Vector of all sample names. Note: Duplicated sample names are permitted
n = names(achillea_spec)

# Vector of wavelengths
w = wavelengths(achillea_spec)

# Reflectance matrix
r = reflectance(achillea_spec)

# Metadata. Use simplify = TRUE to get a vector instead of a data.frame
m = meta(achillea_spec, "ssp", simplify = TRUE)
```

## Subsetting spectra

You can subset the `spectra` using a notation *similar* to the `[i,j]` function used in matrices and data.frames. The first argument in `[i, ]` matches *sample names*, whereas the second argument `[ ,j]` matches the *wavelength names*. Here are some examples of how `[` works in `spectra`:

  - `x[1:3, ]` will keep the first three samples of `x`, i.e. `1:3` are indexes.
  - `x["sp_1", ]` keeps **all** entries in `x` where sample names match `"sp_1"`
  - `x[ ,800:900]` will keep wavelengths between `800` and `900`.
  - `x[ ,1:5] ` will **fail**!. *wavelengths __cannot__ be subset by indexes!*

Subsetting lets you, for instance, exclude noisy regions at the beginning and end of the spectrum or limit the data to specific entries.


```r
# Subset wavelength regions.
spec_sub_vis = achillea_spec[ , 400:700 ]

# Subset spectra to all entries where sample_name matches "ACHMI_7" or
# get the forst three samples
spec_sub_byname = achillea_spec["ACHMI_7", ]
spec_sub_byidx  = achillea_spec[ 1:3, ]
```

The resolution of some spectra may be different from 1nm, as is the case with SVC. In those cases, the best way to subset spectra is using the `min` and `max` argguments for wavelengths:


```r
acer_spectra_trim = acer_spectra[ , wavelengths(acer_spectra, 400, 2400) ]
```

Note that you can (1) subset samples using indexes and (2) use characters or numerics to subset wavelengths. As said before, you cannot use indexes to subset wavelengths though.


```r
# Subsetting samples by indexes works and so does subsetting wavelengths by numerics or characters.
spec_sub_byidx[1, "405"] == spec_sub_byidx[1, 405]
```

```
## ACHMI_1 
##    TRUE
```

*But remember that you CANNOT use indexes to subset wavelengths!*


```r
# Something that is obvioulsy an index, like using 2 instead of 401 (the 2nd band 
# in our case), will fail.
spec_sub_byidx[ , 2]
```

```
## Error in i_match_label(wavelengths(x), j): Following labels not found: 2
```

```r
`Error in i_match_ij_spectra(this = this, i = i, j = j) : Wavelength subscript out of bounds. Use wavelength labels instead of raw indices.`
```

```
## Error in eval(expr, envir, enclos): object 'Error in i_match_ij_spectra(this = this, i = i, j = j) : Wavelength subscript out of bounds. Use wavelength labels instead of raw indices.' not found
```


## Plotting

The workhorse function for plotting `spectra` is `plot()`. It will jointly plot each spectrum in the `spectra` object. You should be able to pass the usual plot arguments to it, such as `col`, `ylab`, `lwd`, etc.

You can also plot the quantile of a `spectra` object with `plot_quantile()`. It's second argument, `total_prob`, is the total "mass" that the quantile encompasses. For instance, a `total_prob = 0.95` covers 95% of the variation in the `spectra` object, i.e. it is the `0.025 to 0.975` quantile. The quantile plot can stand alone or be added to a current plot if `add = TRUE`.

Last but not least, you can also shade spectral regions with the `plot_regions()` function. `spectrolab` provides a `default_spec_regions()` matrix as an example, but you obviously can customize it for your needs.


```r
# Simple spectra plot
par(mfrow = c(1, 3))
plot(achillea_spec, lwd = 0.75, lty = 1, col = "grey25", main = "All Spectra")

# Stand along quantile plot
plot_quantile(achillea_spec, total_prob = 0.8, col = rgb(1, 0, 0, 0.5), lwd = 0.5, border = TRUE)
title("80% spectral quantile")

# Combined individual spectra, quantiles and shade spectral regions
plot(achillea_spec, lwd = 0.25, lty = 1, col = "grey50", main="Spectra, quantile and regions")
plot_quantile(achillea_spec, total_prob = 0.8, col = rgb(1, 0, 0, 0.25), border = FALSE, add = TRUE)
plot_regions(achillea_spec, regions = default_spec_regions(), add = TRUE)
```

![](introduction_to_spectrolab_files/figure-html/dev_svg-1.png)<!-- -->


## Manipulating samples names, wavelength labels, metadata and reflectance

You may want to edit certain simple attributes of `spectra`, such as making all sample names lowecase This is easily attainable in `spectrolab`.


```r
spec_new = achillea_spec

# Replace names with a lowercase version
names(spec_new) = tolower(names(achillea_spec))

# Check the results
names(spec_new)[1:5]
```

```
## [1] "achmi_1" "achmi_2" "achmi_3" "achmi_4" "achmi_5"
```

If you want to fiddle with the reflectance itself, this is easy, too.

```r
# Scale reflectance by 0.75
spec_new = spec_new * 0.75

# Plot the results
plot(achillea_spec, col = "blue", lwd = 0.75, cex.axis = 0.75)
plot(spec_new, col = "orange", lwd = 0.75, add = TRUE)
```

<img src="introduction_to_spectrolab_files/figure-html/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

Or you can also edit or add new metadata to the `spectra` object.

```r
## Adding metadata to a spectra object: a dummy N content
n_content = rnorm(n = nrow(achillea_spec), mean = 2, sd = 0.5)
meta(achillea_spec, label = "N_percent") = n_content
```


### Converting a `spectra` object into a matrix or data.frame

It is also possible to convert a `spectra` object to a matrix or data.frame using the `as.matrix()` or `as.data.frame()` functions. This is useful if you want to export your data in a particular format, such as .csv. 

If you're converting spectra to a matrix, `spectrolab` will (1) place wavelengths in columns, assigning wavelength labels to `colnames`, and (2) samples in rows, assigning sample names to `rownames`. Since `R` imposes strict rules on column name formats and sometimes on row names, `as.matrix()` will try to fix potential dimname issues if `fix_names != "none"`. Note that `as.matrix()` will not keep metadata.

Conversion to data.frame is similar, but keeps the metadata.


```r
# Make a matrix from a `spectra` object
spec_as_mat = as.matrix(achillea_spec, fix_names = "none")
spec_as_mat[1:4, 1:3]
```

```
##                400        401        402
## ACHMI_1 0.03734791 0.03698631 0.03804012
## ACHMI_2 0.04608409 0.04536371 0.04436544
## ACHMI_3 0.04058113 0.04025678 0.03958125
## ACHMI_4 0.04063730 0.03993420 0.03793455
```

```r
# Make a matrix from a `spectra` object
spec_as_df = as.data.frame(achillea_spec, fix_names = "none")
spec_as_df[1:4, 1:5]
```

```
##   sample_name ident                   ssp N_percent        400
## 1     ACHMI_1 10526 Achillea millefolium   2.080107 0.03734791
## 2     ACHMI_2 10527 Achillea millefolium   3.112791 0.04608409
## 3     ACHMI_3 10528 Achillea millefolium   1.437576 0.04058113
## 4     ACHMI_4 10529 Achillea millefolium   2.442087 0.04063730
```