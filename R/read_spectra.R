#' Read files from various formats into `spectra`
#'
#' @param path Path to directory or input files.
#' @param format File format. Defaults to NULL so spectrolab tries to guess it
#'               from the file name. Alternatively, use "asd" for ASD; "sig"
#'               for SVC (Spectra Vista); or "sed" for PSR (Spectral Evolution)
#' @param type Data type to read. "target_reflectance", "target_radiance", or
#'             "reference_radiance". Defaults to "target_reflectance".
#' @param extract_metadata Boolean. Defaults to FALSE. Only implemented for the
#'                         Spectra Vista (.sig) and Spectral Evolution (.sed)
#'                         file types.
#' @param exclude_if_matches excludes files that match this regular expression.
#'                           Example: "BAD"
#' @param ignore_extension Boolean. If TRUE, the parser will try to read every
#'                         file in path regardless of the expected extension.
#' @return a single `spectra` or a list of `spectra` (in case files have
#'         incompatible band number or bands values)
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' dir_path = system.file("extdata", "Acer_example", package = "spectrolab")
#'
#' spec     = read_spectra(path = dir_path, format = "sig")
read_spectra = function(path,
                        format             = NULL,
                        type               = "target_reflectance",
                        extract_metadata   = FALSE,
                        exclude_if_matches = NULL,
                        ignore_extension   = FALSE) {

  path_and_format = i_verify_path_and_format(path               = path,
                                             format             = format,
                                             exclude_if_matches = exclude_if_matches,
                                             ignore_extension   = ignore_extension)

  path   = path_and_format$i_path
  format = path_and_format$i_format

  #############################################################
  ## Read spectra with the appropriate function
  #############################################################

  if(format == "sig"){

    if(type == "target_reflectance"){
      refl_cols   = 4
      sample_type = "target"
    } else if (type == "target_radiance") {
      refl_cols   = 3
      sample_type = "target"
    } else if (type == "reference_radiance") {
      refl_cols   = 2
      sample_type = "reference"
    } else {
      stop("type must be either target_reflectance, target_radiance or reference_radiance")
    }

    result = i_read_ascii_spectra(path,
                                  skip_until_tag    = "data=",
                                  sep_char          = "",
                                  header            = FALSE,
                                  wl_col            = 1,
                                  refl_cols         = refl_cols,
                                  divide_refl_by    = 100)

    if(extract_metadata){

      svc_meta_tags = c("name=", "instrument=", "integration=",
                        "scan method=", "scan coadds=", "scan time=",
                        "scan settings=", "optic=", "temp=",
                        "battery=", "error=", "units=", "time=",
                        "longitude=", "latitude=", "gpstime=",
                        "memory slot=", "inclinometer x offset=",
                        "inclinometer y offset=")

     metadata = i_read_ascii_metadata(file_paths  = path,
                                       sample_type = sample_type,
                                       sep_char    = ",",
                                       max_lines   = 40,
                                       meta_tags   = svc_meta_tags,
                                       tag_sep     = "=")
      meta(result) = metadata
    }

    return(result)
  }

  if(format == "sed"){

    if(type == "target_reflectance"){
      refl_cols      = c("Reflect. %", "Reflect. [1.0]")
      divide_refl_by = c(100, 1)
      sample_type    = "target"

    } else if (type == "target_radiance") {
      refl_cols      = "Rad. (Target)"
      divide_refl_by = 1
      sample_type    = "target"

    } else if (type == "reference_radiance") {
      refl_cols      = "Rad. (Ref.)"
      divide_refl_by = 1
      sample_type    = "reference"

    } else {
      stop("type must be either target_reflectance, target_radiance or reference_radiance")
    }

    result = i_read_ascii_spectra(path,
                                  skip_until_tag    = "Data:",
                                  sep_char          = "\t",
                                  header            = TRUE,
                                  wl_col            = "Wvl",
                                  refl_cols         = refl_cols,
                                  divide_refl_by    = divide_refl_by,
                                  check.names       = FALSE)

    if(extract_metadata){

      psr_meta_tags = c("Version:", "File Name:", "Instrument:", "Detectors:",
                        "Measurement:", "Date:","Time:", "Temperature (C):",
                        "Battery Voltage:", "Averages:", "Integration:", "Dark Mode:",
                        "Foreoptic:", "Radiometric Calibration:", "Units:", "band Range:",
                        "Latitude:", "Longitude:", "Altitude:", "GPS Time:", "Satellites:",
                        "Calibrated Reference Correction File:", "Channels:")

      metadata = i_read_ascii_metadata(file_paths  = path,
                                       sample_type = sample_type,
                                       sep_char    = ",",
                                       max_lines   = 40,
                                       meta_tags   = psr_meta_tags,
                                       tag_sep     = ":")
      meta(result) = metadata
    }

    return(result)
  }

  if(format == "asd"){
    result = i_read_asd_spectra(path,
                                type              = type,
                                divide_refl_by    = 1)
    return(result)
  }
}


#' Internal function to verify file paths and format
#'
#' @param path Path to directory or input files
#' @param format File format. Defaults to NULL so spectrolab tries to guess it
#'               from the file name. Alternatively, use "asd" for ASD; "sig"
#'               for SVC (Spectra Vista); or "sed" for PSR (Spectral Evolution)
#' @param exclude_if_matches excludes files that match this regular expression.
#'                           Example: "BAD"
#' @param ignore_extension Boolean. If TRUE, the parser will try to read every
#'                         file in path regardless of the expected extension.
#'
#' @return a list containing a vector of paths called `i_path` and a char
#'         with the file format called `i_format`
#'
#' @importFrom tools file_ext
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_verify_path_and_format = function(path,
                                    format             = NULL,
                                    exclude_if_matches = NULL,
                                    ignore_extension   = FALSE) {

  ##############################
  # Test if path exists
  ##############################
  i_path   = path
  f_exists = file.exists(i_path)

  ## Error out if at least one path don't exist
  if(!all(f_exists)){
    stop("Path(s) not found ", i_path[!f_exists])
  }

  ##############################
  # Test if path is a folder
  ##############################
  is_dir = dir.exists(i_path)


  if(any(is_dir)){

    ## Error out if:
    # user mixed file names and dir names
    if(any(!is_dir)){
      stop("Cannot mix directory and file paths.")
    }

    # user provided more than one directory
    if(length(is_dir) > 1 ){
      stop("Cannot read multiple directories at once. Please use a single directory as your path.")
    }

    ## Read the filenames
    i_path = dir(path        = i_path,
                 full.names  = TRUE)

    # Check if dir is empty
    if(length(i_path) == 0){
      stop("The directory is empty.")
    }

    # And make sure that folders are not included
    i_path = i_path[ ! dir.exists(i_path) ]

    # Check if dir is empty again
    if(length(i_path) == 0){
      stop("The directory only includes other directories. `path` should be the directory that includes the spectral files themselves.")
    }

  }

  #########################################
  ## Match formats
  #########################################

  file_extensions = tolower(tools::file_ext(i_path))

  format_lookup   = c("sig", "sed", "asd")


  if( ! is.null(format) ){

    format_match = pmatch(tolower(format), format_lookup)

    ## Error if format isn't found
    if(length(format_match) == 0){
      stop("Files did not match any known format")
    }

    format = format_lookup[format_match]

  } else {
    extensions = file_extensions[file_extensions %in% format_lookup]
    extensions = names(sort(table(extensions), decreasing = TRUE))

    if(length(extensions) > 1){
      stop("Multiple file formats found. Spectrolab can only read one file format at a time.")
    }

    if(length(extensions) == 0){
      stop("Files did not match any known format")
    }

    format = extensions[1]
  }


  if(! ignore_extension ){
    i_path = i_path[ grepl(format, file_extensions) ]

    ## error if no files of the right extension are found
    if(length(i_path) == 0){
      stop("No files have the extension ", format)
    }
  }


  #########################################
  ## filter files that match bad spectra
  #########################################
  if(!is.null(exclude_if_matches)){
    bad_tag = exclude_if_matches
    m       = grepl(paste(bad_tag,collapse = "|"), i_path)
    i_path  = i_path[!m]
  }

  ## error if bad spectra fiters out all file names
  if(length(i_path) == 0){
    stop("No paths left after removing bad spectra. Check your `exclude_if_matches` parameter")
  }


  ## Return
  list(i_path   = i_path,
       i_format = format)

}


#' Internal parser for ASCII format
#'
#' Generic parser for SVC's `.sig` and PSR's `.sed`
#'
#' @param file_paths paths for files already parsed by `read_spectra`
#' @param skip_until_tag Tag that precedes the table of value data,
#'                       indicating that lines until that tag should be skipped.
#'                       Tag is matched with a regexpr.
#' @param sep_char separator
#' @param header boolean. keep header?
#' @param wl_col idx or name of band column
#' @param refl_cols idx or name of value columns. MULTIPLE
#' @param divide_refl_by divide values by this. MULTIPLE
#' @param ... additional arguments passed to read table
#' @return single `spectra` or list of `spectra`
#'
#' @importFrom utils read.delim
#'
#' @keywords internal
#' @author Jose Eduardo Meireles and Anna Schweiger
i_read_ascii_spectra = function(file_paths,
                                skip_until_tag = NULL,
                                sep_char,
                                header,
                                wl_col,
                                refl_cols,
                                divide_refl_by,
                                ...){

  ############################################################
  ## Internal function to read table
  ############################################################

  parse = function(x, tag = skip_until_tag) {
    max_l = 40
    skip  = grep(tag, trimws(readLines(x, n = max_l)), fixed = TRUE)

    if(length(skip) == 1){
      return(utils::read.delim(x, skip = skip, sep = sep_char, header = header, check.names = FALSE))
    } else if (length(skip) == 0){
      stop(paste0("No match found for skip_until_tag in the first ", max_l, " lines"))
    } else {
      stop(paste0("More than one match found for skip_until_tag in the first ", max_l, " lines"))
    }
  }

  ############################################################
  ## Requirements and param checks

  if(length(refl_cols) > 1 && any(i_is_whole(refl_cols)) ){
    stop("refl_cols cannot be a vector of indices.")
  }

  ## Deal with cases where multiple value columns or multiple value
  ## scalars (divide_refl_by) are given
  if(length(refl_cols) < length(divide_refl_by)) {
    warning("Length of divide_refl_by should be either 1 or equals to the length of refl_cols. divide_refl_by has been prunned to length", length(refl_cols), ".")
    divide_refl_by = rep(divide_refl_by, length.out = length(refl_cols))
  }

  ############################################################
  ## Parse data
  ############################################################
  data        = lapply(file_paths, parse)
  names(data) = file_paths

  ############################################################
  ## Choose right value columns from parsed data
  ## Updates refl_cols to an INDEX
  if(length(refl_cols) > 1) {

    d = data[[1]]                                 ## OK to use the first file as representative of the whole thing?
    m = colnames(d) %in% refl_cols
    n = which(refl_cols %in% colnames(d))

    if(all( !m )){
      stop("refl_cols did not match any columns.")
    }

    if( sum(m) > 1 ){
      stop("refl_cols matched more than one column.")
    }

    # Update refl cols and divide by
    # subset 1st as a safeguard in case m matches more than one column
    refl_cols      = which(m)
    divide_refl_by = divide_refl_by[n]
  }


  ## there mabye files with different number of bands or band values
  ## check for them and split the data if needed
  wl_factor = unlist(
    lapply(data, function(x){
      paste0(x[ , wl_col], collapse = "")
    })
  )

  data = unname(split(data, wl_factor))

  ## Construct spectra
  spec = lapply(data, function(x) {
    rf = lapply(x, function(y){ y[ , refl_cols ] })
    rf = do.call(rbind, rf)
    rf = rf / divide_refl_by
    wl = x[[1]][ , wl_col ]
    nm = basename(names(x))

    spectra(rf, wl, nm)
  })

  if(length(spec) > 1){
    warning("Returning a list of `spectra` because some files had different number of bands or band values. If you want to make those data compatible, consider resampling (with resample) and then combining them (with combine)")
    return(spec)
  } else {
    return(spec[[1]])
  }
}

#' Read metadata
#'
#' @param file_paths paths
#' @param sample_type target or reference
#' @param max_lines max number of lines to read
#' @param sep_char separator of data within a field
#' @param meta_tags tags that match the metadata fields in the file
#' @param tag_sep char that separates the tags from the data in the file
#'
#' @importFrom utils type.convert
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_read_ascii_metadata = function(file_paths,
                                 sample_type,
                                 max_lines,
                                 sep_char,
                                 meta_tags,
                                 tag_sep){

  message("reading metadata may take a while...")

  mat = sapply(file_paths, function(x){

    f_lines   = trimws(readLines(x, n = max_lines))
    pick      = ifelse(test = sample_type == "target", yes = 2, no = 1)
    meta_tags = setNames(meta_tags, meta_tags)

    data = lapply(meta_tags, function(x){
      y = f_lines[grep(x, f_lines)]

      if(length(y) == 0){
        return(NULL)
      }

      y = strsplit(gsub(x, "", y), sep_char)[[1]]
      s = sort(rep( c(1,2) , length.out = length(y)))

      if(length(s) == 1){
        return(trimws(y))
      } else {
        return(trimws(split(y, s)[[pick]]))
      }
    })

    names(data) = gsub(tag_sep, "", names(data))

    unlist(data)

  }, USE.NAMES = FALSE)

  mat = as.data.frame( t(mat),
                       stringsAsFactors = FALSE,
                       row.names        = FALSE,
                       check.names      = FALSE)
  mat = lapply(mat, type.convert, as.is = TRUE)
  as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
}



#' Parser for ASD's `.asd`
#'
#' @param file_paths paths for files already parsed by `read_spectra`
#' @param type Data type to read. "target_refl", "target_rad", "reference_rad".
#'             Defaults to "target_refl".
#' @param format choice of ASD format. Either "binary" or "txt"
#' @param divide_refl_by divide values by this
#' @return spectra object
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_read_asd_spectra = function(file_paths,
                              type   = "target_reflectance",
                              divide_refl_by){

  ENDIAN      = "little"
  TYPES       = c("raw", "reflectance", "radiance", "no_units", "irradiance",
                  "qi", "transmittance", "unknown", "absorbance")
  DATA_FORMAT = c("numeric", "integer", "double", "unknown")

  result = lapply(file_paths, FUN = function(f){

    con = file(f, open = "rb")

    ####################
    # Data Type
    ####################

    seek(con, 186)
    data_type = readBin(con, integer(), size = 1)
    data_type = TYPES[data_type + 1L]

    ####################
    # Bands
    ####################

    seek(con, 191)
    band_start = readBin(con, numeric(), size = 4, endian = ENDIAN)

    seek(con, 195)
    band_step  = readBin(con, numeric(), size = 4, endian = ENDIAN)

    seek(con, 204)
    n_bands = readBin(con, integer(), size = 2, endian = ENDIAN)

    bands = seq(from = band_start,
                to   = band_start + n_bands * band_step - 1L,
                by   = band_step)

    ####################
    # Data Format
    ####################

    seek(con, 199)
    data_format = readBin(con, integer(), size = 1)
    data_format = DATA_FORMAT[data_format + 1L]

    ####################
    # Integration Time
    ####################

    seek(con, 390)
    integration_time = readBin(con, integer(), size = 4, endian = ENDIAN)

    ####################
    # SWIR Gain
    ####################

    seek(con, 436)
    swir1_gain = readBin(con, integer(), size = 2, endian = ENDIAN)

    seek(con, 438)
    swir2_gain = readBin(con, integer(), size = 2, endian = ENDIAN)

    ####################
    # Splice Bands
    ####################

    seek(con, 444)
    splice1 = readBin(con, numeric(), size = 4, endian = ENDIAN)

    seek(con, 448)
    splice2 = readBin(con, numeric(), size = 4, endian = ENDIAN)

    ####################
    # Spectrum &
    # White Reference
    ####################

    seek(con, where = 484)
    spectrum = readBin(con, what = data_format, n = n_bands, endian = ENDIAN)

    seek(con, 17710)
    comment_nchar = readBin(con, integer(), size = 2, endian = ENDIAN)

    seek(con, 17712 + comment_nchar)
    white_ref = readBin(con, what = data_format, n = n_bands, endian = ENDIAN)

    close(con)


    ########################################
    # Process
    ########################################

    if(data_type == "raw"){
      s1 = bands <= splice1
      s2 = bands > splice1 & bands <= splice2
      s3 = bands > splice2

      spectrum[s1] = spectrum[s1] / integration_time
      spectrum[s2] = spectrum[s2] * swir1_gain / 2048
      spectrum[s3] = spectrum[s3] * swir2_gain / 2048

      white_ref[s1] = white_ref[s1] / integration_time
      white_ref[s2] = white_ref[s2] * swir1_gain / 2048
      white_ref[s3] = white_ref[s3] * swir2_gain / 2048
    }

    relative_reflectance = spectrum / white_ref


    spec_name = gsub(pattern     = ".asd$",
                     replacement = "",
                     x           = basename(f),
                     ignore.case = TRUE)

    ########################################
    # Return
    ########################################

    if(type == "target_reflectance"){
      result = cbind(bands, relative_reflectance, spec_name)
    } else if (type == "target_radiance") {
      result = spectra(bands, spectrum, spec_name)
    } else if (type == "reference_radiance") {
      result = spectra(bands, white_ref, spec_name)
    } else {
      stop("type must be either target_reflectance, target_radiance or reference_radiance")
    }
  })

  # Wavelengths
  wl = lapply(result, function(x){ x[ , 1] })

  ## there mabye files with different number of bands or band values
  ## check for them and split the data if needed
  wl_factor = unlist(
    lapply(wl, function(x){
      paste0(x, collapse = "")
    })
  )

  data = unname(split(result, wl_factor))

  ## Construct spectra
  spec = lapply(data, function(x) {
    rf = lapply(x, function(y){ y[ , 2 ] })
    rf = do.call(rbind, rf)
    wl = x[[1]][ , 1 ]
    nm = sapply(x, function(y){ y[1 , 3 ] })
    spectra(rf, wl, nm)
  })

  if(length(spec) > 1){
    warning("Returning a list of `spectra` because some files had different number of bands or band values. If you want to make those data compatible, consider resampling (with resample) and then combining them (with combine)")
    return(spec)
  } else {
    return(spec[[1]])
  }
}
