#' bash_gdalinfo
#' Applies the `gdal_info` algorithm to a `.HDF` file, using `bash`
#' @param x A string, specifying the the `.HDF` file name
#' @param x_dir A string, the directory \code{x} is in
#' @param gdal_dir A string, the directory the `gdal_info` is in
#'                 (QGIS GDAL on my MacBook is the default)
#' @return The `gdal_info` output describing \code{x}
bash_gdalinfo <- function(x, x_dir = getwd(),
                          gdal_dir = paste0("/Library/Frameworks/",
                                            "GDAL.framework",
                                            "/Versions/1.11/Programs/")) {
    system(paste0(
        gdal_dir, "gdalinfo ",
        "\"", x_dir, x, "\""
    ))
}

#' bash_gdalinfo_one
#' Applies the `gdal_info` algorithm to a band in a `.HDF` file, using `bash`
#' @param x A string, specifying the the `.HDF` file name
#' @param prefix A string, e.g. "HDF4_EOS:EOS_GRID:", needed for the
#'               `gdal-info` algorithm to parse properly
#' @param band A string, e.g. ":MODIS_Grid_Month_6km_LST:Emis_20",
#'             specifying the band
#' @param x_dir A string, the directory \code{x} is in
#' @param gdal_dir A string, the directory the `gdal_info` is in
#'                 (QGIS GDAL on my MacBook is the default)
#' @return The `gdal_info` output describing the specified band \code{x}
bash_gdalinfo_one <- function(x, prefix = "HDF4_EOS:EOS_GRID:",
                              band, x_dir = getwd(),
                              gdal_dir = paste0("/Library/Frameworks/",
                                                "GDAL.framework",
                                                "/Versions/1.11/Programs/")) {
    system(paste0(
        gdal_dir, "gdalinfo ",
        prefix, "\"", x_dir, x, "\"", band
    ))
}
