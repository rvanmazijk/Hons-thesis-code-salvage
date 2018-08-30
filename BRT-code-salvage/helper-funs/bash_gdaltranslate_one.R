#' bash_gdaltranslate_one
#' Applies `gdal_translate` to one band of some `.HDF` file in `bash`,
#' using the gdal library executables that QGIS uses,
#' in order to change the file format
#' @param x A string, specifying the the `.HDF` file name
#' @param prefix A string, e.g. "HDF4_EOS:EOS_GRID:", needed for the
#'               `gdal-info` algorithm to parse properly
#' @param band A string, e.g. ":MODIS_Grid_Month_6km_LST:Emis_20",
#'             specifying the band
#' @param out A string, specifying the desired output file---typically a `.tif`
#' @param x_dir A string, the directory \code{x} is in
#' @param out_dir A string, the directory \code{out} is to be put in
#' @param gdal_dir A string, the directory the `gdal_info` is in
#'                                 (QGIS GDAL on my MacBook is the default)
#' @return A converted `.HDF` band to, e.g., a `.tif`, in \code{out_dir}
bash_gdaltranslate_one <- function(x, prefix = "HDF4_EOS:EOS_GRID:",
                          band, out_format = ".tif", x_dir = getwd(), out_dir = getwd(),
                          gdal_dir = paste0("/Library/Frameworks/",
                                            "GDAL.framework",
                                            "/Versions/1.11/Programs/")) {
    out <- paste0(
        substr(x, 0, nchar(x) - 4),
        out_format
    )
    system(paste0(
        gdal_dir, "gdal_translate ",
        prefix, "\"", x_dir, x, "\"", band, " ",
        "\"", out_dir, out, "\""
    ))
}
