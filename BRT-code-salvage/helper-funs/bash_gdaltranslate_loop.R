#' bash_gdaltranslate_loop
#' Applies `gdal_translate` to one band of some each `.HDF` file in a list,
#' in `bash`, by calling on `bash_gdaltranslate_one()` i
#' n a loop through that list, using the gdal library executables that
#' QGIS uses, in order to change the file format
#' @param x A character vector, specifying the the `.HDF` file names
#' @param prefix A string, e.g. "HDF4_EOS:EOS_GRID:", needed for the
#'               `gdal-info` algorithm to parse properly
#' @param band A string, e.g. ":MODIS_Grid_Month_6km_LST:Emis_20",
#'             specifying the band
#' @param out_format A string, specifying the desired outputs'
#'                   file formats---default `.tif`
#' @param x_dir A string, the directory \code{x} is in
#' @param out_dir A string, the directory \code{out} is to be put in
#' @param gdal_dir A string, the directory the `gdal_info` is in
#'                 (QGIS GDAL on my MacBook is the default)
#' @return A converted batch of `.HDF` bands to, e.g., a `.tif`,
#'         in \code{out_dir}
bash_gdaltranslate_loop <- function(x, prefix = "HDF4_EOS:EOS_GRID:",
                           band, out_format = ".tif",
                           x_dir = getwd(), out_dir = getwd(),
                           gdal_dir = paste0("/Library/Frameworks/",
                                             "GDAL.framework",
                                             "/Versions/1.11/Programs/")) {
    if (not(is.atomic(x)) || not(is.character(x))) {
        warning("x must be an atomic (i.e. 1-D) character vector!")
        warning("e.g. `order$name` (NOT the data-frame `order`)")
    }
    for (i in seq_along(x)) {
        bash_gdaltranslate_one(
            x = x[i],
            band = band,
            out_format = out_format,
            x_dir = x_dir,
            out_dir = out_dir
        )
    }
}
