project_MOD13C2 <- function(file, crs, filedir, outdir, suffix) {
    filename <- stringr::str_split_fixed(file, "tif", n = 2)[1]
    layer <- raster::raster(paste0(
        filedir,
        file
    ))
    layer <- raster::projectRaster(layer, crs = crs)
    raster::writeRaster(layer, paste0(
        outdir,
        filename,
        suffix,
        ".tif"
    ))
    message(appendLF = FALSE, glue::glue("
        {filename}_{suffix} saved \\
        \x20 \x20 \x20 \x20 \x20 \x20 \x20 \x20 \x20 \x20 \\
        \r
    "))
}
