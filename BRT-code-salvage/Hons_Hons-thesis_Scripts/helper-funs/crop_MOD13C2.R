crop_MOD13C2 <- function(file, filedir, outdir, suffix, box) {
    filename <- stringr::str_split_fixed(file, "tif", n = 2)[1]
    layer <- raster::raster(paste0(
        filedir, file
    ))
    layer <- raster::crop(layer, box)
    layer <- raster::mask(layer, box)
    raster::writeRaster(layer, filename = paste0(
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
