# Imports `.grd` raster band-by-band into a RasterStack class object

import_raster_stack <- function(what = c("rainfall", "temperature"),
                                file,
                                n_bands = c(36, 12, 434)) {
    out <- stack()
    for (i in 1:n_bands) {
        x <- raster(band = i, here::here(
            "Data",
            "derived-data",
            what,
            file
        ))
        out %<>% stack(x)
    }
    return(out)
}
