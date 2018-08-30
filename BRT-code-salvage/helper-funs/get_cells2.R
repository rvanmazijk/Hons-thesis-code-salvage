get_cells2 <- function(flora_occs = "SpatialPointsDataframe",
                       region_raster = "RasterLayer") {
    # Get the cell-ID of the cell that a point falls in on the given raster,
    # so that this can be used as an INDEX in `calc_richness()`.
    cells <- region_raster %>%
        extract(SpatialPoints(flora_occs), cellnumbers = TRUE)
    flora_occs@data %<>% cbind(cells)  # S4! f*** yeah!
    return(flora_occs)
}
