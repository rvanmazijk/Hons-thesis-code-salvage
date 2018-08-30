# Richness & rasters

make_flora <- function(region_border = GCFR_border,
                       region_raster = agg_MAP_GCFR$`5`,
                       how_many_spp = 25,
                       how_many_occ = 100) {

    # Creates a specified amount of random occurence points, for each "species"
    # for the amount of species specified by running `populate_a_species()`
    # in a loop of that length.
    # All within the confines of a SpatialPolygonsDataframe provided.

    populate_a_species <- function(region_border = region_border,
                                   how_many_occ = how_many_occ,
                                   species_name = "sp1") {
        # Creates a specified amount of random occurence points,
        # thus representing a single "species", within the confines of a
        # SpatialPolygonsDataframe provided.
        a_species_occs <- spsample(
            region_border,
            how_many_occ,
            type = "random"
        )
        id_df <- data.frame(
            ID = seq_along(a_species_occs),
            species = species_name
        )
        a_species_occs %<>%
            SpatialPointsDataFrame(id_df) %>%
            na.omit() %>%
            spTransform(crs(region_border))
        return(a_species_occs)
    }

    # Run `populate_a_species()` in a loop.
    flora <- vector("list", length = how_many_spp)
    for (i in 1:how_many_spp) {
        flora[[i]] <- populate_a_species(
            region_border,
            how_many_occ,
            # Name sp column identically within a species
            species_name = paste0("sp", i)
        )
        # Name list's slots accordingly
        names(flora)[i] <- glue("sp{i}")
    }

    # Add all 1:how_many species' SpatialPointsDataframe together
    # (i.e. bind them into 1 object (e.g. a + b)).
    temp <- flora[[1]]
    for (i in 2:how_many_spp) {
        temp <- temp + flora[[i]]  # TODO: same as `temp %<>% add(flora[[i]])`?
    }
    flora <- temp

    return(flora)

}

get_cells <- function(flora_occs = "SpatialPointsDataframe",
                      region_raster = "RasterLayer") {
    # Get the cell-ID of the cell that a point falls in on the given raster,
    # so that this can be used as an INDEX in `calc_richness()`.
    cells <- region_raster %>%
        extract(SpatialPoints(flora_occs), cellnumbers = TRUE) %>%
        as.data.frame() %>%
        dplyr::select(cells)
    flora_occs@data %<>% cbind(cells)  # S4! f*** yeah!
    return(flora_occs)
}

calc_richness <- function(flora_occs = "SpatialPointsDataframe",
                          region_raster = "RasterLayer") {
    # Returns a raster & frequency table describing the number of unique
    # species occurences (i.e. the species richness) in cells on a raster,
    # when looking at a SpatialPointsDataframe that has cell-IDs based on
    # the same raster as that provided to this function.
    richness_raster <- region_raster
    richness_raster[] <- 0
    # (See count_occs() [DEPREC] for the logic of the frequency table below.)
    richness_by_cell <- tapply(
        X = flora_occs$species,
        INDEX = flora_occs$cells,
        FUN = function(x) length(unique(x))
    )
    richness_raster[as.numeric(names(richness_by_cell))] <- richness_by_cell
    return(list(
        freq_tab = richness_by_cell,
        raster = richness_raster
    ))
}

QDSpolydf2raster <- function(qds_shp = "SpatialPolygonsDataFrame",
                             region_border = "SpatialPolygonsDataFrame",
                             crs = std_CRS,
                             resolution = 0.25) {
    qds_shp@polygons %>%
        SpatialPolygons(proj4string = CRS(crs)) %>%
        rasterize(
            raster(resolution = resolution, crs = crs) %>%
                crop(region_border) %>%
                mask(region_border)
        )
}

QDSpolydf2pixels <- function(qds_shp = "SpatialPolygonsDataFrame",
                             region_border = "SpatialPolygonsDataFrame",
                             crs = std_CRS,
                             resolution = 0.25) {
    qds_shp@data %>%
        dplyr::select(lon, lat) %>%
        map(as.character) %>%
        map(as.numeric) %>%
        data.frame() %>%
        SpatialPoints(proj4string = CRS(crs)) %>%
        SpatialPixelsDataFrame(
            data = data.frame(
                qdgc = qds_shp@data$qdgc,
                blank = rep(0, length(qds_shp@data$qdgc))
            ),
            proj4string = CRS(crs)
        )
}

plot_pixelgrid <- function(x = "SpatialPixelsDataframe",
                           var = "blank",
                           add = FALSE) {
    plot(
        x[var],
        col = "white",
        border = "black",
        what = "image",
        scale = FALSE,
        add = add
    )
}

calc_richness_QDS <- function(flora_occs = "SpatialPointsDataframe",
                              qds_shp = "SpatialPolygonsDataFrame") {
    # Returns a raster & frequency table describing the number of unique
    # species occurences (i.e. the species richness) in qdgc cells,
    # when looking at a SpatialPointsDataframe that has cell-IDs based on
    # the same raster as that provided to this function.
    richness_raster <- region_raster
    richness_raster[] <- 0
    # (See count_occs() [DEPREC] for the logic of the frequency table below.)
    richness_by_cell <- tapply(
        X = flora_occs$species,
        INDEX = flora_occs$cells,
        FUN = function(x) length(unique(x))
    )
    richness_raster[as.numeric(names(richness_by_cell))] <- richness_by_cell
    return(list(
        freq_tab = richness_by_cell,
        raster = richness_raster
    ))
}

make_richness_raster <- function(flora_occs, region_raster, crs) {
    # For use on GBIF occ dfs (that I have cleaned using taxize::)
    flora_sp_pts_df <-
        SpatialPointsDataFrame(
            coords = flora_occs[, c("decimallongitude", "decimallatitude")],
            data = flora_occs[, c("species")],
            proj4string = crs(crs)
        ) %>%
        get_cells(region_raster)
    raster <- calc_richness(flora_sp_pts_df, region_raster)
    return(list(
        flora_sp_pts_df = flora_sp_pts_df,
        freq_tab = raster$freq_tab,
        raster = raster$raster
    ))
}
