# Species occ pts to richness value raster

# Hons thesis
# Ruan van Mazijk

# created:      2017-07-22
# last edited:  2017-07-22


# Preamble ---------------------------------------------------------------------

# Here I attempt to teach myself how to turn lon-lat occ pts for an arbitrary
# set of species into species richness values on raster

# So, a SpatialPointsDataframe to a RasterLayer!

# I do this on randomy generated data, within the GCFR region, for practice
# purposes and to develope the method :)


# Setup ------------------------------------------------------------------------

rm(list = ls())
# The order in which these source() cmds are run is NB
source("Scripts/i_my_funs_4spaces.R")
source("Scripts/ii_my_objs.R")


# Simulated floral richness raster ---------------------------------------------

set.seed(57701)
sim_flora <- make_flora(
    region_border = GCFR_border,
    region_raster = agg_MAP_GCFR$`5`,  # fact 5 = QDS
    how_many_spp = 25,
    how_many_occ = 100
)
sim_flora %<>% get_cells(agg_MAP_GCFR$`5`)
sim_richness <- calc_richness(sim_flora, agg_MAP_GCFR$`5`)
plot(sim_richness$raster)
plot(GCFR_border, add = TRUE)
plot(terrain(sim_richness$raster, "roughness"))
plot(GCFR_border, add = TRUE)

# 2017-07-22 17:52 ---
# Seems to work well! yay!
# So, what I will do when I get REAL floral data for the GCFR and SWAFR is
# use `get_cells()` on the tidied SpatialPointsDataframe of those floral occs,
# and then use `calc_richness()` on that :)
# 'Til then: baiiiii


# Deprecated -------------------------------------------------------------------

if (FALSE) {

    populate_flora <- function(region_border = GCFR_border,
                               region_raster = agg_MAP_GCFR$`5`,
                               how_many_spp = 10,
                               how_many_occ = 10) {
        # ...
    }

    make_many_species_df <- function(region_border = region_border,
                                     how_many_spp = how_many_spp,
                                     how_many_occ = how_many_occ) {
        many_species_occs <- vector("list", length = how_many_spp)
        for (i in 1:how_many_spp) {
            many_species_occs[[i]] <- region_border %>%
                populate_a_species(how_many_occ) %>%
                as.data.frame()
            names(many_species_occs)[i] <- glue("sp{i}")
        }
        many_species_occs %<>%
            melt(id.vars = c("x", "y", "ID")) %>%
            dplyr::select("x", "y", "ID", "L1")
        names(many_species_occs) <- c("lon", "lat", "ID", "species")
        return(many_species_occs)
    }

    count_occs <- function(occs = many_species_occs,
                           region_raster = region_raster) {
        occ_freq_raster <- region_raster
        occ_freq_raster[] <- 0
        # "[...] tabulating the frequency of cell numbers represented by points,
        # then assigning these frequencies to the cells' values,
        # and finally extracting the cells' coordinates and values",
        # <https://stackoverflow.com/
        #       questions/32889531/
        #       r-how-can-i-count-how-many-points-are-in-each-cell-of-my-grid>,
        # Accessed on 2017-07-22 at 16:26.
        occ_freq_tab <- table(cellFromXY(
            object = region_raster,
            xy = many_species_occs
        ))
        occ_freq_raster[as.numeric(names(occ_freq_tab))] <- occ_freq_tab
        return(list(
            tab = occ_freq_tab,
            raster = occ_freq_raster
        ))
    }

    count_unique_occs <- function(occs = many_species_occs,
                                  region_raster = region_raster) {
        species <- levels(occ$species)
        for (i in 1:nlevels(occs$species)) {
            #count_occs(
            #    occs[occs$species == species[i], ],
            #    region_raster
            #)
        }
    }

}

# </> --------------------------------------------------------------------------

