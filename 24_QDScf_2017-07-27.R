# Comparing my fact = 5 "QDS" resolution rasters to
#     "official" QDIS .shp's a la Larsen et al.
# ...

# Hons thesis
# Ruan van Mazijk

# created:      2017-07-27
# last edited:  2017-07-27


# Set up -----------------------------------------------------------------------

rm(list = ls())
# The order in which these source() cmds are run is NB
source("Scripts/i_my_funs_4spaces.R")
source("Scripts/ii_my_objs.R")


# Import Larsen QDS .shp's -----------------------------------------------------

# Use lvl 2 bc that is 0.25 x 0.25 deg (i.e. QDS s.s.)

GCFR_QDS <- here::here("Data", "qdgc_zaf") %>%
    readOGR(layer = "qdgc_02_zaf") %>%
    crop(GCFR_border_buffered)
proj4string(GCFR_QDS) == std_CRS

SWAFR_QDS <- here::here("Data", "qdgc_aus") %>%
    readOGR(layer = "qdgc_02_aus") %>%
    crop(SWAFR_border_buffered)
proj4string(SWAFR_QDS) == std_CRS


# Compare those and my agg(rasters) --------------------------------------------
# Do they line up?

plot(agg_MAP_GCFR$`5`)
plot(GCFR_QDS, add = TRUE)
plot(agg_MAP_SWAFR$`5`)
plot(SWAFR_QDS, add = TRUE)

# My agg(raster)s' grids are /slightly/ skew,
# but that's not too much of a worry!
# But just to be safe, I will use these Larsen QDSs for my richness rasters
# from now on :)


# Convert Larsen QDS shapes into rasters with those grids ----------------------

GCFR_QDS_raster <- GCFR_QDS %>%
    QDSpolydf2raster(region_border = GCFR_border_buffered)
GCFR_QDS_pixels <- GCFR_QDS %>%
    QDSpolydf2pixels()
plot_pixelgrid(GCFR_QDS_pixels)
plot(rasterToPolygons(GCFR_QDS_raster), border = "black", add = TRUE)
plot(GCFR_QDS, add = TRUE)
# All aligned! Yay!

SWAFR_QDS_raster <- SWAFR_QDS %>%
    QDSpolydf2raster(region_border = SWAFR_border_buffered)
SWAFR_QDS_pixels <- SWAFR_QDS %>%
    QDSpolydf2pixels()
plot_pixelgrid(SWAFR_QDS_pixels)
plot(rasterToPolygons(SWAFR_QDS_raster), border = "black", add = TRUE)
plot(SWAFR_QDS, add = TRUE)
# All aligned! Yay!

# Will use the raster, then, as my get_cells() fn can handle them :)
# Continue in `23_GBIF_work_mergeHWPPC_2017-07-25.R`


# </> --------------------------------------------------------------------------

