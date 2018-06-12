# Practicing manipulating rasters w/ CHIRPS-v2.0.2016 in the GCFR

# Hons thesis
# Ruan van Mazijk

# created:      2017-04-14
# last edited:  2017-04-18


# (Relevant) contents of this script ----------------------------------------------------

# - Vegmap polygons from CCAB_current_biome (SANParks) (~ SANBI BGIS)
# - Learnt how to mask & crop an environmental raster (CHIRPS) to the GCFR
# - Chose a CRS (the defaults for all my data are the same! yay!)


# Setup ---------------------------------------------------------------------------------

rm(list = ls())
require(pacman)
p_load(magrittr, raster, rasterVis, sp, ggplot2, rgdal)

datwd <- "/users/ruanvanmazijk/Documents/R/Hons_thesis/Data/"
giswd <- "/Volumes/RUAN'S HDD/GIS/"
reswd <- "/users/ruanvanmazijk/Documents/R/Hons_thesis/Results/"


# /CRS ----------------------------------------------------------------------------------

std_CRS <- "+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
# Have yet to std'ise the CRS for all the stuff below here.
# Also not sure if `stdCRS` here is best?


# Region polygons -----------------------------------------------------------------------

# Get country polygons
# (do I need this? Nope!)

#getData('ISO3')
#SA_border <- getData(
#  'GADM',
#  country = c('ZAF'),
#  level = 0,
#  path = paste0(giswd, "GADM")
#)
#AU_border <- getData(
#  'GADM',
#  country = c('AUS'),
#  level = 0,
#  path = paste0(giswd, "GADM")
#)

# Get vegmap polygons. Ways/sources I know of (for SA):

# 1. NVM (Jasper's way) (~ SANBI BGIS)

#SA_vegmap_nvm <- readOGR(
#  dsn = "/users/ruanvanmazijk/desktop/nvm2012beta2_wgs84_Geo/",
#  layer = "nvm2012beta2_wgs84_Geo"
#)
#summary(SA_vegmap_nvm)
#plot(SA_vegmap_nvm[SA_vegmap_nvm$BIOME == "Fynbos" |
#                     SA_vegmap_nvm$BIOME == "Succulent Karoo", ])

# 2. vegm2006_reclass (Mike's way)

#SA_vegmap_rc <- readOGR(
#  dsn = "/users/ruanvanmazijk/desktop/Vegmap_2006_reclass/",
#  layer = "vegm2006_reclass"
#)
#summary(SA_vegmap_rc)
#plot(SA_vegmap_rc)

# Reclass takes aaaages longer to plot than NVM ... 
# is actually much more detailed than NVM it seems.

# 3. vegm2006_biomes (~ SANBI BGIS)

# Have to put vegmap data on MacBook, bc the readOGR fn doesn't seem to like my HDD

SA_vegmap_biomes <- readOGR(
  dsn = paste0(datwd, "vegm2006_biomes_withforests/"),
  layer = "vegm2006_biomes_withforests"
)
summary(SA_vegmap_biomes)
GCFR <- SA_vegmap_biomes[SA_vegmap_biomes$BIOMECODE == "F" |
                           SA_vegmap_biomes$BIOMECODE == "SK",]
plot(GCFR)
extent(GCFR)

# This works really well! (so far...)

# 4. CCAB_current_biome (SANParks) (~ SANBI BGIS)

# Have to put vegmap data on MacBook, bc the readOGR fn doesn't seem to like my HDD

SA_vegmap_CCAB <- readOGR(
  dsn = paste0(datwd, "CCAB_current_biome/"),
  layer = "Current_biome"
)
summary(SA_vegmap_CCAB)
SA_vegmap_CCAB_narm <- SA_vegmap_CCAB[1:9, ] # removes this one pesky NA
GCFR_CCAB <- SA_vegmap_CCAB_narm[SA_vegmap_CCAB_narm$LA_CURRENT == "Fynbos" |
                                   SA_vegmap_CCAB_narm$LA_CURRENT == "Succulent Karoo", ]
plot(GCFR_CCAB)
extent(GCFR_CCAB)
extent(GCFR)

# Seems to be just as good as no. 3...
# only that it is less detailed, and differs minutely on the exact extent.


# Importing CHIRPS 2016 -----------------------------------------------------------------

(chirps_2016 <- raster(paste0(giswd, "CHIRPS_v2.0/global_annual/chirps-v2.0.2016.tif")))
extent(chirps_2016)

# extent.SA <- c(15, 35, -35, -25)
# chirps.2016.SA <- crop(chirps.2016, extent.SA)
# plot(chirps.2016)
# plot(chirps.2016.SA)
# 
# chirps.2016.SA.narm <- chirps.2016.SA
# chirps.2016.SA.narm[chirps.2016.SA.narm[] < 0] <- NA
# 
# chirps.2016.SA; chirps.2016.SA.narm
# plot(chirps.2016.SA.narm)

# proj4string(chirps.2016)
# # wrong CRS/projection --> needs to change:
# chirps.2016 %<>% projectRaster(crs = CRS(stdCRS))
# proj4string(chirps.2016)
# writeRaster(chirps.2016, "chirps-v2.0.2016_UTM.tif", format = "GTiff")

# Importing a DEM -----------------------------------------------------------------------

SA.elev <- getData("alt", country = "ZAF")
#AU.elev <- getData("alt", country = "AUS")

# vs

SA.dem <- getData(
  'SRTM',
  lon  = 19,
  lat  = -32.5,
  path = paste0(giswd, "SRTM/")
)
dem <- raster(paste0(giswd, "SRTM/srtm_40_19.tif"))


# Mask & crop raster to study region ----------------------------------------------------

# Clip to polygon boundary
#chirps_2016_GCFR_poly <- mask(chirps_2016, GCFR)
#plot(chirps.2016.GCFR.poly)

# Then crop to extent
#chirps_2016_GCFR_extent <- crop(chirps_2016_GCFR_poly, GCFR)
#plot(chirps_2016_GCFR_extent)

# Which is succinctly:

chirps_2016 %>%
  mask(GCFR) %>%
  crop(GCFR) ->
  chirps_2016_GCFR


# Calculations (fiddling) ---------------------------------------------------------------

#chirps.2016.GCFR.extent.roughness <- terrain(chirps.2016.GCFR.extent, "roughness")
#plot(chirps.2016.GCFR.extent.roughness)
#plot(chirps.2016.GCFR.extent.roughness[] ~ chirps.2016.GCFR.extent[])
#
#(roughness.chirps.2016.SA.narm <- terrain(chirps.2016.SA.narm, "roughness"))
#plot(roughness.chirps.2016.SA.narm)
#plot(roughness.chirps.2016.SA.narm[] ~ chirps.2016.SA.narm[])
#m1 <- lm(roughness.chirps.2016.SA.narm[] ~ chirps.2016.SA.narm[])
#abline(m1, col = "red")
#summary(m1)
#new <- data.frame(`chirps.2016.SA.narm[]` = seq(0.025, 2000, by = 0.025))
#CI <- predict(m1, newdata = new, interval = "confidence")
#lines(chirps.2016.SA.narm[], CI[, 2], col = "red")
#lines(chirps.2016.SA.narm[], CI[, 3], col = "red")
#PI <- predict(m1, newdata = new, interval = "prediction")
#lines(chirps.2016.SA.narm[], PI[, 2], col = "red")
#lines(chirps.2016.SA.narm[], PI[, 3], col = "red")
#summary(CI)
#summary(PI)


# Choosing a vegmap: comparing GCFR & GCFR_CCAB -----------------------------------------

par(mfrow = c(1, 2))
plot(GCFR)
plot(GCFR_CCAB)
par(mfrow = c(1, 1))

#chirps.2016.GCFR.poly <- mask(chirps.2016, GCFR)
#chirps.2016.GCFR.extent <- crop(chirps.2016.GCFR.poly, GCFR)
#
#chirps.2016.GCFR.CCAB.poly <- mask(chirps.2016, GCFR.CCAB)
#chirps.2016.GCFR.CCAB.extent <- crop(chirps.2016.GCFR.poly, GCFR.CCAB)
#
#par(mfrow = c(1, 2))
#plot(chirps.2016.GCFR.extent); plot(chirps.2016.GCFR.CCAB.extent)
#par(mfrow = c(1, 1))

# Gonna go with GCFR.CCAB bc seems to be more efficient,
# and there is largely no difference anyway :P --> for now!


# Choosing a CRS ------------------------------------------------------------------------

proj4string(SA_vegmap_CCAB) == proj4string(chirps_2016)                   # TRUE
proj4string(SA_vegmap_CCAB) == proj4string(GCFR_CCAB)                     # TRUE

chirps_2016 %>%
  mask(GCFR_CCAB) %>%
  crop(GCFR_CCAB) ->
  chirps_2016_GCFR_CCAB

proj4string(SA_vegmap_CCAB) == proj4string(chirps_2016_GCFR_CCAB)         # TRUE

# Therefore, this CRS is what I will use:

proj4string(SA_vegmap_CCAB)


# </> -----------------------------------------------------------------------------------