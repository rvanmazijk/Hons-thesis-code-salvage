# Saving regional CHIRPS stacks to disc

# Hons thesis
# Ruan van Mazijk

# created:      2017-04-15
# last edited:  2017-04-18


# Setup ---------------------------------------------------------------------------------

rm(list = ls())
require(pacman)
p_load(magrittr, tidyverse, raster, rasterVis, sp, ggplot2, rgdal, beepr)

datwd <- "/users/ruanvanmazijk/Documents/R/Hons_thesis/Data/"
giswd <- "/Volumes/RUAN'S HDD/GIS/"
reswd <- "/users/ruanvanmazijk/Documents/R/Hons_thesis/Results/"


# CRS -----------------------------------------------------------------------------------

std_CRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# GCFR polygon --------------------------------------------------------------------------

# Have to put vegmap data on MacBook (= `datwd`),
# bc the readOGR fn doesn't seem to like my HDD

SA_vegmap_CCAB <- readOGR(
  dsn = paste0(datwd, "CCAB_current_biome/"),
  layer = "Current_biome"
)
SA_vegmap_CCAB %<>% subset(!is.na(LA_CURRENT)) # removes this one pesky NA
GCFR <-
  SA_vegmap_CCAB %>%
  subset(LA_CURRENT %in% c("Fynbos", "Succulent Karoo"))

# (I would used `dplyr::filer()` or `dplyr::select()`,
# but they cannot be applied to spatial objects)

# Importing monthly CHIRPS (GCFR) -------------------------------------------------------

# Reading the .tif for each month into a stack with a for-loop

year <- as.character(1981:2017)
month <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
#pb <- txtProgressBar(min = 0, max = 4, style = 3)

# Loop for 1981 to 2016
start.time <- Sys.time() # --------------------------------------------------- Start loop
stacked_chirps <- stack()
for (i in 1:36) {
  for (j in 1:12) {
    x <-
      paste0(
        giswd, "CHIRPS_v2.0/global_monthly/chirps-v2.0.",
        year[i], ".", month[j], ".tif"
      ) %>%
      raster() %>%
      mask(GCFR) %>%
      crop(GCFR)
    stacked_chirps %<>% stack(x)
    paste0("chirps-v2.0.", year[i], ".", month[j], " is finished.") %>%
      print()
    #setTxtProgressBar(pb, j + (1 - i))
  }
}
beep(2)
#close(pb)
end.time <- Sys.time() # ------------------------------------------------------- End loop

(time.taken <- end.time - start.time) # For 1 loop: `time.taken = 7.160724 s`
((36 * 12 * 7.160724) / 60 )     # Therefore, 36*12 loops will take ± 52 min

stacked_chirps
object.size(stacked_chirps)

# Add the two months of 2017 manually
# (accidentally didnt include them in the 1st loop lol)

for (j in 1:2) {
  x <-
    paste0(
      giswd, "CHIRPS_v2.0/global_monthly/chirps-v2.0.",
      "2017.", month[j], ".tif"
    ) %>%
    raster() %>%
    mask(GCFR) %>%
    crop(GCFR)
  stacked_chirps %<>% stack(x)
}

stacked_chirps
#plot(stacked_chirps)

# Write stack to file (in local `reswd`)

writeRaster(
  stacked_chirps,
  filename = paste0(reswd, "CHIRPS_monthly_GCFR.grd"),
  bandorder = "BIL",
  overwrite = T
)

# Read stack back from that file
# And loop to add each band (= layer) to the stack

stacked_chirps_from_file <- stack()
for (i in 1:434) {
  x <-
    paste0(reswd, "CHIRPS_monthly_GCFR.grd") %>%
    raster(band = i)
  stacked_chirps_from_file %<>% stack(x)
}

stacked_chirps_from_file

par(mfrow = c(1, 2))
plot(stacked_chirps[[1]])
plot(stacked_chirps_from_file[[1]])
par(mfrow = c(1, 1))

# They seem to be the same! Yay!
# The .grd & .gri work! Yay!

chirps_monthly_GCFR <- stacked_chirps_from_file

#stacky[[paste0("chirps.v2.0.1981.", month[1])]]


# Importing annual CHIRPS (GCFR) --------------------------------------------------------

# Reading the .tif for each year into a stack with a for-loop

year <- as.character(1981:2016) # note, no 2017 annual---yet! duh!
stacked_chirps <- stack()
for (i in 1:36) {
  x <-
    paste0(
      giswd, "CHIRPS_v2.0/global_annual/chirps-v2.0.",
      year[i], ".tif"
    ) %>% 
    raster() %>%
    mask(GCFR) %>%
    crop(GCFR)
  stacked_chirps %<>% stack(x)
  paste0("chirps-v2.0.", year[i], " is finished.") %>%
    print()
}
beep(2)

writeRaster(
  stacked_chirps,
  filename = paste0(reswd, "CHIRPS_annual_GCFR.grd"),
  bandorder = "BIL",
  overwrite = T
)

stacked_chirps_from_file <- stack()
for (i in 1:36) {
  x <-
    paste0(reswd, "CHIRPS_annual_GCFR.grd") %>%
    raster(band = i)
  stacked_chirps_from_file %<>% stack(x)
}

stacked_chirps_from_file
chirps_annual_GCFR <- stacked_chirps_from_file

# And with that, two stack-objects `chirps_monthly_GCFR`
# and `chirps_annual_GCFR` are done!


# SWAFR polygon -------------------------------------------------------------------------


# Importing monthly CHIRPS (SWAFR) ------------------------------------------------------


# Importing annual CHIRPS (SWAFR) -------------------------------------------------------


# </> -----------------------------------------------------------------------------------