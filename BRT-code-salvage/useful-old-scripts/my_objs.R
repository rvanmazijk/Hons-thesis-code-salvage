# My saved objects & outputs
# For my various Hons thesis analyses
# Ruan van Mazijk


# TODO: ensure all 4 space indentation
# TODO: reindent a l aLisp for `(...)`, and K&R for `{...}` and long `(...)`s


# Region polygons --------------------------------------------------------------

SA_vegmap_CCAB <- readOGR(dsn = paste0(datwd, "CCAB_current_biome/"),
                          layer = "Current_biome")
SA_vegmap_CCAB %<>% subset(!is.na(LA_CURRENT)) # removes this one pesky NA
GCFR_border <- SA_vegmap_CCAB %>%
    subset(LA_CURRENT %in% c("Fynbos", "Succulent Karoo"))

SWAFR_border <- readOGR(dsn = paste0(datwd, "SWBP/"))


# CHIRPS -----------------------------------------------------------------------

chirps_annual_GCFR <- stack()
for (i in 1:36) {
    x <- paste0(reswd, "CHIRPS_annual_GCFR.grd") %>%
        raster(band = i)
    chirps_annual_GCFR %<>% stack(x)
}

chirps_annual_SWAFR <- stack()
for (i in 1:36) {
    x <- paste0(reswd, "CHIRPS_annual_SWAFR.grd") %>%
        raster(band = i)
    chirps_annual_SWAFR %<>% stack(x)
}

chirps_annual_GCFR_buffered <- stack()
for (i in 1:36) {
    x <- paste0(reswd, "CHIRPS_annual_GCFR_buffered.grd") %>%
        raster(band = i)
    chirps_annual_GCFR_buffered %<>% stack(x)
}

chirps_annual_SWAFR_buffered <- stack()
for (i in 1:36) {
    x <- paste0(reswd, "CHIRPS_annual_SWAFR_buffered.grd") %>%
        raster(band = i)
    chirps_annual_SWAFR_buffered %<>% stack(x)
}

#chirps_monthly_GCFR <- stack()
#for (i in 1:434) {
#  x <- paste0(reswd, "CHIRPS_monthly_GCFR.grd") %>%
#    raster(band = i)
#  chirps_monthly_GCFR %<>% stack(x)
#}

MAP_GCFR_0.05 <- raster(paste0(reswd, "MAP_GCFR.grd"))

MAP_SWAFR_0.05 <- raster(paste0(reswd, "MAP_SWAFR.grd"))

MAP_GCFR_buffered <- raster(paste0(reswd, "MAP_GCFR_buffered.grd"))

MAP_SWAFR_0.05_buffered <- raster(paste0(reswd, "MAP_SWAFR_buffered.grd"))

reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_MAP_GCFR/")

abs_vs_rough_MAP_GCFR_buffered_df <-
    paste0(reswd_temp, "abs_vs_rough_MAP_GCFR_buffered_df.csv") %>%
    read.csv() %>%
    dplyr::select(fact, MAP, rough_agg_MAP)

extracted_rough_MAP_GCFR_buffered <-
    paste0(reswd_temp, "extracted_rough_MAP_GCFR_buffered_df.csv") %>%
    read.csv() %>%
    dplyr::select(lon, lat, pt_ID, fact, rough_agg_MAP)


# SRTM -------------------------------------------------------------------------

SRTM_GCFR_0.05 <- raster(paste0(reswd, "SRTM_GCFR_0.05.grd"))

SRTM_GCFR_0.05_buffered <- raster(paste0(reswd, "SRTM_GCFR_0.05_buffered.grd"))

SRTM_SWAFR_0.05_buffered <- raster(paste0(reswd, "SRTM_SWAFR_0.05_buffered.grd"))

reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_SRTM_GCFR/")

abs_vs_rough_SRTM_GCFR_df <-
    paste0(reswd_temp, "abs_vs_rough_SRTM_GCFR_df.csv") %>%
    read.csv() %>%
    dplyr::select(fact, SRTM, rough_agg_SRTM)

abs_vs_rough_SRTM_GCFR_buffered_df <-
    paste0(reswd_temp, "abs_vs_rough_SRTM_GCFR_buffered_df.csv") %>%
    read.csv() %>%
    dplyr::select(fact, SRTM, rough_agg_SRTM)

extracted_rough_SRTM_GCFR <-
    paste0(reswd_temp, "extracted_abs_vs_rough_agg_SRTM_GCFR.csv") %>%
    read.csv() %>%
    dplyr::select(lon, lat, pt_ID, fact, rough_agg_SRTM)


# MODIS ------------------------------------------------------------------------

monthly_LST_GCFR_0.05_buffered <- stack()
for (i in 1:12) {
    x <- paste0(reswd, "MODIS_monthly_means_GCFR_0.05_buffered.grd") %>%
        raster(band = i)
    monthly_LST_GCFR_0.05_buffered %<>% stack(x)
}

MA_LST_GCFR_0.05_buffered <-
    raster(paste0(reswd, "MODIS_annual_mean_GCFR_0.05_buffered.grd"))


monthly_LST_SWAFR_0.05_buffered <- stack()
for (i in 1:12) {
    x <- paste0(reswd, "MODIS_monthly_means_SWAFR_0.05_buffered.grd") %>%
        raster(band = i)
    monthly_LST_SWAFR_0.05_buffered %<>% stack(x)
}

MA_LST_SWAFR_0.05_buffered <-
    raster(paste0(reswd, "MODIS_annual_mean_SWAFR_0.05_buffered.grd"))


# Buffered region polygons -----------------------------------------------------

# 2017-06-30 17:31 TODO ---
# Make buffered border polygons as if at factor 20 or 15?
# Currently at 20, but that means the agg() plots' pixels *seem* misalgined
# (bc in a sense they are) witht he border poly


facts <- 2:20 # the factors to which we want to `agg()`
agg_MAP_GCFR <- custom_aggregate_loop(MAP_GCFR_0.05, facts = facts)
agg_MAP_GCFR %<>% c(MAP_GCFR_0.05, .)  # include the original raster...
names(agg_MAP_GCFR)[1] <- "1"  # ... and name it, intuitively, as factor = 1
agg_MAP_GCFR_borders <- vector("list", length = 20)
for (i in 1:20) {
    temp <- agg_MAP_GCFR[[i]] > -Inf
    agg_MAP_GCFR_borders[[i]] <- rasterToPolygons(temp, dissolve = T)
}
names(agg_MAP_GCFR_borders) <- 1:20
GCFR_border_buffered <- agg_MAP_GCFR_borders$`20`

agg_MAP_SWAFR <- custom_aggregate_loop(MAP_SWAFR_0.05, facts = 2:20)
agg_MAP_SWAFR %<>% c(MAP_SWAFR_0.05, .)
names(agg_MAP_SWAFR)[1] <- 1
agg_MAP_SWAFR_borders <- vector("list", length = 20)
for (i in 1:20) {
    temp <- agg_MAP_SWAFR[[i]] > -Inf
    agg_MAP_SWAFR_borders[[i]] <- rasterToPolygons(temp, dissolve = T)
}
names(agg_MAP_SWAFR_borders) <- 1:20
SWAFR_border_buffered <- agg_MAP_SWAFR_borders$`20`


# Region QDS grids -------------------------------------------------------------

GCFR_QDS <- here::here("Data", "qdgc_zaf") %>%
    readOGR(layer = "qdgc_02_zaf") %>%
    crop(GCFR_border_buffered)

SWAFR_QDS <- here::here("Data", "qdgc_aus") %>%
    readOGR(layer = "qdgc_02_aus") %>%
    crop(SWAFR_border_buffered)


# Regional floras --------------------------------------------------------------

GBIF_GCFR <- read_tsv(here::here("Data", "GBIF_GCFRquery_2017-07-24.csv"))
GBIF_GCFR_tidy <- tidy_gbif(GBIF_GCFR)
GCFR_names_flagged <-
    readRDS(here::here("Results", "GCFR_names_flagged_2017-07-26.rds"))
GCFR_spp_sets <- summarise_spp_sets(GBIF_GCFR_tidy)
GCFR_names_CRAP <- GCFR_names_flagged %>%
    filter(not(tnrs_cond) & not(gnr_cond))
GCFR_names_badTNRS <- GCFR_names_flagged %>%
    filter(not(tnrs_cond) & gnr_cond)
GCFR_names_badGNR <- GCFR_names_flagged %>%
    filter(tnrs_cond & not(gnr_cond))
GCFR_clean_flora <-
    readRDS(here::here("Results", "GCFR_clean_flora_2017-07-27.rds"))

GBIF_SWAFR <- read_tsv(here::here("Data", "GBIF_SWAFRquery_2017-07-24.csv"))
GBIF_SWAFR_tidy <- tidy_gbif(GBIF_SWAFR)
SWAFR_spp_sets <- summarise_spp_sets(GBIF_SWAFR_tidy)
SWAFR_names_flagged <-
    readRDS(here::here("Results", "SWAFR_names_flagged_2017-07-26.rds"))
SWAFR_names_CRAP <- SWAFR_names_flagged %>%
    filter(not(tnrs_cond) & not(gnr_cond))
SWAFR_names_badTNRS <- SWAFR_names_flagged %>%
    filter(not(tnrs_cond) & gnr_cond)
SWAFR_names_badGNR <- SWAFR_names_flagged %>%
    filter(tnrs_cond & not(gnr_cond))
SWAFR_clean_flora <-
    readRDS(here::here("Results", "SWAFR_clean_flora_2017-07-27.rds"))
