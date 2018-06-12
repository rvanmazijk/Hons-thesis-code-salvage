# [...]

# Hons thesis
# Ruan van Mazijk

# created:      2017-07-25
# last edited:  2017-07-27


# Preamble ---------------------------------------------------------------------

# ...


# Set up -----------------------------------------------------------------------

rm(list = ls())
# The order in which these source() cmds are run is NB
source("Scripts/i_my_funs_4spaces.R")
source("Scripts/ii_my_objs.R")

if (FALSE) {
    if (!require(rlang)) install.packages("rlang", dependencies = TRUE)
    library(rlang)
    if (!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE)
    library(tidyverse)
    if (!require(magrittr)) install.packages("magrittr", dependencies = TRUE)
    library(magrittr)
    if (!require(readr)) install.packages("readr", dependencies = TRUE)
    library(readr)
    if (!require(readxl)) install.packages("readxl", dependencies = TRUE)
    library(readxl)
    if (!require(here)) install.packages("here", dependencies = TRUE)
    library(here)
    if (!require(glue)) install.packages("glue", dependencies = TRUE)
    library(glue)
    if (!require(taxize)) install.packages("taxize", dependencies = TRUE)
    library(taxize)
    library(dplyr)
    library(purrr)
    library(stringr)
}


# Query GBIF -------------------------------------------------------------------

if (FALSE) {
    
    # Create polygons to query occs in GBIF ------------------------------------
    
    SWAFR_border_buffered %>%
        make_wkt() %>%
        write(file = here::here("Data", "SWAFR_POLYGON_WKT.txt"))
    GCFR_border_buffered %>%
        make_wkt() %>%
        write(file = here::here("Data", "GCFR_POLYGON_WKT.txt"))
    
    
    
    # e.g. <URL>
    #   http://www.gbif.org/occurrence/search?
    #       TAXON_KEY=7707728&
    #       TAXON_KEY=6&
    #       HAS_COORDINATE=true&
    #       GEOMETRY=
    #           118.90000445+-34.50000126%2C
    #           118.90000445+-35.50000127%2C
    #           ...
    #           118.90000445+-34.50000126
    # </URL>
    # as derived from `make_wkt()`'s .txt outputs above

}


# Import resulting GBIF queries from disc --------------------------------------

GBIF_GCFR <- read_tsv(here::here("Data", "GBIF_GCFRquery_2017-07-24.csv"))
GBIF_GCFR_tidy <- tidy_gbif(GBIF_GCFR)
GBIF_SWAFR <- read_tsv(here::here("Data", "GBIF_SWAFRquery_2017-07-24.csv"))
GBIF_SWAFR_tidy <- tidy_gbif(GBIF_SWAFR)


# Cleaning and synonymy checking -----------------------------------------------

GCFR_spp_sets <- summarise_spp_sets(GBIF_GCFR_tidy)
summary(GCFR_spp_sets)
SWAFR_spp_sets <- summarise_spp_sets(GBIF_SWAFR_tidy)
summary(SWAFR_spp_sets)


# 1) Checking for name-acceptedness --------------------------------------------

# 1.1) Tests -------------------------------------------------------------------

# TODO: choose taxonomic reference! (e.g. EOL vs TNRS vs NCBI etc.)
# TODO: check official column name descr from GBIF

# Note, `tnrs()` doesn't check for homonyms/synonyms if there isn't an
# authority in the query!
# There are two solutions to this (for me):
#     1) `tnrs(GBIF_SWAFR_tidy$scientificname)`
#     2) `gnr_resolve()`

# Cf these:
tnrs_test <- test_tnrs(how_many = 5, flora = GBIF_SWAFR)
# (Note, do on _tidy when doing actual one!)
gnr_test <- test_gnr(how_many = 5, flora = GBIF_SWAFR, data_source_ids = 12)

# TNRS has 1x output row per submitted name
tnrs_test$df
# GNR has ≥1x output row per submitted name
gnr_test$df

# So, in the case where there is only 1 output row for a given name in
# BOTH TNRS and GNR outputs, the test of name-consistency is easy:
is_consistent(tnrs_test$df, gnr_test$df, index = 1)
is_consistent(tnrs_test$df, gnr_test$df, 2)
is_consistent(tnrs_test$df, gnr_test$df, 4)

# But when the GNR output has ≥1 row per submitted name (as is actually
# very helpful), it gets a bit more tricky, e.g.
is_consistent(tnrs_test$df, gnr_test$df, 3)
is_consistent(tnrs_test$df, gnr_test$df, 5)

# 1.2) Do for real on real GBIF data -------------------------------------------

summary(GCFR_spp_sets)
summary(SWAFR_spp_sets)

# Now let's do this in stages/chunks just for safety/sanity:

# TODO: redo with MORE taxo databases (Mike says that even _CRAP drops too 
# many) + maybe a webscrape? + an alien filter (check GBIF?) + local Aus alien
# database?


if (FALSE) {
    
    
    # 1.2.1) For 1st 100 unique names ------------------------------------------
    
    # 1.2.1.1) GCFR ------------------------------------------------------------
    
    GCFR_names_1_100 <- flag_inconsistent_names(GBIF_GCFR_tidy, 1:100)
    saveRDS(
        GCFR_names_1_100,
        here::here("Results", "GCFR_names_1_100_2017-07-26.rds")
    )
    GCFR_names_1_100 <-
        readRDS(here::here("Results", "GCFR_names_1_100_2017-07-26.rds"))
    
    GCFR_names_1_100
    GCFR_names_1_100$consistency_check_conds %>%
        filter(consistent) %>%
        nrow()  # 90% are fine! Not bad!
    
    # Let's look at Acanthopsis, for e.g.:
    GCFR_names_1_100$tnrs_actual %>%
        filter(str_detect(submittedname, "Acanthopsis"))
    GCFR_names_1_100$gnr_actual %>%
        filter(str_detect(user_supplied_name, "Acanthopsis"))
    GCFR_names_1_100$consistency_check_gnr_df %>%
        filter(str_detect(user_supplied_name, "Acanthopsis"))
    GCFR_names_1_100$consistency_check_conds %>%
        filter(str_detect(submitted_name, "Acanthopsis"))
    GCFR_names_1_100$consistency_check_conds %>%
        filter(str_detect(submitted_name, "Acanthopsis")) %>%
        print_gnr_matches()
    # And Acacia:
    GCFR_names_1_100$consistency_check_conds %>%
        filter(str_detect(submitted_name, "Acacia")) %>%
        print_gnr_matches()
    
    # NB:
    GCFR_names_1_100$flagged
    
    
    # 1.2.1.2) SWAFR -----------------------------------------------------------
    
    SWAFR_names_1_100 <- flag_inconsistent_names(GBIF_SWAFR_tidy, 1:100)
    saveRDS(
        SWAFR_names_1_100,
        here::here("Results", "SWAFR_names_1_100_2017-07-26.rds")
    )
    SWAFR_names_1_100 <-
        readRDS(here::here("Results", "SWAFR_names_1_100_2017-07-26.rds"))
    SWAFR_names_1_100
    SWAFR_names_1_100$consistency_check_conds %>%
        filter(consistent) %>%
        nrow() # Not bad!
    # Let's peek at Acacia:
    SWAFR_names_1_100$consistency_check_conds %>%
        filter(str_detect(submitted_name, "Acacia"))
    SWAFR_names_1_100$consistency_check_conds %>%
        filter(str_detect(submitted_name, "Acacia")) %>%
        print_gnr_matches()
    
    # NB:
    SWAFR_names_1_100$flagged
    
    
    # 1.2.2) For rest of the unique names --------------------------------------
    
    # For 101st to 500th unique names
    GCFR_names_101_500 <- flag_inconsistent_names(GBIF_GCFR_tidy, 101:500)
    saveRDS(
        GCFR_names_101_500,
        here::here("Results", "GCFR_names_101_500_2017-07-26.rds")
    )
    GCFR_names_101_500 <-
        readRDS(here::here("Results", "GCFR_names_101_500_2017-07-26.rds"))
    SWAFR_names_101_500 <- flag_inconsistent_names(GBIF_SWAFR_tidy, 101:500)
    saveRDS(
        SWAFR_names_101_500,
        here::here("Results", "SWAFR_names_101_500_2017-07-26.rds")
    )
    SWAFR_names_101_500 <-
        readRDS(here::here("Results", "SWAFR_names_101_500_2017-07-26.rds"))
    
    # For 501st to 1500th unique names
    GCFR_names_501_1500 <- flag_inconsistent_names(GBIF_GCFR_tidy, 501:1500)
    saveRDS(
        GCFR_names_501_1500,
        here::here("Results", "GCFR_names_501_1500_2017-07-26.rds")
    )
    GCFR_names_501_1500 <-
        readRDS(here::here("Results", "GCFR_names_501_1500_2017-07-26.rds"))
    SWAFR_names_501_1500 <- flag_inconsistent_names(GBIF_SWAFR_tidy, 501:1500)
    saveRDS(
        SWAFR_names_501_1500,
        here::here("Results", "SWAFR_names_501_1500_2017-07-26.rds")
    )
    SWAFR_names_501_1500 <-
        readRDS(here::here("Results", "SWAFR_names_501_1500_2017-07-26.rds"))
    
    # For 1501st to 3000th unique names
    GCFR_names_1501_3000 <- flag_inconsistent_names(GBIF_GCFR_tidy, 1501:3000)
    saveRDS(
        GCFR_names_1501_3000,
        here::here("Results", "GCFR_names_1501_3000_2017-07-26.rds")
    )
    GCFR_names_1501_3000 <-
        readRDS(here::here("Results", "GCFR_names_1501_3000_2017-07-26.rds"))
    SWAFR_names_1501_3000 <- flag_inconsistent_names(GBIF_SWAFR_tidy, 1501:3000)
    saveRDS(
        SWAFR_names_1501_3000,
        here::here("Results", "SWAFR_names_1501_3000_2017-07-26.rds")
    )
    SWAFR_names_1501_3000 <-
        readRDS(here::here("Results", "SWAFR_names_1501_3000_2017-07-26.rds"))
    
    # For 3001st to 5000th unique names
    GCFR_names_3001_5000 <- flag_inconsistent_names(GBIF_GCFR_tidy, 3001:5000)
    saveRDS(
        GCFR_names_3001_5000,
        here::here("Results", "GCFR_names_3001_5000_2017-07-26.rds")
    )
    GCFR_names_3001_5000 <-
        readRDS(here::here("Results", "GCFR_names_3001_5000_2017-07-26.rds"))
    SWAFR_names_3001_5000 <- flag_inconsistent_names(GBIF_SWAFR_tidy, 3001:5000)
    saveRDS(
        SWAFR_names_3001_5000,
        here::here("Results", "SWAFR_names_3001_5000_2017-07-26.rds")
    )
    SWAFR_names_3001_5000 <-
        readRDS(here::here("Results", "SWAFR_names_3001_5000_2017-07-26.rds"))
    
    # For GCFR 5001st to 8000th unique names
    GCFR_names_5001_8000 <- flag_inconsistent_names(GBIF_GCFR_tidy, 5001:8000)
    saveRDS(
        GCFR_names_5001_8000,
        here::here("Results", "GCFR_names_5001_8000_2017-07-26.rds")
    )
    GCFR_names_5001_8000 <-
        readRDS(here::here("Results", "GCFR_names_5001_8000_2017-07-26.rds"))
    
    # For SWAFR 5001st to end
    SWAFR_names_5001_end <- flag_inconsistent_names(
        GBIF_SWAFR_tidy,
        5001:length(unique(GBIF_SWAFR_tidy$species))
    )
    saveRDS(
        SWAFR_names_5001_end,
        here::here("Results", "SWAFR_names_5001_end_2017-07-26.rds")
    )
    SWAFR_names_5001_end <-
        readRDS(here::here("Results", "SWAFR_names_5001_end_2017-07-26.rds"))
    
    # For GCFR 8001st to end (TODO)
    GCFR_names_8001_end <- flag_inconsistent_names(
        GBIF_GCFR_tidy,
        8001:length(unique(GBIF_GCFR_tidy$species))
    )
    saveRDS(
        GCFR_names_8001_end,
        here::here("Results", "GCFR_names_8001_end_2017-07-26.rds")
    )
    GCFR_names_8001_end <-
        readRDS(here::here("Results", "GCFR_names_8001_end_2017-07-26.rds"))

    
}


# 1.2.3) Compile flagged floral spp. -------------------------------------------

# GCFR

GCFR_names_flagged <- rbind_flagged_spp(c(
    GCFR_names_1_100,
    GCFR_names_101_500,
    GCFR_names_501_1500,
    GCFR_names_1501_3000,
    GCFR_names_3001_5000,
    GCFR_names_5001_8000,
    GCFR_names_8001_end
))
saveRDS(
    GCFR_names_flagged,
    here::here("Results", "GCFR_names_flagged_2017-07-26.rds")
)
GCFR_names_flagged <-
    readRDS(here::here("Results", "GCFR_names_flagged_2017-07-26.rds"))

GCFR_names_CRAP <- GCFR_names_flagged %>%
    filter(not(tnrs_cond) & not(gnr_cond))
GCFR_names_badTNRS <- GCFR_names_flagged %>%
    filter(not(tnrs_cond) & gnr_cond)
GCFR_names_badGNR <- GCFR_names_flagged %>%
    filter(tnrs_cond & not(gnr_cond))

# SWAFR

SWAFR_names_flagged <- rbind_flagged_spp(c(
    SWAFR_names_1_100,
    SWAFR_names_101_500,
    SWAFR_names_501_1500,
    SWAFR_names_1501_3000,
    SWAFR_names_3001_5000,
    SWAFR_names_5001_end
))
saveRDS(
    SWAFR_names_flagged,
    here::here("Results", "SWAFR_names_flagged_2017-07-26.rds")
)
SWAFR_names_flagged <-
    readRDS(here::here("Results", "SWAFR_names_flagged_2017-07-26.rds"))

SWAFR_names_CRAP <- SWAFR_names_flagged %>%
    filter(not(tnrs_cond) & not(gnr_cond))
SWAFR_names_badTNRS <- SWAFR_names_flagged %>%
    filter(not(tnrs_cond) & gnr_cond)
SWAFR_names_badGNR <- SWAFR_names_flagged %>%
    filter(tnrs_cond & not(gnr_cond))


# 1.2.4) Compile clean floras --------------------------------------------------
# (drop only the spp that fail both the TNRS and GNR look-ups)

GCFR_clean_flora <- GBIF_GCFR_tidy %>%
    filter(not(species %in% GCFR_names_CRAP$submitted_name))
saveRDS(
    GCFR_clean_flora,
    here::here("Results", "GCFR_clean_flora_2017-07-27.rds")
)
GCFR_clean_flora <- 
    readRDS(here::here("Results", "GCFR_clean_flora_2017-07-27.rds"))
length(unique(GCFR_clean_flora$species))
summary(GCFR_spp_sets)
nrow(GBIF_GCFR_tidy)
nrow(GCFR_clean_flora)

SWAFR_clean_flora <- GBIF_SWAFR_tidy %>%
    filter(not(species %in% SWAFR_names_CRAP$submitted_name))
saveRDS(
    SWAFR_clean_flora,
    here::here("Results", "SWAFR_clean_flora_2017-07-27.rds")
)
SWAFR_clean_flora <- 
    readRDS(here::here("Results", "SWAFR_clean_flora_2017-07-27.rds"))
length(unique(SWAFR_clean_flora$species))
summary(SWAFR_spp_sets)
nrow(GBIF_SWAFR_tidy)
nrow(SWAFR_clean_flora)

# TODO: cf dropping _badTNRS and _badGNR? too severe?
# TODO: read through flags by hand


# 2) Synonmy/homonymy ----------------------------------------------------------

# TODO (check just non-flagged, or all again?)

# Pairwise comparison for homonymy/synonymy of all names in the two floras?

as.data.frame(GCFR_names_1_100$gnr_actual)


# 3) Check invasive status, etc. -----------------------------------------------
# (see mike's script for what he checked for)

# TODO


# Import Larsen QDS grids and convert to raster --------------------------------

# <Imported in ii_my_obs.R>

GCFR_QDS_raster <- GCFR_QDS %>%
    QDSpolydf2raster(region_border = GCFR_border_buffered)
SWAFR_QDS_raster <- SWAFR_QDS %>%
    QDSpolydf2raster(region_border = SWAFR_border_buffered)


# Make rasters of floral richness ----------------------------------------------

GCFR_richness <- make_richness_raster(
    flora_occs = GCFR_clean_flora,
    region_raster = GCFR_QDS_raster,
    crs = std_CRS  # TODO: double check!
)
plot(GCFR_richness$raster)
plot(terrain(GCFR_richness$raster, "slope"))  # "richness turnover"
saveRDS(GCFR_richness, here::here("Results", "GCFR_richness_2017-07-27.rds"))
writeRaster(
    GCFR_richness$raster,
    here::here("Results", "GCFR_richness_2017-07-27.tif"),
    overwrite = TRUE
)
writeRaster(
    GCFR_richness$raster,
    here::here("Results", "GCFR_richness_2017-07-27.grd"),
    overwrite = TRUE
)

SWAFR_richness <- make_richness_raster(
    flora_occs = SWAFR_clean_flora,
    region_raster = SWAFR_QDS_raster,
    crs = std_CRS  # TODO: double check!
)
plot(SWAFR_richness$raster)
plot(terrain(SWAFR_richness$raster, "slope"))
saveRDS(SWAFR_richness, here::here("Results", "SWAFR_richness_2017-07-27.rds"))
writeRaster(
    SWAFR_richness$raster,
    here::here("Results", "SWAFR_richness_2017-07-27.tif"),
    overwrite = TRUE
)
writeRaster(
    SWAFR_richness$raster,
    here::here("Results", "SWAFR_richness_2017-07-27.grd"),
    overwrite = TRUE
)

# TODO: species (composition) turnover?

# TODO: set logical for creating objects FALSE

# TODO: compare MY GCFR_clean_flora to Mike's


# Deprecated -------------------------------------------------------------------
    
check_tnrs1 <- function(spp_vect, unique_index_range) {
    spp_vect_unique <- spp_vect %>%
        unique() %>%
        as.character() %>%
        `[`(unique_index_range)
    out_tnrs <- vector("list", length = length(spp_vect_unique))
    pb <- txtProgressBar(min = 0, max = length(spp_vect_unique), style = 3)
    for (i in seq_along(spp_vect_unique)) {
        out_tnrs[[i]] <- tnrs(
            query = spp_vect_unique[i],
            source = "iPlant_TNRS"
        )
        setTxtProgressBar(pb, i)
    }
    close(pb)
    return(out_tnrs)
}
list2df <- function(x, nrow = length(x), ncol = length(x[[1]])) {
    out <- matrix(nrow = nrow, ncol = ncol)
    for (i in 1:nrow) {
        for (j in 1:ncol) {
            out[i, j] <- x[[i]][[j]]
        }
    }
    return(as.data.frame(out))
}
is_consistent1 <- function(tnrs_df, gnr_df, index) {
    # If A = B, B = C, and C = D, then one needn't test that A = C or A = D,
    # as it is logically consequential! As such:
    consistent <- all(c(
        # TNRS submitted == matched
        tnrs_df %$% {submittedname[index] == matchedname[index]},
        # TNRS matched == GNR submitted
        tnrs_df$matchedname[index] == gnr_df$user_supplied_name[index],
        # GNR submitted == matched
        gnr_df %$% {
            user_supplied_name[index] == binom_only(matched_name[index])
        }
    ))
    return(consistent)
}

