# [...]
# # Version for running on HWP Hons Lab PC (the leftmost one)

# Hons thesis
# Ruan van Mazijk

# created:      2017-07-25
# last edited:  2017-07-25


# Preamble ---------------------------------------------------------------------

# ...


# Set up -----------------------------------------------------------------------

rm(list = ls())
# The order in which these source() cmds are run is NB
source("Scripts/i_my_funs_4spaces.R")
source("Scripts/ii_my_objs.R")
if (!require(taxize)) devtools::install_github("ropensci/taxize")
p_load(taxize)

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


# New functions ----------------------------------------------------------------

make_wkt <- function(x = "SpatialPolygonsDataframe") {
    # Converts a matrix of XY-coords from within a polygon
    # into a "Well Known Text" (WKT) output, for use in querying GBIF
    x <- x@polygons[[1]]@Polygons[[1]]@coords  # dig in the S4 stuff
    wkt_expr <- ""
    for (i in 1:nrow(x)) {
        # glue() together all the x's and y's recursively
        # ("+" = " ", "%2C" = ",")
        wkt_expr <- glue("{wkt_expr}{x[i, 1]}+{x[i, 2]}%2C")
    }
    wkt_expr %<>%
        as.character() %>%
        substr(., 0, nchar(.) - 3) # rm the trailing "%2C"
    return(wkt_expr)
}

tidy_gbif <- function(x) {
    x %>%
        dplyr::filter(
            phylum == "Tracheophyta",  # Just in case
            taxonrank %in% c("SPECIES", "SUBSPECIES", "FORM", "VARIETY")
            # (so that all infrasp. ranks can be merged into their spp.)
        ) %>%
        dplyr::select(
            family, genus, species, infraspecificepithet,
            scientificname, taxonrank,
            # Note: use `species` mostly, as does not contain any infrasp. info!
            # TODO: think about `species` vs `scientificname`
            decimallatitude, decimallongitude,
            coordinateuncertaintyinmeters, coordinateprecision
        ) %>%
        dplyr::arrange(species)
}

test_tnrs <- function() {
    start <- Sys.time()
    tnrs(
        query = as.character(unique(GBIF_SWAFR$species[1])),
        source = "iPlant_TNRS"
    )
    end <- Sys.time()
    est_GCFR <-
        (end - start) * length(as.character(unique(GBIF_GCFR$species)))
    est_SWAFR <-
        (end - start) * length(as.character(unique(GBIF_SWAFR$species)))
    return(list(
        est_GCFR_runtime_hr = as.numeric(est_GCFR) / 60 / 60,
        est_SWAFR_runtime_hr = as.numeric(est_SWAFR) / 60 / 60
    ))
}

check_tnrs <- function(spp_vect, unique_index_range = NULL) {
    spp_vect_unique <- spp_vect %>%
        unique() %>%
        as.character()
    if (not(is.null(unique_index_range))) {
        spp_vect_unique <- spp_vect_unique[unique_index_range]
    }
    out_tnrs <- tnrs(
        query = spp_vect_unique,
        source = "iPlant_TNRS"
    )
    return(out_tnrs)
}


# Import resulting GBIF queries from disc --------------------------------------

GBIF_GCFR <- read_tsv(here::here("GBIF_GCFRquery_2017-07-24.csv"))
GBIF_GCFR_tidy <- tidy_gbif(GBIF_GCFR)
GBIF_SWAFR <- read_tsv(here::here("GBIF_SWAFRquery_2017-07-24.csv"))
GBIF_SWAFR_tidy <- tidy_gbif(GBIF_SWAFR)


# Cleaning and synonymy checking -----------------------------------------------

GCFR_spp_sets <- GBIF_GCFR_tidy %$% {
    list(
        species = species,
        infraspecificepithet = unique(infraspecificepithet),
        scientificname = scientificname
    ) %>%
    map(unique)
}
GCFR_spp_sets$species_supsp <- GBIF_GCFR %$% {
    paste(species, infraspecificepithet) %>%
    str_replace_all(" NA", "") %>%
    unique()
}
GCFR_spp_sets$scientificname_species <- GCFR_spp_sets %$% {
    str_extract(scientificname, "^[a-zA-Z]* [a-zA-Z]*") %>%
    unique()
}
GCFR_spp_sets$intersect_species_sciname <- GCFR_spp_sets %$% {
    intersect(species, scientificname_species) %>%
    unique()
}
summary(GCFR_spp_sets)
GCFR_spp_sets %$% intersect(species, scientificname_species)

GCFR_spp_sets %$% {
    species[species %in% scientificname_species][1:10]
}
GCFR_spp_sets %$% {
    scientificname_species[scientificname_species %in% species][1:10]
}

NA %in% GCFR_spp_sets$scientificname_species
"NA" %in% GCFR_spp_sets$scientificname

GCFR_spp_sets$species_supsp
GCFR_spp_sets$scientificname_species
    GCFR_spp_sets %$% {
        scientificname_species[scientificname_species %in% species_supsp]
      
GCFR_spp_sets %$% species_supsp[not(species_supsp %in% scientificname)]
GCFR_spp_sets %$% scientificname[not(scientificname %in% species_supsp)]

str_extract(GCFR_spp_sets$scientificname, "^[a-zA-Z]+ [a-zA-Z]+")

length(unique(GBIF_GCFR_tidy$scientificname))
length(unique(na.omit(GBIF_GCFR_tidy$infraspecificepithet))) + 
    length(unique(GBIF_GCFR_tidy$species))
length(unique(GBIF_SWAFR_tidy$species))



# TODO: choose taxonomic reference! (e.g. EOL vs TNRS vs NCBI etc.)
# TODO: check official column name descr from GBIF

# Note, `tnrs()` doesn't check for homonyms/synonyms if there isn't an
# authority in the query!
# There are two solutions to this (for me):
#     1) `tnrs(GBIF_SWAFR_tidy$scientificname)`
#     2) `gnr_resolve()`

# Cf:
unique(GBIF_GCFR_tidy$scientificname)[1:2]
unique(GBIF_GCFR_tidy$species)[1:2]
gnr_resolve(GBIF_GCFR_tidy$species[1:2])
gnr_resolve(GBIF_GCFR_tidy$scientificname[1:2])
tnrs(GBIF_GCFR_tidy$species[1:2])
tnrs(GBIF_GCFR_tidy$scientificname[1:2])






test_tnrs()  # Very bad guideline for how long this will take... FIXME


SWAFR_tnrs <- check_tnrs(GBIF_SWAFR_tidy$species, 1:20)
write.csv(SWAFR_tnrs, here("SWAFR_TNRSchecked2.csv"))



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


