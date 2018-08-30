# [...]
# # Version for running on HWP Hons Lab PC (the leftmost one)

# Hons thesis
# Ruan van Mazijk

# created:      2017-07-25
# last edited:  2017-07-25


# Preamble ---------------------------------------------------------------------

# ...


# Import resulting GBIF queries from disc --------------------------------------

GBIF_GCFR <- read_tsv(here::here("Data", "GBIF_GCFRquery_2017-07-24.csv"))
GBIF_GCFR_tidy <- tidy_gbif(GBIF_GCFR)
GBIF_SWAFR <- read_tsv(here::here("Data", "GBIF_SWAFRquery_2017-07-24.csv"))
GBIF_SWAFR_tidy <- tidy_gbif(GBIF_SWAFR)
length(unique(GBIF_GCFR_tidy$species))
length(unique(GBIF_SWAFR_tidy$species))
# (Cape is more rich yeaaaaaahhhh)
# (But will that be that case after the cleaning? Oh my...)


# Cleaning and synonymy checking -----------------------------------------------

# TODO: choose taxonomic reference! (e.g. EOL vs TNRS vs NCBI etc.)
# TODO: check official column name descr from GBIF

# Tests
start <- Sys.time()
SWAFR_species_names_tnrs <- tnrs(
    query = as.character(unique(GBIF_SWAFR$species[1])),
    source = "iPlant_TNRS"
)
end <- Sys.time()
est_GCFR <- (end - start) * length(as.character(unique(GBIF_GCFR$species)))
est_SWAFR <- (end - start) * length(as.character(unique(GBIF_SWAFR$species)))
as.numeric(est_GCFR) / 60 / 60  # t.f. estimated ±8.5hr runtime for GCFR spp.
as.numeric(est_SWAFR) / 60 / 60  # and ±5.5hr for SWAFR spp.
# /Tests

GBIF_GCFR_mike <- read.csv(here::here(
    "Data", "Mikes_help_w_speciesdata",
    "Genera_data_essentials_final.csv"
))
