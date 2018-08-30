# [...]

# Hons thesis
# Ruan van Mazijk

# created:      2017-07-03
# last edited:  2017-07-25


# Preamble ---------------------------------------------------------------------

# ...


# Setup ------------------------------------------------------------------------

rm(list = ls())
# The order in which these source() cmds are run is NB
source("Scripts/i_my_funs_4spaces.R")
source("Scripts/ii_my_objs.R")
if (!require(taxize)) devtools::install_github("ropensci/taxize")


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
        filter(taxonrank %in% c("SPECIES", "SUBSPECIES", "FORM", "VARIETY")) %>%
        # (so that all infrasp. ranks are merged into their spp.)
        dplyr::select(
            family, genus, species, scientificname, taxonrank,
            # Note: use `species` mostly, as does not contain any infrasp. info!
            # TODO: think about `species` vs `scientificname`
            decimallatitude, decimallongitude,
            coordinateuncertaintyinmeters, coordinateprecision
        ) %>%
        arrange(species)
}


# Create polygons to query occs in GBIF ----------------------------------------

SWAFR_border_buffered %>%
    make_wkt() %>%
    write(file = here::here("Data", "SWAFR_POLYGON_WKT.txt"))
GCFR_border_buffered %>%
    make_wkt() %>%
    write(file = here::here("Data", "GCFR_POLYGON_WKT.txt"))


# <Query GBIF> -----------------------------------------------------------------

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










# Check name-correctness with EoL:
gnr_resolve(GBIF_GCFR$species[1])  # TODO: loop through all
# Check synonymy
tsn <- get_tsn(GBIF_GCFR$species[1:10], accepted = FALSE)
GBIF_GCFR$scientificname[1:10]










S_SWAFR <- GBIF_SWAFR %>%
    filter(taxonrank == "SPECIES") %>%  # TODO: include subsp. and var.?
    dplyr::select(scientificname) %>%
    unique() %>%
    arrange(scientificname)
    
    
S_GCFR_bo <- binom_only(S_GCFR$scientificname)
S_GCFR[30, ]
S_GCFR_bo[30]
GBIF_GCFR$taxonrank[GBIF_GCFR$genus == "Hypnea"]
GBIF_GCFR$taxonrank
arrange(scientificname)
length(S_GCFR$scientificname)
length(S_SWAFR$scientificname)






# Check name-correctness with EoL:
gnr_resolve(names = binom_only(S_GCFR$scientificname[1]))  # TODO: loop through all
# Check synonymy
rm_yr_auth(S_GCFR$scientificname[1])
str_extract(S_GCFR$scientificname[1], "^[a-zA-Z]+ [a-zA-Z]+( [a-z\\.]+ [a-z\\-]+)?")
binom_only(S_GCFR$scientificname[1])
tsn <- get_tsn(binom_only(S_GCFR$scientificname[1:10]), accepted = FALSE)
tsn_accepted <- vector("list", length = 10)
for (i in 1:10) {
    if (not(is.na(tsn[i]))) {
        tsn_accepted[[i]] <- itis_acceptname(tsn[i])
    } else {
        tsn_accepted[[i]] <- NA
    }
}
tax_name(query = binom_only_ss(S_GCFR$scientificname[1000]), get = )
get_tsn(binom_only(S_GCFR$scientificname[1000]))
binom_only_ss(S_GCFR$scientificname[1000])
get_tsn(binom_only_ss(S_GCFR$scientificname[1000]))




















SWAFR_POLYGON <- SWAFR_border_buffered@polygons[[1]]@Polygons[[1]]@coords
make_wkt <- function(x) {
    wkt_expr <- ""
    for (i in 1:nrow(x)) {
        wkt_expr <- glue("{wkt_expr}{x[i, 1]}+{x[i, 2]}%2C")
    }
    wkt_expr %<>%
        as.character() %>%
        substr(., 0, nchar(.) - 3)
    return(wkt_expr)
}
write(make_wkt(SWAFR_POLYGON), file = here("Data", "SWAFR_POLYGON_WKT.txt"))


#?geometry=POLYGON((30.1 10.1, 10 20, 20 40, 40 40, 30.1 10.1))


















#df <- occ(query = "South Africa", from = "gbif")
df <- occ(from = "gbif", gbifopts = list(
    taxonKey = 6,
    country = "ZA",
    hasCoordinate = TRUE,
    limit = 200000
))
is(df)

"GBIF_GCFRquery_2017-07-03"
"doi:10.15468/dl.qvcpbz"
"http://www.gbif.org/occurrence/download/0002636-170627171947987"


unique(GBIF_GCFR$scientificname)
length(S_GCFR)
novarsubsp_S_GCFR <- S_GCFR[not(
  str_detect(S_GCFR, fixed("var.")) |
  str_detect(S_GCFR, fixed("subsp."))
)]

novarsubsp_GBIF_GCFR <- GBIF_GCFR[
  GBIF_GCFR$scientificname %in% novarsubsp_S_GCFR,
]
quartz()
plot(data = novarsubsp_GBIF_GCFR, decimallatitude ~ decimallongitude)

pts_novarsubsp_GBIF_GCFR <- SpatialPoints(coords = novarsubsp_GBIF_GCFR[,
  c("decimallongitude", "decimallatitude")
])
MAP_GCFR_buffered <- raster(paste0(
  "/Users/ruanvanmazijk/Documents/R/Hons_thesis/",
  "Results/",
  "MAP_GCFR_buffered.grd"
))
raster_pts_novarsubsp_GBIF_GCFR <- rasterize(pts_novarsubsp_GBIF_GCFR, MAP_GCFR_buffered)
raster_pts_novarsubsp_GBIF_GCFR[!is.na(raster_pts_novarsubsp_GBIF_GCFR[])] <- 1

pts_novarsubsp_GBIF_GCFR
plot(raster_pts_novarsubsp_GBIF_GCFR)

pts_novarsubsp_GBIF_GCFR %<>% crop(extent(MAP_GCFR_buffered))
r <- MAP_GCFR_buffered
tab <- table(cellFromXY(MAP_GCFR_buffered, pts_novarsubsp_GBIF_GCFR))
r[as.numeric(names(tab))] <- tab
plot(r)
points(pts_novarsubsp_GBIF_GCFR)

r <- raster(xmn=0, ymn=0, xmx=10, ymx=10, res=1)
r[] <- 0
xy <- spsample(as(extent(r), 'SpatialPolygons'), 100, 'random')
tab <- table(cellFromXY(r, xy))
r[as.numeric(names(tab))] <- tab

r





dataset_suggest(query = "TAXON_KEY=6&COUNTRY=ZA&COUNTRY=AU")

# `taxonKey = 6` is for Plantae (aot tracheophyta),
foo <- occ_search(taxonKey = 6, country = "ZA", hasCoordinate = TRUE)
foo
plot(foo$data$decimalLatitude ~ foo$data$decimalLongitude)

df <- occ(query = "TAXON_KEY=6&COUNTRY=ZA", from = "gbif")
df


# Depecated --------------------------------------------------------------------

is_unique1 <- function(x) {
    unique_x <- unique(x)
    uniqueness_bool <- vector(length = length(x))
    pb <- txtProgressBar(min = 0, max = length(x))
    for (i in seq_along(x)) {
        if (x[i] %in% unique_x) {
            uniqueness_bool[i] <- TRUE
        } else {
            uniqueness_bool[i] <- FALSE
        }
        setTxtProgressBar(pb, i)
    }
    close(pb)
    return(uniqueness_bool)
}
is_unique2 <- function(x) {
    unique_x <- unique(x)
    uniqueness_bool <- vector(length = length(x))
    pb <- txtProgressBar(min = 0, max = length(x))
    for (i in seq_along(x)) {
        uniqueness_bool[i] <- ifelse(x[i] %in% unique_x, TRUE, FALSE)
        setTxtProgressBar(pb, i)
    }
    close(pb)
    return(uniqueness_bool)
}
is_unique3 <- function(x) {
    unique_x <- unique(x)
    uniqueness_bool <- c(TRUE)
    pb <- txtProgressBar(min = 0, max = length(unique_x))
    for (i in 2:length(x)) {
        if ((x[i] %in% unique_x) && not(x[i] %in% x[uniqueness_bool])) {
            uniqueness_bool[i] <- TRUE
        } else if (not(x[i] %in% unique_x) || (x[i] %in% uniqueness_bool)) {
            uniqueness_bool[i] <- FALSE
        } else {
            uniqueness_bool[i] <- FALSE
        }
        setTxtProgressBar(pb, i)
    }
    close(pb)
    return(uniqueness_bool)
}
rm_yr <- function(x) {
    out <- vector(length = length(x))
    for (i in seq_along(x)) {
        out[i] <- substr(x[i], 0, nchar(x[i]) - 6)
    }
    return(out)
}
rm_yr_auth <- function(x) {
    out <- vector(length = length(x))
    for (i in seq_along(x)) {
        out[i] <- str_split(rm_yr(x[i]), " \\(")[[1]][1]
    }
    return(out)
}
binom_only <- function(x) {
    out <- vector(length = length(x))
    for (i in seq_along(x)) {
        out[i] <- str_extract(x[i], "^[a-zA-Z]+ [a-zA-Z]+")
    }
    return(out)
}
binom_winfrasp <- function(x) {
    # Old regex stuff:
    # "/^[[:word:]]{3,} [[:word:]]{3,} ((var\\.|subsp\\.) [[:word:]]{3,})?/g"
    # " \\(.+\\).+"
    # "^.+ .+ ((var\\.|subsp\\.) [[:word:]])?"
    # "^[a-zA-Z]+ [a-z]+( [a-z\.]+ [a-z\-]+)?"
    out <- vector(length = length(x))
    for (i in seq_along(x)) {
        out[i] <- str_extract(
            x[i],
            "^[a-zA-Z]+ [a-zA-Z]+( [a-z\\.]+ [a-z\\-]+)?"
        )
    }
    return(out)
}