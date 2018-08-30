# Applying do_prelim_anal() with the HSAR pipeline to the richness rasters

# Hons thesis
# Ruan van Mazijk

# created:      2017-07-27
# last edited:  2017-07-27


# Preamble ---------------------------------------------------------------------

# ...


# Set up -----------------------------------------------------------------------

rm(list = ls())
# The order in which these source() cmds are run is NB
source("Scripts/i_my_funs_4spaces.R")
source("Scripts/ii_my_objs.R")
source("Scripts/v_prelim_anal_fn.R")
source("Scripts/vi_rand_raster_null_fn.R")
source("Scripts/vii_compare_null_obs_fn.R")
source("Scripts/viii_prelim_rand_compare_fn.R")


# Note on resolution -----------------------------------------------------------

# As the richness rasters are in QDS res (i.e. cells of 0.25 X 0.25 deg,
# or fact = 5 in terms of my environmental rasters),
# I must only agg() as follows:

agg <- function(base_res, times) {
    agg_res <- c()
    agg_res[1] <- base_res
    for (i in 2:times) {
        agg_res[i] <- agg_res[i - 1] + base_res
    }
    return(cbind(
        1:times,
        agg_res
    ))
}
agg(base_res = 0.05, times = 15)
agg(base_res = 0.25, times = 4)


# `do_prelim_anal()`-style analysis --------------------------------------------

# FIXME: prelim_rand_compare bombs out @ model fitting stage for random null
# Why? because the randomisation spread out beyond the border...

# GCFR
prelim_rand_compare(
  x                   = GCFR_richness$raster,
  need_do_prelim_anal = TRUE,
  need_do_rand_null   = TRUE,
  border              = GCFR_border,
  border_buffer       = GCFR_border_buffered,
  facts               = 2:4,
  var_name            = "floral_richness",
  region_name         = "GCFR",
  how_many_SARs       = 1,
  how_many_rand_pts   = 1000,
  x_tester            = MAP_GCFR_0.05,
  obs_dir             = paste0(reswd,
                        "prelim_anal_floral_richness_GCFR_2017-07-27/"),
  null_dir            = paste0(reswd,
                        "rand_null_floral_richness_GCFR_2017-07-27/"),
  compare_dir         = paste0(reswd,
                        "compare_null_obs_floral_richness_GCFR_2017-07-27/")
)

# SWAFR
prelim_rand_compare(
  x                   = SWAFR_richness$raster,
  need_do_prelim_anal = TRUE,
  need_do_rand_null   = TRUE,
  border              = SWAFR_border,
  border_buffer       = SWAFR_border_buffered,
  facts               = 2:4,
  var_name            = "floral_richness",
  region_name         = "SWAFR",
  how_many_SARs       = 1,
  how_many_rand_pts   = 1000,
  x_tester            = MAP_SWAFR_0.05,
  obs_dir             = paste0(reswd,
                        "prelim_anal_floral_richness_SWAFR_2017-07-27/"),
  null_dir            = paste0(reswd,
                        "rand_null_floral_richness_SWAFR_2017-07-27/"),
  compare_dir         = paste0(reswd,
                        "compare_null_obs_floral_richness_SWAFR_2017-07-27/")
)

# `PCA(A); PCA(B); PCA(A+B);`-style analysis -----------------------------------

# GCFR
do_PCAandkmeans(
    region_a_dir = paste0(
        reswd, "prelim_anal_floral_richness_GCFR_2017-07-27/",
        "dfs_floral_richness_GCFR/",
        "extracted_abs_vs_rough_floral_richness_GCFR_buffered_df.csv"
    ),
    region_a_name = "GCFR",  border_a = GCFR_border,
    var_name = "floral_richness"
)

# SWAFR
do_PCAandkmeans(
    region_a_dir = paste0(
        reswd, "prelim_anal_floral_richness_SWAFR_2017-07-27/",
        "dfs_floral_richness_SWAFR/",
        "extracted_abs_vs_rough_floral_richness_SWAFR_buffered_df.csv"
    ),
    region_a_name = "SWAFR",  border_a = SWAFR_border,
    var_name = "floral_richness"
)

# Combined PCA for both regions
do_PCAandkmeans(
    region_a_dir = paste0(
        reswd, "prelim_anal_floral_richness_GCFR_2017-07-27/",
        "dfs_floral_richness_GCFR/",
        "extracted_abs_vs_rough_floral_richness_GCFR_buffered_df.csv"
    ),
    region_b_dir = paste0(
        reswd, "prelim_anal_floral_richness_SWAFR_2017-07-27/",
        "dfs_floral_richness_SWAFR/",
        "extracted_abs_vs_rough_floral_richness_SWAFR_buffered_df.csv"
    ),
    region_a_name = "GCFR",  border_a = GCFR_border,
    region_b_name = "SWAFR", border_b = SWAFR_border,
    var_name = "floral_richness",
    do_kmeans = TRUE
)
