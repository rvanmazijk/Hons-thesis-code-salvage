# PCA of MAP rough vs fact labelled by GCFR or SWAFR

# Hons thesis
# Ruan van Mazijk

# created:      2017-06-29
# last edited:  2017-07-20


# Setup ------------------------------------------------------------------------

rm(list = ls())
# The order in which these source() cmds are run is NB
source("Scripts/i_my_funs.R")
#source("Scripts/i_my_funs_4spaces.R")  # FIXME
source("Scripts/ii_my_objs.R")
source("Scripts/v_prelim_anal_fn.R")
source("Scripts/vi_rand_raster_null_fn.R")
source("Scripts/vii_compare_null_obs_fn.R")
source("Scripts/viii_prelim_rand_compare_fn.R")


# New functions ----------------------------------------------------------------

plot_abs_vs_rough_custom <- function(a, b, method) {
    op <- par(op)
    par(mfrow = c(2, 2), mar = c(1, 1, 2, 1))
    plot(a)
    for (i in seq_along(method)) {
        a %>%
            terrain(opt = method[i]) %>%
            plot(main = method[i])
    }
    plot(b)
    for (i in seq_along(method)) {
        b %>%
            terrain(opt = method[i]) %>%
            plot(main = method[i])
    }
    par(op)
}


# PCA rough vs scale MAP, for GCFR & SWAFR -------------------------------------

# GCFR
do_PCAandkmeans(
    region_a_dir = paste0(
        reswd, "prelim_anal_MAP_GCFR_2017-06-28/", "dfs_MAP_GCFR/",
        "extracted_abs_vs_rough_MAP_GCFR_buffered_df.csv"
    ),
    region_a_name = "GCFR",  border_a = GCFR_border,
    var_name = "MAP"
)

# SWAFR
do_PCAandkmeans(
    region_a_dir = paste0(
        reswd, "prelim_anal_MAP_SWAFR_2017-06-28/", "dfs_MAP_SWAFR/",
        "extracted_abs_vs_rough_MAP_SWAFR_buffered_df.csv"
    ),
    region_a_name = "SWAFR", border_a = SWAFR_border,
    var_name = "MAP"
)

# Combined PCA for both regions
do_PCAandkmeans(
    region_a_dir = paste0(
        reswd, "prelim_anal_MAP_GCFR_2017-06-28/", "dfs_MAP_GCFR/",
        "extracted_abs_vs_rough_MAP_GCFR_buffered_df.csv"
    ),
    region_b_dir = paste0(
        reswd, "prelim_anal_MAP_SWAFR_2017-06-28/", "dfs_MAP_SWAFR/",
        "extracted_abs_vs_rough_MAP_SWAFR_buffered_df.csv"
    ),
    region_a_name = "GCFR",  border_a = GCFR_border,
    region_b_name = "SWAFR", border_b = SWAFR_border,
    var_name = "MAP",
    do_kmeans = FALSE
)

# Concl: not two clusters...

# TODO:
plot_abs_vs_rough_custom(
    agg_MAP_GCFR$`1`, agg_MAP_SWAFR$`1`,
    c("roughness", "TRI", "TPI")
)


# '' Elev '' -------------------------------------------------------------------

# 2017-06-30 18:17 --- After chatting to Mike, he thinks the large amount
# of high PC1 score in the SW coastal corner of SWAFR panel is suspicious...
# And this remains even with a common scale with the GCFR.
# So let's investigate the actual roughness rasters for the GCFR and SWAFR,
# aot the PC1 proxy

# 2017-06-30 18:39 --- There does indeed seem to be some roughness there! hmmm!!

# Let's look at elevation (= topography):

agg_elev_GCFR <- vector("list", length = 15)
for (i in seq_along(agg_elev_GCFR)) {
    agg_elev_GCFR[[i]] <- raster(paste0(
        reswd, "prelim_anal_elev_GCFR_2017-06-28/",
        "raster_lists_elev_GCFR_GRDs/",
        "agg_elev_GCFR_fact_", i, ".grd"
    ))
    names(agg_elev_GCFR)[i] <- i
}
agg_elev_SWAFR <- vector("list", length = 15)
for (i in seq_along(agg_elev_SWAFR)) {
    agg_elev_SWAFR[[i]] <- raster(paste0(
        reswd, "prelim_anal_elev_SWAFR_2017-06-28/",
        "raster_lists_elev_SWAFR_GRDs/",
        "agg_elev_SWAFR_fact_", i, ".grd"
    ))
    names(agg_elev_SWAFR)[i] <- i
}
plot_abs_vs_rough_custom(
    agg_elev_GCFR$`1`, agg_elev_SWAFR$`1`,
    c("roughness", "TRI", "TPI")
)

# GCFR
do_PCAandkmeans(
    region_a_dir = paste0(
        reswd, "prelim_anal_elev_GCFR_2017-06-28/", "dfs_elev_GCFR/",
        "extracted_abs_vs_rough_elev_GCFR_buffered_df.csv"
    ),
    region_a_name = "GCFR", border_a = GCFR_border,
    var_name = "elev"
)

# SWAFR
do_PCAandkmeans(
    region_a_dir = paste0(
        reswd, "prelim_anal_elev_SWAFR_2017-06-28/", "dfs_elev_SWAFR/",
        "extracted_abs_vs_rough_elev_SWAFR_buffered_df.csv"
    ),
    region_a_name = "SWAFR", border_a = SWAFR_border,
    var_name = "elev"
)

# Both
do_PCAandkmeans(
    region_a_dir = paste0(
        reswd, "prelim_anal_elev_GCFR_2017-06-28/", "dfs_elev_GCFR/",
        "extracted_abs_vs_rough_elev_GCFR_buffered_df.csv"
    ),
    region_b_dir = paste0(
        reswd, "prelim_anal_elev_SWAFR_2017-06-28/", "dfs_elev_SWAFR/",
        "extracted_abs_vs_rough_elev_SWAFR_buffered_df.csv"
    ),
    region_a_name = "GCFR",  border_a = GCFR_border,
    region_b_name = "SWAFR", border_b = SWAFR_border,
    var_name = "elev",
    do_kmeans = FALSE    #TRUE
)

# WOOOOOOOW!!!!!!!!!!!!!!!!
# YESSSSSSSSS!!!!!!!!!!!!!
# re: kmeans clustering: AAAAAAAAHHHHHHHH!!!! YESSSSSSS!!!


# '' MA_LST '' -----------------------------------------------------------------

agg_MA_LST_GCFR <- vector("list", length = 15)
for (i in seq_along(agg_MA_LST_GCFR)) {
    agg_MA_LST_GCFR[[i]] <- raster(paste0(
        reswd, "prelim_anal_MA_LST_GCFR_2017-06-28/",
        "raster_lists_MA_LST_GCFR_GRDs/",
        "agg_MA_LST_GCFR_fact_", i, ".grd"
    ))
    names(agg_MA_LST_GCFR)[i] <- i
}
agg_MA_LST_SWAFR <- vector("list", length = 15)
for (i in seq_along(agg_MA_LST_SWAFR)) {
    agg_MA_LST_SWAFR[[i]] <- raster(paste0(
        reswd, "prelim_anal_MA_LST_SWAFR_2017-06-28/",
        "raster_lists_MA_LST_SWAFR_GRDs/",
        "agg_MA_LST_SWAFR_fact_", i, ".grd"
    ))
    names(agg_MA_LST_SWAFR)[i] <- i
}
plot_abs_vs_rough_custom(
    agg_MA_LST_GCFR$`1`, agg_MA_LST_SWAFR$`1`,
    c("roughness", "TRI", "TPI")
)

# GCFR
do_PCAandkmeans(
    region_a_dir = paste0(
        reswd, "prelim_anal_MA_LST_GCFR_2017-06-28/", "dfs_MA_LST_GCFR/",
        "extracted_abs_vs_rough_MA_LST_GCFR_buffered_df.csv"
    ),
    region_a_name = "GCFR", border_a = GCFR_border,
    var_name = "MA_LST"
)

# SWAFR
do_PCAandkmeans(
    region_a_dir = paste0(
        reswd, "prelim_anal_MA_LST_SWAFR_2017-06-28/", "dfs_MA_LST_SWAFR/",
        "extracted_abs_vs_rough_MA_LST_SWAFR_buffered_df.csv"
    ),
    region_a_name = "SWAFR", border_a = SWAFR_border,
    var_name = "MA_LST"
)

# BOTH
do_PCAandkmeans(
    region_a_dir = paste0(
        reswd, "prelim_anal_MA_LST_GCFR_2017-06-28/", "dfs_MA_LST_GCFR/",
        "extracted_abs_vs_rough_MA_LST_GCFR_buffered_df.csv"
    ),
    region_b_dir = paste0(
        reswd, "prelim_anal_MA_LST_SWAFR_2017-06-28/", "dfs_MA_LST_SWAFR/",
        "extracted_abs_vs_rough_MA_LST_SWAFR_buffered_df.csv"
    ),
    region_a_name = "GCFR",  border_a = GCFR_border,
    region_b_name = "SWAFR", border_b = SWAFR_border,
    var_name = "MA_LST",
    do_kmeans = FALSE    #TRUE
)


# </> --------------------------------------------------------------------------

