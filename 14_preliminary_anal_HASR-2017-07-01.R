# Applying do_prelim_anal() with the HSAR pipeline to everything

# (For all variables so far, at facts 1:15 only, on the GCFR & SWAFR)

# Hons thesis
# Ruan van Mazijk

# created:      2017-06-23
# last edited:  2017-07-01


# Setup ---------------------------------------------------------------------------------

rm(list = ls())
# Order in which these source() cmds are run is NB
source("Scripts/i_my_funs.R")
source("Scripts/ii_my_objs.R")
source("Scripts/v_prelim_anal_fn.R")
source("Scripts/vi_rand_raster_null_fn.R")
source("Scripts/vii_compare_null_obs_fn.R")
source("Scripts/viii_prelim_rand_compare_fn.R")


# [√] MAP -------------------------------------------------------------------------------

# GCFR_buffered

if (FALSE) { #<DEPREC>

  do_prelim_anal(
    x                 = MAP_GCFR_buffered,  # res = 0.05 degrees, but i named it badly lol
    border            = GCFR_border,
    border_buffer     = GCFR_border_buffered,
    facts             = 2:15,
    var_name          = "MAP",
    region_name       = "GCFR", 
    reswd_temp        = paste0(reswd, "prelim_anal_MAP_GCFR_2017-06-28/"),
    x_tester          = MAP_GCFR_0.05,
    how_many_rand_pts = 1000,
    save_grd          = TRUE
  )
  
  do_rand_null(
    x                 = MAP_GCFR_buffered,
    how_many_SARs     = 1,  # note: `r SAR <- raster %>% agg() %>% shuff()`
    facts             = 2:15,
    border            = GCFR_border,
    border_buffer     = GCFR_border_buffered,
    var_name          = "MAP",
    region_name       = "GCFR", 
    reswd_temp        = paste0(reswd, "rand_null_MAP_GCFR_2017-06-29/"),
    x_tester          = MAP_GCFR_0.05,
    how_many_rand_pts = 1000,
    save_grd          = TRUE
  )
  
  compare_null_obs(
    obs_dir     = paste0(reswd, "prelim_anal_MAP_GCFR_2017-06-28/"),
    null_dir    = paste0(reswd, "rand_null_MAP_GCFR_2017-06-29/"),
    compare_dir = paste0(reswd, "compare_null_obs_MAP_GCFR_2017-06-29/"),
    var_name    = "MAP",
    region_name = "GCFR"
  )
  
}

# OR

prelim_rand_compare(
  x                   = MAP_GCFR_buffered,
  need_do_prelim_anal = TRUE,
  need_do_rand_null   = TRUE,
  border              = GCFR_border,
  border_buffer       = GCFR_border_buffered,
  facts               = 2:15,
  var_name            = "MAP",
  region_name         = "GCFR",
  how_many_SARs       = 1,
  how_many_rand_pts   = 1000,
  x_tester            = MAP_GCFR_0.05,
  obs_dir             = paste0(reswd, "prelim_anal_MAP_GCFR_2017-06-28/"),
  null_dir            = paste0(reswd, "rand_null_MAP_GCFR_2017-06-29/"),
  compare_dir         = paste0(reswd, "compare_null_obs_MAP_GCFR_2017-06-29/")
)


# SWAFR_buffered

prelim_rand_compare(
  x                   = MAP_SWAFR_0.05_buffered,
  need_do_prelim_anal = TRUE,
  need_do_rand_null   = TRUE,
  border              = SWAFR_border,
  border_buffer       = SWAFR_border_buffered,
  facts               = 2:15,
  var_name            = "MAP",
  region_name         = "SWAFR",
  how_many_SARs       = 1,
  how_many_rand_pts   = 1000,
  x_tester            = MAP_SWAFR_0.05,
  obs_dir             = paste0(reswd, "prelim_anal_MAP_SWAFR_2017-06-28/"),
  null_dir            = paste0(reswd, "rand_null_MAP_SWAFR_2017-06-29/"),
  compare_dir         = paste0(reswd, "compare_null_obs_MAP_SWAFR_2017-06-29/")
)


# [√] MA_LST ----------------------------------------------------------------------------

# GCFR_buffered

prelim_rand_compare(
  x                   = MA_LST_GCFR_0.05_buffered,
  need_do_prelim_anal = TRUE,
  need_do_rand_null   = TRUE,
  border              = GCFR_border,
  border_buffer       = GCFR_border_buffered,
  facts               = 2:15,
  var_name            = "MA_LST",
  region_name         = "GCFR",
  how_many_SARs       = 1,
  how_many_rand_pts   = 1000,
  x_tester            = MAP_GCFR_0.05,  # always using the same raster to make the rand_pts
  obs_dir             = paste0(reswd, "prelim_anal_MA_LST_GCFR_2017-06-28/"),
  null_dir            = paste0(reswd, "rand_null_MA_LST_GCFR_2017-06-29/"),
  compare_dir         = paste0(reswd, "compare_null_obs_MA_LST_GCFR_2017-06-29/")
)


# SWAFR_buffered

prelim_rand_compare(
  x                   = MA_LST_SWAFR_0.05_buffered,
  need_do_prelim_anal = TRUE,
  need_do_rand_null   = TRUE,
  border              = SWAFR_border,
  border_buffer       = SWAFR_border_buffered,
  facts               = 2:15,
  var_name            = "MA_LST",
  region_name         = "SWAFR",
  how_many_SARs       = 1,
  how_many_rand_pts   = 1000,
  x_tester            = MAP_SWAFR_0.05,  # always using the same raster to make the rand_pts
  obs_dir             = paste0(reswd, "prelim_anal_MA_LST_SWAFR_2017-06-28/"),
  null_dir            = paste0(reswd, "rand_null_MA_LST_SWAFR_2017-06-29/"),
  compare_dir         = paste0(reswd, "compare_null_obs_MA_LST_SWAFR_2017-06-29/")
)


# [√] Elev ------------------------------------------------------------------------------

# GCFR_buffered

prelim_rand_compare(
  x                   = SRTM_GCFR_0.05_buffered,
  need_do_prelim_anal = TRUE,
  need_do_rand_null   = TRUE,
  border              = GCFR_border,
  border_buffer       = GCFR_border_buffered,
  facts               = 2:15,
  var_name            = "elev",
  region_name         = "GCFR",
  how_many_SARs       = 1,
  how_many_rand_pts   = 1000,
  x_tester            = MAP_GCFR_0.05,  # always using the same raster to make the rand_pts
  obs_dir             = paste0(reswd, "prelim_anal_elev_GCFR_2017-06-28/"),
  null_dir            = paste0(reswd, "rand_null_elev_GCFR_2017-06-29/"),
  compare_dir         = paste0(reswd, "compare_null_obs_elev_GCFR_2017-06-29/")
)


# SWAFR_buffered 

prelim_rand_compare(
  x                   = SRTM_SWAFR_0.05_buffered,
  need_do_prelim_anal = TRUE,
  need_do_rand_null   = TRUE,
  border              = SWAFR_border,
  border_buffer       = SWAFR_border_buffered,
  facts               = 2:15,
  var_name            = "elev",
  region_name         = "SWAFR",
  how_many_SARs       = 1,
  how_many_rand_pts   = 1000,
  x_tester            = MAP_SWAFR_0.05,  # always using the same raster to make the rand_pts
  obs_dir             = paste0(reswd, "prelim_anal_elev_SWAFR_2017-06-28/"),
  null_dir            = paste0(reswd, "rand_null_elev_SWAFR_2017-06-29/"),
  compare_dir         = paste0(reswd, "compare_null_obs_elev_SWAFR_2017-06-29/")
)


# </> -----------------------------------------------------------------------------------

