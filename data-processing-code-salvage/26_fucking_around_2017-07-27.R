# Fucking around

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


# ------------------------------------------------------------------------------

plot(
    GCFR_richness$raster[] ~ agg_MAP_GCFR$`5`[]
)

# TODO: Extracted MAP vs extracted richness @ 1000 random sample pts
# TODO: '' for all vars