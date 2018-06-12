# [...] ------------------------------------------------------------------------

GCFR_vars_pca <- do_env_vars_PCA(
    vars = c(agg_elev_GCFR$`1`, agg_MAP_GCFR$`1`, agg_MA_LST_GCFR$`1`),
    border = GCFR_border
)
# Variation in elevation is most aligned with PC1 in the GCFR! ...
GCFR_vars_pca$vars_pca$rotation
# = 0.99 rotation for elev w/ PC1
# And MAP ~ PC2
GCFR_vars_pca$rough_vars_pca$rotation
# = 0.98 rotation for rough_elev w/ PC1
# And MAP ~ PC2
GCFR_vars_pca$combo_pca$rotation
# = -0.98 rotation for elev w/ PC1
# And rough_elev & MAP ~ PC2

SWAFR_vars_pca <- do_env_vars_PCA(
    vars = c(agg_elev_SWAFR$`1`, agg_MAP_SWAFR$`1`, agg_MA_LST_SWAFR$`1`),
    border = SWAFR_border
)
SWAFR_vars_pca$vars_pca
SWAFR_vars_pca$rough_vars_pca
SWAFR_vars_pca$combo_pca
# But in the SWAFR, MAP is the closest to PC1!
# And elev ~ PC2
# (across all 3 PCAs!)
# The roughness vars *barely*/don't matter! yay! flat oz!!!

# 2017-07-01 16:40 TODO --- Repeat this analysis across scales?

plot(agg_MA_LST_SWAFR$`1`[] ~ agg_elev_SWAFR$`1`[])
plot(agg_MAP_SWAFR$`1`[] ~ agg_elev_SWAFR$`1`[])
# ????? why????? diferent length vectors????