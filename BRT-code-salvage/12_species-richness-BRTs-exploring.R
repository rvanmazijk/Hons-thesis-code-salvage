# Exploring the gbm.step saved BRTs
# Hons thesis
# R. van Mazijk

GCFR_QDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "G:/gbm_step_outputs_2017-09-30",
    scale = "QDS",
    region = "GCFR"
)
GCFR_HDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "G:/gbm_step_outputs_2017-09-30",
    scale = "HDS",
    region = "GCFR"
)
GCFR_3QDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "G:/gbm_step_outputs_2017-09-30",
    scale = "3QDS",
    region = "GCFR"
)

SWAFR_QDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "G:/gbm_step_outputs_2017-09-30",
    scale = "QDS",
    region = "SWAFR"
)
SWAFR_HDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "G:/gbm_step_outputs_2017-09-30",
    scale = "HDS",
    region = "SWAFR"
)
SWAFR_3QDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "G:/gbm_step_outputs_2017-09-30",
    scale = "3QDS",
    region = "SWAFR"
)

BOTH_QDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "G:/gbm_step_outputs_2017-09-30",
    scale = "QDS",
    region = "BOTH"
)
BOTH_HDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "G:/gbm_step_outputs_2017-09-30",
    scale = "HDS",
    region = "BOTH"
)
BOTH_3QDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "G:/gbm_step_outputs_2017-09-30",
    scale = "3QDS",
    region = "BOTH"
)

GCFR_saved_BRTs_summary <- as_tibble(rbind(
    cbind(scale = "QDS", GCFR_QDS_saved_BRTs$model_summaries),
    cbind(scale = "HDS", GCFR_HDS_saved_BRTs$model_summaries),
    cbind(scale = "3QDS", GCFR_3QDS_saved_BRTs$model_summaries)
))
SWAFR_saved_BRTs_summary <- as_tibble(rbind(
    cbind(scale = "QDS", SWAFR_QDS_saved_BRTs$model_summaries),
    cbind(scale = "HDS", SWAFR_HDS_saved_BRTs$model_summaries),
    cbind(scale = "3QDS", SWAFR_3QDS_saved_BRTs$model_summaries)
))
BOTH_saved_BRTs_summary <- as_tibble(rbind(
    cbind(scale = "QDS", BOTH_QDS_saved_BRTs$model_summaries),
    cbind(scale = "HDS", BOTH_HDS_saved_BRTs$model_summaries),
    cbind(scale = "3QDS", BOTH_3QDS_saved_BRTs$model_summaries)
))

saved_BRTs_summary <- as_tibble(rbind(
    cbind(region = "GCFR", GCFR_saved_BRTs_summary),
    cbind(region = "SWAFR", SWAFR_saved_BRTs_summary),
    cbind(region = "BOTH", BOTH_saved_BRTs_summary)
))

ggplot(saved_BRTs_summary) +
    geom_tile(aes(x = tc, y = factor(lr), fill = pseudo_r2)) +
    facet_grid(region ~ scale) +
    scale_fill_distiller(palette = "Spectral") +
    theme_minimal()

ggplot(saved_BRTs_summary) +
    geom_tile(aes(x = tc, y = factor(lr), fill = nt)) +
    facet_grid(region ~ scale) +
    scale_fill_distiller(palette = "Spectral") +
    theme_minimal()

# Conclusion:
#   want to run these all again @ tol = 0.001 (default), and also @ tol = 0.0001
