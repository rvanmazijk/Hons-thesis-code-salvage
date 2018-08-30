# ...
# Hons thesis
# R. van Mazijk

# GCFR -------------------------------------------------------------------------

# Import saved BRT-models
GCFR_QDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30",
    scale = "QDS",
    region = "GCFR"
)
GCFR_HDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30",
    scale = "HDS",
    region = "GCFR"
)
GCFR_3QDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30",
    scale = "3QDS",
    region = "GCFR"
)

# Extract the chosen lr-tc config
GCFR_QDS_BRT_candidate <- GCFR_QDS_saved_BRTs$saved_BRTs$tol_5e_04_tc_5_lr_0.001
GCFR_HDS_BRT_candidate <- GCFR_HDS_saved_BRTs$saved_BRTs$tol_5e_04_tc_5_lr_0.001
GCFR_3QDS_BRT_candidate <- GCFR_3QDS_saved_BRTs$saved_BRTs$tol_5e_04_tc_5_lr_0.001

# Run gbm.simplify (w/ n.drops = "auto" (default))
GCFR_QDS_BRT_candidate.simp <- gbm.simplify(GCFR_QDS_BRT_candidate)
GCFR_HDS_BRT_candidate.simp <- gbm.simplify(GCFR_HDS_BRT_candidate)
GCFR_3QDS_BRT_candidate.simp <- gbm.simplify(GCFR_3QDS_BRT_candidate)

# Save
saveRDS(
    GCFR_QDS_BRT_candidate.simp,
    paste0(
        "/Volumes/RUAN_PVT/gbm_simplify_outputs_2017-10-19/",
        "GCFR_QDS_BRT_candidate.simp.rds"
    )
)
saveRDS(
    GCFR_HDS_BRT_candidate.simp,
    paste0(
        "/Volumes/RUAN_PVT/gbm_simplify_outputs_2017-10-19/",
        "GCFR_HDS_BRT_candidate.simp.rds"
    )
)
saveRDS(
    GCFR_3QDS_BRT_candidate.simp,
    paste0(
        "/Volumes/RUAN_PVT/gbm_simplify_outputs_2017-10-19/",
        "GCFR_3QDS_BRT_candidate.simp.rds"
    )
)


# SWAFR ------------------------------------------------------------------------

# Import
SWAFR_QDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30",
    scale = "QDS",
    region = "SWAFR"
)
SWAFR_HDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30",
    scale = "HDS",
    region = "SWAFR"
)
SWAFR_3QDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30",
    scale = "3QDS",
    region = "SWAFR"
)

# Extract
SWAFR_QDS_BRT_candidate <- SWAFR_QDS_saved_BRTs$saved_BRTs$tol_5e_04_tc_5_lr_0.001
SWAFR_HDS_BRT_candidate <- SWAFR_HDS_saved_BRTs$saved_BRTs$tol_5e_04_tc_5_lr_0.001
SWAFR_3QDS_BRT_candidate <- SWAFR_3QDS_saved_BRTs$saved_BRTs$tol_5e_04_tc_5_lr_0.001

# Run
SWAFR_QDS_BRT_candidate.simp <- gbm.simplify(SWAFR_QDS_BRT_candidate)
SWAFR_HDS_BRT_candidate.simp <- gbm.simplify(SWAFR_HDS_BRT_candidate)
SWAFR_3QDS_BRT_candidate.simp <- gbm.simplify(SWAFR_3QDS_BRT_candidate)

# Save
saveRDS(
    SWAFR_QDS_BRT_candidate.simp,
    paste0(
        "/Volumes/RUAN_PVT/gbm_simplify_outputs_2017-10-19/",
        "SWAFR_QDS_BRT_candidate.simp.rds"
    )
)
saveRDS(
    SWAFR_HDS_BRT_candidate.simp,
    paste0(
        "/Volumes/RUAN_PVT/gbm_simplify_outputs_2017-10-19/",
        "SWAFR_HDS_BRT_candidate.simp.rds"
    )
)
saveRDS(
    SWAFR_3QDS_BRT_candidate.simp,
    paste0(
        "/Volumes/RUAN_PVT/gbm_simplify_outputs_2017-10-19/",
        "SWAFR_3QDS_BRT_candidate.simp.rds"
    )
)


# BOTH -------------------------------------------------------------------------

# Import
BOTH_QDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30",
    scale = "QDS",
    region = "BOTH"
)
BOTH_HDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30",
    scale = "HDS",
    region = "BOTH"
)
BOTH_3QDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30",
    scale = "3QDS",
    region = "BOTH"
)

# Extract
BOTH_QDS_BRT_candidate <- BOTH_QDS_saved_BRTs$saved_BRTs$tol_5e_04_tc_5_lr_0.001
BOTH_HDS_BRT_candidate <- BOTH_HDS_saved_BRTs$saved_BRTs$tol_5e_04_tc_5_lr_0.001
BOTH_3QDS_BRT_candidate <- BOTH_3QDS_saved_BRTs$saved_BRTs$tol_5e_04_tc_5_lr_0.001

# Run
BOTH_QDS_BRT_candidate.simp <- gbm.simplify(BOTH_QDS_BRT_candidate)
BOTH_HDS_BRT_candidate.simp <- gbm.simplify(BOTH_HDS_BRT_candidate)
BOTH_3QDS_BRT_candidate.simp <- gbm.simplify(BOTH_3QDS_BRT_candidate)

# Save
saveRDS(
    BOTH_QDS_BRT_candidate.simp,
    paste0(
        "/Volumes/RUAN_PVT/gbm_simplify_outputs_2017-10-19/",
        "BOTH_QDS_BRT_candidate.simp.rds"
    )
)
saveRDS(
    BOTH_HDS_BRT_candidate.simp,
    paste0(
        "/Volumes/RUAN_PVT/gbm_simplify_outputs_2017-10-19/",
        "BOTH_HDS_BRT_candidate.simp.rds"
    )
)
saveRDS(
    BOTH_3QDS_BRT_candidate.simp,
    paste0(
        "/Volumes/RUAN_PVT/gbm_simplify_outputs_2017-10-19/",
        "BOTH_3QDS_BRT_candidate.simp.rds"
    )
)
