# Exploring the gbm.step saved BRTs
# Hons thesis
# R. van Mazijk

# GCFR -------------------------------------------------------------------------

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

# Explore quality & visualise
GCFR_saved_BRTs_summary <- as_tibble(rbind(
    cbind(scale = "QDS", GCFR_QDS_saved_BRTs$model_summaries),
    cbind(scale = "HDS", GCFR_HDS_saved_BRTs$model_summaries),
    cbind(scale = "3QDS", GCFR_3QDS_saved_BRTs$model_summaries)
))
GCFR_r2_nt_plot <- make_r2_and_nt_plots(GCFR_saved_BRTs_summary)
GCFR_r2_nt_plot_panelled <- plot_grid(align = "hv",
    nrow = 2,
    labels = c("(A)", "(B)"),
    GCFR_r2_nt_plot$r2_plot,
    GCFR_r2_nt_plot$nt_plot
)
ggsave(
    filename = here::here(
        "Results",
        "GCFR_r2_nt_plot_panelled.tiff"
    ),
    GCFR_r2_nt_plot_panelled
)
# Done!
# Now visualsise/quantify nt ~ lr + tc, r2 ~ lr + tc
r2_m <- lm(pseudo_r2 ~ tc + lr, GCFR_saved_BRTs_summary)
write.csv(broom::tidy(r2_m), file = here::here(
    "Results",
    "r2_m_table_GCFR.csv"
))
nt_m <- lm(nt ~ tc + lr, GCFR_saved_BRTs_summary)
write.csv(broom::tidy(nt_m), file = here::here(
    "Results",
    "nt_m_table_GCFR.csv"
))

# Explore variables' contributions
GCFR_contrib_summary <- make_contrib_summary(list(
    scale_QDS = GCFR_QDS_saved_BRTs$saved_BRTs,
    scale_HDS = GCFR_HDS_saved_BRTs$saved_BRTs,
    scale_3QDS = GCFR_3QDS_saved_BRTs$saved_BRTs
))
GCFR_contrib_summary2 <-
    foreach(scale = GCFR_contrib_summary) %do% {
        foreach(lr = scale) %do% {
            foreach(tc = lr) %do% {
                tc$contributions
            }
        }
    }
names(GCFR_contrib_summary2) <- names(GCFR_contrib_summary)
for (i in seq_along(GCFR_contrib_summary2)) {
    names(GCFR_contrib_summary2[[i]]) <- c(
        "lr_0.01",
        "lr_0.005",
        "lr_0.001",
        "lr_5e_04",
        "lr_1e_04"
    )
}
GCFR_contrib_summary2

# Now to make this tidy!

nvar <- nrow(GCFR_contrib_summary2$scale_QDS$lr_0.01[[1]])
foofoo <- array(dim = c(3, 5, 5, nvar, 2))

dimnames(foofoo)[[1]] <- c("QDS", "HDS", "TQDS")
for (i in seq_along(GCFR_contrib_summary2)) {

    dimnames(foofoo)[[2]] <- names(GCFR_contrib_summary2$scale_QDS)
    for (j in seq_along(GCFR_contrib_summary2[[i]])) {

        dimnames(foofoo)[[3]] <- paste0("tc_", 1:5)
        for (k in seq_along(GCFR_contrib_summary2[[i]][[j]])) {

            dimnames(foofoo)[[5]] <- c("contrib", "var_name")
            for (h in 1:nrow(GCFR_contrib_summary2[[i]][[j]][[k]])) {
                foofoo[i, j, k, h, 1] <-
                    GCFR_contrib_summary2[[i]][[j]][[k]]$rel.inf[[h]]
                foofoo[i, j, k, h, 2] <-
                    rownames(GCFR_contrib_summary2[[i]][[j]][[k]])[[h]]
            }
        }
    }
}

foofoo_tidy_for_a_scale_QDS <- as_tibble(cbind(
    lr = c(
        rep(names(GCFR_contrib_summary2$scale_QDS)[1], 125),
        rep(names(GCFR_contrib_summary2$scale_QDS)[2], 125),
        rep(names(GCFR_contrib_summary2$scale_QDS)[3], 125),
        rep(names(GCFR_contrib_summary2$scale_QDS)[4], 125),
        rep(names(GCFR_contrib_summary2$scale_QDS)[5], 125)
    ),
    rbind(
        as_tibble(cbind(
            sort(rep(names(foofoo[1, 1, , 1, 1]), 25)),
            rbind(
                foofoo[1, 1, 1, ,],
                foofoo[1, 1, 2, ,],
                foofoo[1, 1, 3, ,],
                foofoo[1, 1, 4, ,],
                foofoo[1, 1, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[1, 1, , 1, 1]), 25)),
            rbind(
                foofoo[1, 2, 1, ,],
                foofoo[1, 2, 2, ,],
                foofoo[1, 2, 3, ,],
                foofoo[1, 2, 4, ,],
                foofoo[1, 2, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[1, 1, , 1, 1]), 25)),
            rbind(
                foofoo[1, 3, 1, ,],
                foofoo[1, 3, 2, ,],
                foofoo[1, 3, 3, ,],
                foofoo[1, 3, 4, ,],
                foofoo[1, 3, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[1, 1, , 1, 1]), 25)),
            rbind(
                foofoo[1, 4, 1, ,],
                foofoo[1, 4, 2, ,],
                foofoo[1, 4, 3, ,],
                foofoo[1, 4, 4, ,],
                foofoo[1, 4, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[1, 1, , 1, 1]), 25)),
            rbind(
                foofoo[1, 5, 1, ,],
                foofoo[1, 5, 2, ,],
                foofoo[1, 5, 3, ,],
                foofoo[1, 5, 4, ,],
                foofoo[1, 5, 5, ,]
            )
        ))
    )
))
foofoo_tidy_for_a_scale_HDS <- as_tibble(cbind(
    lr = c(
        rep(names(GCFR_contrib_summary2$scale_QDS)[1], 125),
        rep(names(GCFR_contrib_summary2$scale_QDS)[2], 125),
        rep(names(GCFR_contrib_summary2$scale_QDS)[3], 125),
        rep(names(GCFR_contrib_summary2$scale_QDS)[4], 125),
        rep(names(GCFR_contrib_summary2$scale_QDS)[5], 125)
    ),
    rbind(
        as_tibble(cbind(
            sort(rep(names(foofoo[2, 1, , 1, 1]), 25)),
            rbind(
                foofoo[2, 1, 1, ,],
                foofoo[2, 1, 2, ,],
                foofoo[2, 1, 3, ,],
                foofoo[2, 1, 4, ,],
                foofoo[2, 1, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[2, 1, , 1, 1]), 25)),
            rbind(
                foofoo[2, 2, 1, ,],
                foofoo[2, 2, 2, ,],
                foofoo[2, 2, 3, ,],
                foofoo[2, 2, 4, ,],
                foofoo[2, 2, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[2, 1, , 1, 1]), 25)),
            rbind(
                foofoo[2, 3, 1, ,],
                foofoo[2, 3, 2, ,],
                foofoo[2, 3, 3, ,],
                foofoo[2, 3, 4, ,],
                foofoo[2, 3, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[2, 1, , 1, 1]), 25)),
            rbind(
                foofoo[2, 4, 1, ,],
                foofoo[2, 4, 2, ,],
                foofoo[2, 4, 3, ,],
                foofoo[2, 4, 4, ,],
                foofoo[2, 4, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[2, 1, , 1, 1]), 25)),
            rbind(
                foofoo[2, 5, 1, ,],
                foofoo[2, 5, 2, ,],
                foofoo[2, 5, 3, ,],
                foofoo[2, 5, 4, ,],
                foofoo[2, 5, 5, ,]
            )
        ))
    )
))
foofoo_tidy_for_a_scale_3QDS <- as_tibble(cbind(
    lr = c(
        rep(names(GCFR_contrib_summary2$scale_QDS)[1], 125),
        rep(names(GCFR_contrib_summary2$scale_QDS)[2], 125),
        rep(names(GCFR_contrib_summary2$scale_QDS)[3], 125),
        rep(names(GCFR_contrib_summary2$scale_QDS)[4], 125),
        rep(names(GCFR_contrib_summary2$scale_QDS)[5], 125)
    ),
    rbind(
        as_tibble(cbind(
            sort(rep(names(foofoo[3, 1, , 1, 1]), 25)),
            rbind(
                foofoo[3, 1, 1, ,],
                foofoo[3, 1, 2, ,],
                foofoo[3, 1, 3, ,],
                foofoo[3, 1, 4, ,],
                foofoo[3, 1, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[3, 1, , 1, 1]), 5)),
            rbind(
                foofoo[3, 2, 1, ,],
                foofoo[3, 2, 2, ,],
                foofoo[3, 2, 3, ,],
                foofoo[3, 2, 4, ,],
                foofoo[3, 2, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[3, 1, , 1, 1]), 25)),
            rbind(
                foofoo[3, 3, 1, ,],
                foofoo[3, 3, 2, ,],
                foofoo[3, 3, 3, ,],
                foofoo[3, 3, 4, ,],
                foofoo[3, 3, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[3, 1, , 1, 1]), 25)),
            rbind(
                foofoo[3, 4, 1, ,],
                foofoo[3, 4, 2, ,],
                foofoo[3, 4, 3, ,],
                foofoo[3, 4, 4, ,],
                foofoo[3, 4, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[3, 1, , 1, 1]), 25)),
            rbind(
                foofoo[3, 5, 1, ,],
                foofoo[3, 5, 2, ,],
                foofoo[3, 5, 3, ,],
                foofoo[3, 5, 4, ,],
                foofoo[3, 5, 5, ,]
            )
        ))
    )
))
foofoo_tidy <- as_tibble(cbind(
    region = "GCFR",
    rbind(
        cbind(
            scale = "QDS",
            foofoo_tidy_for_a_scale_QDS
        ),
        cbind(
            scale = "HDS",
            foofoo_tidy_for_a_scale_HDS
        ),
        cbind(
            scale = "3QDS",
            foofoo_tidy_for_a_scale_3QDS
        )
    )
))
foofoo_tidy
GCFR_contribs_tidy <- foofoo_tidy %>%
    transmute(
        region = region,
        scale = scale,
        lr = lr,
        tc = Var.2,
        var_name = var_name,
        contrib = contrib
    )
GCFR_contribs_tidy

# Double check that all that mess worked
GCFR_contrib_summary2$scale_QDS$lr_0.01[[1]][1, 2] ==
    GCFR_contribs_tidy %>%
    filter(
        scale == "QDS",
        lr == "lr_0.01",
        tc == "tc_1",
        var_name == "NDVI"
    ) %>%
    dplyr::select(contrib)
# TRUE
GCFR_contrib_summary2$scale_HDS$lr_0.001[[3]][2, 2] ==
    GCFR_contribs_tidy %>%
    filter(
        scale == "HDS",
        lr == "lr_0.001",
        tc == "tc_3",
        var_name == "NDVI"
    ) %>%
    dplyr::select(contrib)
# TRUE

write.csv(GCFR_contribs_tidy, here::here(
    "Results",
    "GCFR_contribs_tidy.csv"
))


# SWAFR ------------------------------------------------------------------------

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

# Explore quality & visualise
SWAFR_saved_BRTs_summary <- as_tibble(rbind(
    cbind(scale = "QDS", SWAFR_QDS_saved_BRTs$model_summaries),
    cbind(scale = "HDS", SWAFR_HDS_saved_BRTs$model_summaries),
    cbind(scale = "3QDS", SWAFR_3QDS_saved_BRTs$model_summaries)
))
make_r2_and_nt_plots(SWAFR_saved_BRTs_summary)
SWAFR_r2_nt_plot <- make_r2_and_nt_plots(SWAFR_saved_BRTs_summary)
SWAFR_r2_nt_plot_panelled <- plot_grid(
    align = "hv",
    nrow = 2,
    labels = c("(A)", "(B)"),
    SWAFR_r2_nt_plot$r2_plot,
    SWAFR_r2_nt_plot$nt_plot
)
ggsave(
    filename = here::here(
        "Results",
        "SWAFR_r2_nt_plot_panelled.tiff"
    ),
    SWAFR_r2_nt_plot_panelled
)
# Done!
# Now visualsise/quantify nt ~ lr + tc, r2 ~ lr + tc
r2_m <- lm(pseudo_r2 ~ tc + lr, SWAFR_saved_BRTs_summary)
write.csv(broom::tidy(r2_m), file = here::here(
    "Results",
    "r2_m_table_SWAFR.csv"
))
nt_m <- lm(nt ~ tc + lr, SWAFR_saved_BRTs_summary)
write.csv(broom::tidy(nt_m), file = here::here(
    "Results",
    "nt_m_table_SWAFR.csv"
))

# Explore variables' contributions
SWAFR_contrib_summary <- make_contrib_summary(list(
    scale_QDS = SWAFR_QDS_saved_BRTs$saved_BRTs,
    scale_HDS = SWAFR_HDS_saved_BRTs$saved_BRTs,
    scale_3QDS = SWAFR_3QDS_saved_BRTs$saved_BRTs
))
SWAFR_contrib_summary2 <-
    foreach(scale = SWAFR_contrib_summary) %do% {
        foreach(lr = scale) %do% {
            foreach(tc = lr) %do% {
                tc$contributions
            }
        }
    }
names(SWAFR_contrib_summary2) <- names(SWAFR_contrib_summary)
for (i in seq_along(SWAFR_contrib_summary2)) {
    names(SWAFR_contrib_summary2[[i]]) <- c(
        "lr_0.01",
        "lr_0.005",
        "lr_0.001",
        "lr_5e_04",
        "lr_1e_04"
    )
}
SWAFR_contrib_summary2

# Now to make this tidy!

nvar <- nrow(SWAFR_contrib_summary2$scale_QDS$lr_0.01[[1]])
foofoo <- array(dim = c(3, 5, 5, nvar, 2))

dimnames(foofoo)[[1]] <- c("QDS", "HDS", "TQDS")
for (i in seq_along(SWAFR_contrib_summary2)) {

    dimnames(foofoo)[[2]] <- names(SWAFR_contrib_summary2$scale_QDS)
    for (j in seq_along(SWAFR_contrib_summary2[[i]])) {

        dimnames(foofoo)[[3]] <- paste0("tc_", 1:5)
        for (k in seq_along(SWAFR_contrib_summary2[[i]][[j]])) {

            dimnames(foofoo)[[5]] <- c("contrib", "var_name")
            for (h in 1:nrow(SWAFR_contrib_summary2[[i]][[j]][[k]])) {
                foofoo[i, j, k, h, 1] <-
                    SWAFR_contrib_summary2[[i]][[j]][[k]]$rel.inf[[h]]
                foofoo[i, j, k, h, 2] <-
                    rownames(SWAFR_contrib_summary2[[i]][[j]][[k]])[[h]]
            }
        }
    }
}

foofoo_tidy_for_a_scale_QDS <- as_tibble(cbind(
    lr = c(
        rep(names(SWAFR_contrib_summary2$scale_QDS)[1], 125),
        rep(names(SWAFR_contrib_summary2$scale_QDS)[2], 125),
        rep(names(SWAFR_contrib_summary2$scale_QDS)[3], 125),
        rep(names(SWAFR_contrib_summary2$scale_QDS)[4], 125),
        rep(names(SWAFR_contrib_summary2$scale_QDS)[5], 125)
    ),
    rbind(
        as_tibble(cbind(
            sort(rep(names(foofoo[1, 1, , 1, 1]), 25)),
            rbind(
                foofoo[1, 1, 1, ,],
                foofoo[1, 1, 2, ,],
                foofoo[1, 1, 3, ,],
                foofoo[1, 1, 4, ,],
                foofoo[1, 1, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[1, 1, , 1, 1]), 25)),
            rbind(
                foofoo[1, 2, 1, ,],
                foofoo[1, 2, 2, ,],
                foofoo[1, 2, 3, ,],
                foofoo[1, 2, 4, ,],
                foofoo[1, 2, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[1, 1, , 1, 1]), 25)),
            rbind(
                foofoo[1, 3, 1, ,],
                foofoo[1, 3, 2, ,],
                foofoo[1, 3, 3, ,],
                foofoo[1, 3, 4, ,],
                foofoo[1, 3, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[1, 1, , 1, 1]), 25)),
            rbind(
                foofoo[1, 4, 1, ,],
                foofoo[1, 4, 2, ,],
                foofoo[1, 4, 3, ,],
                foofoo[1, 4, 4, ,],
                foofoo[1, 4, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[1, 1, , 1, 1]), 25)),
            rbind(
                foofoo[1, 5, 1, ,],
                foofoo[1, 5, 2, ,],
                foofoo[1, 5, 3, ,],
                foofoo[1, 5, 4, ,],
                foofoo[1, 5, 5, ,]
            )
        ))
    )
))
foofoo_tidy_for_a_scale_HDS <- as_tibble(cbind(
    lr = c(
        rep(names(SWAFR_contrib_summary2$scale_QDS)[1], 125),
        rep(names(SWAFR_contrib_summary2$scale_QDS)[2], 125),
        rep(names(SWAFR_contrib_summary2$scale_QDS)[3], 125),
        rep(names(SWAFR_contrib_summary2$scale_QDS)[4], 125),
        rep(names(SWAFR_contrib_summary2$scale_QDS)[5], 125)
    ),
    rbind(
        as_tibble(cbind(
            sort(rep(names(foofoo[2, 1, , 1, 1]), 25)),
            rbind(
                foofoo[2, 1, 1, ,],
                foofoo[2, 1, 2, ,],
                foofoo[2, 1, 3, ,],
                foofoo[2, 1, 4, ,],
                foofoo[2, 1, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[2, 1, , 1, 1]), 25)),
            rbind(
                foofoo[2, 2, 1, ,],
                foofoo[2, 2, 2, ,],
                foofoo[2, 2, 3, ,],
                foofoo[2, 2, 4, ,],
                foofoo[2, 2, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[2, 1, , 1, 1]), 25)),
            rbind(
                foofoo[2, 3, 1, ,],
                foofoo[2, 3, 2, ,],
                foofoo[2, 3, 3, ,],
                foofoo[2, 3, 4, ,],
                foofoo[2, 3, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[2, 1, , 1, 1]), 25)),
            rbind(
                foofoo[2, 4, 1, ,],
                foofoo[2, 4, 2, ,],
                foofoo[2, 4, 3, ,],
                foofoo[2, 4, 4, ,],
                foofoo[2, 4, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[2, 1, , 1, 1]), 25)),
            rbind(
                foofoo[2, 5, 1, ,],
                foofoo[2, 5, 2, ,],
                foofoo[2, 5, 3, ,],
                foofoo[2, 5, 4, ,],
                foofoo[2, 5, 5, ,]
            )
        ))
    )
))
foofoo_tidy_for_a_scale_3QDS <- as_tibble(cbind(
    lr = c(
        rep(names(SWAFR_contrib_summary2$scale_QDS)[1], 125),
        rep(names(SWAFR_contrib_summary2$scale_QDS)[2], 125),
        rep(names(SWAFR_contrib_summary2$scale_QDS)[3], 125),
        rep(names(SWAFR_contrib_summary2$scale_QDS)[4], 125),
        rep(names(SWAFR_contrib_summary2$scale_QDS)[5], 125)
    ),
    rbind(
        as_tibble(cbind(
            sort(rep(names(foofoo[3, 1, , 1, 1]), 25)),
            rbind(
                foofoo[3, 1, 1, ,],
                foofoo[3, 1, 2, ,],
                foofoo[3, 1, 3, ,],
                foofoo[3, 1, 4, ,],
                foofoo[3, 1, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[3, 1, , 1, 1]), 5)),
            rbind(
                foofoo[3, 2, 1, ,],
                foofoo[3, 2, 2, ,],
                foofoo[3, 2, 3, ,],
                foofoo[3, 2, 4, ,],
                foofoo[3, 2, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[3, 1, , 1, 1]), 25)),
            rbind(
                foofoo[3, 3, 1, ,],
                foofoo[3, 3, 2, ,],
                foofoo[3, 3, 3, ,],
                foofoo[3, 3, 4, ,],
                foofoo[3, 3, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[3, 1, , 1, 1]), 25)),
            rbind(
                foofoo[3, 4, 1, ,],
                foofoo[3, 4, 2, ,],
                foofoo[3, 4, 3, ,],
                foofoo[3, 4, 4, ,],
                foofoo[3, 4, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[3, 1, , 1, 1]), 25)),
            rbind(
                foofoo[3, 5, 1, ,],
                foofoo[3, 5, 2, ,],
                foofoo[3, 5, 3, ,],
                foofoo[3, 5, 4, ,],
                foofoo[3, 5, 5, ,]
            )
        ))
    )
))
foofoo_tidy <- as_tibble(cbind(
    region = "SWAFR",
    rbind(
        cbind(
            scale = "QDS",
            foofoo_tidy_for_a_scale_QDS
        ),
        cbind(
            scale = "HDS",
            foofoo_tidy_for_a_scale_HDS
        ),
        cbind(
            scale = "3QDS",
            foofoo_tidy_for_a_scale_3QDS
        )
    )
))
foofoo_tidy
SWAFR_contribs_tidy <- foofoo_tidy %>%
    transmute(
        region = region,
        scale = scale,
        lr = lr,
        tc = Var.2,
        var_name = var_name,
        contrib = contrib
    )
SWAFR_contribs_tidy

# Double check that all that mess worked
SWAFR_contrib_summary2$scale_QDS$lr_0.01[[1]][1, 2] ==
    SWAFR_contribs_tidy %>%
    filter(
        scale == "QDS",
        lr == "lr_0.01",
        tc == "tc_1",
        var_name == "MAP"
    ) %>%
    dplyr::select(contrib)
# TRUE
SWAFR_contrib_summary2$scale_HDS$lr_0.001[[3]][1, 2] ==
    SWAFR_contribs_tidy %>%
    filter(
        scale == "HDS",
        lr == "lr_0.001",
        tc == "tc_3",
        var_name == "MAP"
    ) %>%
    dplyr::select(contrib)
# TRUE

write.csv(SWAFR_contribs_tidy, here::here(
    "Results",
    "SWAFR_contribs_tidy.csv"
))


# BOTH -------------------------------------------------------------------------

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

# Explore quality & visualise
BOTH_saved_BRTs_summary <- as_tibble(rbind(
    cbind(scale = "QDS", BOTH_QDS_saved_BRTs$model_summaries),
    cbind(scale = "HDS", BOTH_HDS_saved_BRTs$model_summaries),
    cbind(scale = "3QDS", BOTH_3QDS_saved_BRTs$model_summaries)
))
BOTH_r2_nt_plot <- make_r2_and_nt_plots(BOTH_saved_BRTs_summary)
BOTH_r2_nt_plot_panelled <- plot_grid(
    align = "hv",
    nrow = 2,
    labels = c("(A)", "(B)"),
    BOTH_r2_nt_plot$r2_plot,
    BOTH_r2_nt_plot$nt_plot
)
ggsave(
    filename = here::here(
        "Results",
        "BOTH_r2_nt_plot_panelled.tiff"
    ),
    BOTH_r2_nt_plot_panelled
)
# Done!
# Now visualsise/quantify nt ~ lr + tc, r2 ~ lr + tc
r2_m <- lm(pseudo_r2 ~ tc + lr, BOTH_saved_BRTs_summary)
write.csv(broom::tidy(r2_m), file = here::here(
    "Results",
    "r2_m_table_BOTH.csv"
))
nt_m <- lm(nt ~ tc + lr, BOTH_saved_BRTs_summary)
write.csv(broom::tidy(nt_m), file = here::here(
    "Results",
    "nt_m_table_BOTH.csv"
))

# Explore variables' contributions
BOTH_contrib_summary <- make_contrib_summary(list(
    scale_QDS = BOTH_QDS_saved_BRTs$saved_BRTs,
    scale_HDS = BOTH_HDS_saved_BRTs$saved_BRTs,
    scale_3QDS = BOTH_3QDS_saved_BRTs$saved_BRTs
))
BOTH_contrib_summary2 <-
    foreach(scale = BOTH_contrib_summary) %do% {
        foreach(lr = scale) %do% {
            foreach(tc = lr) %do% {
                tc$contributions
            }
        }
    }
names(BOTH_contrib_summary2) <- names(BOTH_contrib_summary)
for (i in seq_along(BOTH_contrib_summary2)) {
    names(BOTH_contrib_summary2[[i]]) <- c(
        "lr_0.01",
        "lr_0.005",
        "lr_0.001",
        "lr_5e_04",
        "lr_1e_04"
    )
}
BOTH_contrib_summary2

# Now to make this tidy!

nvar <- nrow(BOTH_contrib_summary2$scale_QDS$lr_0.01[[1]]) - 1  # - 1 to ignore region term
foofoo <- array(dim = c(3, 5, 5, nvar, 2))

dimnames(foofoo)[[1]] <- c("QDS", "HDS", "TQDS")
for (i in seq_along(BOTH_contrib_summary2)) {

    dimnames(foofoo)[[2]] <- names(BOTH_contrib_summary2$scale_QDS)
    for (j in seq_along(BOTH_contrib_summary2[[i]])) {

        dimnames(foofoo)[[3]] <- paste0("tc_", 1:5)
        for (k in seq_along(BOTH_contrib_summary2[[i]][[j]])) {

            dimnames(foofoo)[[5]] <- c("contrib", "var_name")
            for (h in 1:nvar) {
                foofoo[i, j, k, h, 1] <-
                    BOTH_contrib_summary2[[i]][[j]][[k]]$rel.inf[[h]]
                foofoo[i, j, k, h, 2] <-
                    rownames(BOTH_contrib_summary2[[i]][[j]][[k]])[[h]]
            }
        }
    }
}

foofoo_tidy_for_a_scale_QDS <- as_tibble(cbind(
    lr = c(
        rep(names(BOTH_contrib_summary2$scale_QDS)[1], 125),
        rep(names(BOTH_contrib_summary2$scale_QDS)[2], 125),
        rep(names(BOTH_contrib_summary2$scale_QDS)[3], 125),
        rep(names(BOTH_contrib_summary2$scale_QDS)[4], 125),
        rep(names(BOTH_contrib_summary2$scale_QDS)[5], 125)
    ),
    rbind(
        as_tibble(cbind(
            sort(rep(names(foofoo[1, 1, , 1, 1]), 25)),
            rbind(
                foofoo[1, 1, 1, ,],
                foofoo[1, 1, 2, ,],
                foofoo[1, 1, 3, ,],
                foofoo[1, 1, 4, ,],
                foofoo[1, 1, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[1, 1, , 1, 1]), 25)),
            rbind(
                foofoo[1, 2, 1, ,],
                foofoo[1, 2, 2, ,],
                foofoo[1, 2, 3, ,],
                foofoo[1, 2, 4, ,],
                foofoo[1, 2, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[1, 1, , 1, 1]), 25)),
            rbind(
                foofoo[1, 3, 1, ,],
                foofoo[1, 3, 2, ,],
                foofoo[1, 3, 3, ,],
                foofoo[1, 3, 4, ,],
                foofoo[1, 3, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[1, 1, , 1, 1]), 25)),
            rbind(
                foofoo[1, 4, 1, ,],
                foofoo[1, 4, 2, ,],
                foofoo[1, 4, 3, ,],
                foofoo[1, 4, 4, ,],
                foofoo[1, 4, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[1, 1, , 1, 1]), 25)),
            rbind(
                foofoo[1, 5, 1, ,],
                foofoo[1, 5, 2, ,],
                foofoo[1, 5, 3, ,],
                foofoo[1, 5, 4, ,],
                foofoo[1, 5, 5, ,]
            )
        ))
    )
))
foofoo_tidy_for_a_scale_HDS <- as_tibble(cbind(
    lr = c(
        rep(names(BOTH_contrib_summary2$scale_QDS)[1], 125),
        rep(names(BOTH_contrib_summary2$scale_QDS)[2], 125),
        rep(names(BOTH_contrib_summary2$scale_QDS)[3], 125),
        rep(names(BOTH_contrib_summary2$scale_QDS)[4], 125),
        rep(names(BOTH_contrib_summary2$scale_QDS)[5], 125)
    ),
    rbind(
        as_tibble(cbind(
            sort(rep(names(foofoo[2, 1, , 1, 1]), 25)),
            rbind(
                foofoo[2, 1, 1, ,],
                foofoo[2, 1, 2, ,],
                foofoo[2, 1, 3, ,],
                foofoo[2, 1, 4, ,],
                foofoo[2, 1, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[2, 1, , 1, 1]), 25)),
            rbind(
                foofoo[2, 2, 1, ,],
                foofoo[2, 2, 2, ,],
                foofoo[2, 2, 3, ,],
                foofoo[2, 2, 4, ,],
                foofoo[2, 2, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[2, 1, , 1, 1]), 25)),
            rbind(
                foofoo[2, 3, 1, ,],
                foofoo[2, 3, 2, ,],
                foofoo[2, 3, 3, ,],
                foofoo[2, 3, 4, ,],
                foofoo[2, 3, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[2, 1, , 1, 1]), 25)),
            rbind(
                foofoo[2, 4, 1, ,],
                foofoo[2, 4, 2, ,],
                foofoo[2, 4, 3, ,],
                foofoo[2, 4, 4, ,],
                foofoo[2, 4, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[2, 1, , 1, 1]), 25)),
            rbind(
                foofoo[2, 5, 1, ,],
                foofoo[2, 5, 2, ,],
                foofoo[2, 5, 3, ,],
                foofoo[2, 5, 4, ,],
                foofoo[2, 5, 5, ,]
            )
        ))
    )
))
foofoo_tidy_for_a_scale_3QDS <- as_tibble(cbind(
    lr = c(
        rep(names(BOTH_contrib_summary2$scale_QDS)[1], 125),
        rep(names(BOTH_contrib_summary2$scale_QDS)[2], 125),
        rep(names(BOTH_contrib_summary2$scale_QDS)[3], 125),
        rep(names(BOTH_contrib_summary2$scale_QDS)[4], 125),
        rep(names(BOTH_contrib_summary2$scale_QDS)[5], 125)
    ),
    rbind(
        as_tibble(cbind(
            sort(rep(names(foofoo[3, 1, , 1, 1]), 25)),
            rbind(
                foofoo[3, 1, 1, ,],
                foofoo[3, 1, 2, ,],
                foofoo[3, 1, 3, ,],
                foofoo[3, 1, 4, ,],
                foofoo[3, 1, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[3, 1, , 1, 1]), 5)),
            rbind(
                foofoo[3, 2, 1, ,],
                foofoo[3, 2, 2, ,],
                foofoo[3, 2, 3, ,],
                foofoo[3, 2, 4, ,],
                foofoo[3, 2, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[3, 1, , 1, 1]), 25)),
            rbind(
                foofoo[3, 3, 1, ,],
                foofoo[3, 3, 2, ,],
                foofoo[3, 3, 3, ,],
                foofoo[3, 3, 4, ,],
                foofoo[3, 3, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[3, 1, , 1, 1]), 25)),
            rbind(
                foofoo[3, 4, 1, ,],
                foofoo[3, 4, 2, ,],
                foofoo[3, 4, 3, ,],
                foofoo[3, 4, 4, ,],
                foofoo[3, 4, 5, ,]
            )
        )),
        as_tibble(cbind(
            sort(rep(names(foofoo[3, 1, , 1, 1]), 25)),
            rbind(
                foofoo[3, 5, 1, ,],
                foofoo[3, 5, 2, ,],
                foofoo[3, 5, 3, ,],
                foofoo[3, 5, 4, ,],
                foofoo[3, 5, 5, ,]
            )
        ))
    )
))
foofoo_tidy <- as_tibble(cbind(
    region = "BOTH",
    rbind(
        cbind(
            scale = "QDS",
            foofoo_tidy_for_a_scale_QDS
        ),
        cbind(
            scale = "HDS",
            foofoo_tidy_for_a_scale_HDS
        ),
        cbind(
            scale = "3QDS",
            foofoo_tidy_for_a_scale_3QDS
        )
    )
))
foofoo_tidy
BOTH_contribs_tidy <- foofoo_tidy %>%
    transmute(
        region = region,
        scale = scale,
        lr = lr,
        tc = Var.2,
        var_name = var_name,
        contrib = contrib
    )
BOTH_contribs_tidy

# Double check that all that mess worked
BOTH_contrib_summary2$scale_QDS$lr_0.01[[1]][1, 2] ==
    BOTH_contribs_tidy %>%
    filter(
        scale == "QDS",
        lr == "lr_0.01",
        tc == "tc_1",
        var_name == "MAP"
    ) %>%
    dplyr::select(contrib)
# TRUE
BOTH_contrib_summary2$scale_HDS$lr_0.001[[3]][1, 2] ==
    BOTH_contribs_tidy %>%
    filter(
        scale == "HDS",
        lr == "lr_0.001",
        tc == "tc_3",
        var_name == "rough_MAP"
    ) %>%
    dplyr::select(contrib)
# TRUE

write.csv(BOTH_contribs_tidy, here::here(
    "Results",
    "BOTH_contribs_tidy.csv"
))

## <Ooh-Wee!!!!>
#GCFR_QDS_contrib_summary_top_5$lr_0.01[[5]]
#SWAFR_QDS_contrib_summary_top_5$lr_0.01[[5]]
## </Ooh-Wee!!!!>


# TODO -------------------------------------------------------------------------

if (FALSE) {

    # FIXME
    as_tibble(GCFR_contrib_summary_top_5) %>%
        gather(scale) %>%
        #mutate(
        #    lr_0.01  = value[[1]][[1]],
        #    lr_0.005 = value[[1]][[2]],
        #    lr_0.001 = value[[1]][[3]],
        #    lr_5e_04 = value[[1]][[4]],
        #    lr_1e_04 = value[[1]][[5]]
        #)
        (function(x) {
            out <- tibble(nrow = nrow(x), ncol = 5)
            for (row in 1:nrow(x)) {
                out[[1]] <- x$value[i][[1]]
                out[[2]] <- x$value[i][[2]]
                out[[3]] <- x$value[i][[3]]
                out[[4]] <- x$value[i][[4]]
                out[[5]] <- x$value[i][[5]]
            }
            return(out)
        })

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

}
