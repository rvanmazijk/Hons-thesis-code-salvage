# Exploring the gbm.step saved BRTs
# Hons thesis
# R. van Mazijk

source("G:/Hons-thesis-from-sethsPC/Scripts/01_setup.R")


# GCFR -------------------------------------------------------------------------

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

# Explore quality
GCFR_saved_BRTs_summary <- as_tibble(rbind(
    cbind(scale = "QDS", GCFR_QDS_saved_BRTs$model_summaries),
    cbind(scale = "HDS", GCFR_HDS_saved_BRTs$model_summaries),
    cbind(scale = "3QDS", GCFR_3QDS_saved_BRTs$model_summaries)
))
make_r2_and_nt_plots(GCFR_saved_BRTs_summary)

# Explore variables' contributions
GCFR_contrib_summary <- make_contrib_summary(list(
    scale_QDS = GCFR_QDS_saved_BRTs$saved_BRTs,
    scale_HDS = GCFR_HDS_saved_BRTs$saved_BRTs,
    scale_3QDS = GCFR_3QDS_saved_BRTs$saved_BRTs
))
GCFR_contrib_summary_top_5 <-
    foreach(scale = GCFR_contrib_summary) %do% {
        foreach(lr = scale) %do% {
            foreach(tc = lr) %do% {
                # Print the top-5 most NB vars
                tc[1:5, ]
            }
        }
    }
names(GCFR_contrib_summary_top_5) <- names(GCFR_contrib_summary)
for (i in seq_along(GCFR_contrib_summary_top_5)) {
    names(GCFR_contrib_summary_top_5[[i]]) <- c(
        "lr_0.01",
        "lr_0.005",
        "lr_0.001",
        "lr_5e_04",
        "lr_1e_04"
    )
}
GCFR_contrib_summary_top_5


# SWAFR ------------------------------------------------------------------------

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
# Explore quality
SWAFR_saved_BRTs_summary <- as_tibble(rbind(
    cbind(scale = "QDS", SWAFR_QDS_saved_BRTs$model_summaries),
    cbind(scale = "HDS", SWAFR_HDS_saved_BRTs$model_summaries),
    cbind(scale = "3QDS", SWAFR_3QDS_saved_BRTs$model_summaries)
))
make_r2_and_nt_plots(SWAFR_saved_BRTs_summary)

# Explore variables' contributions
SWAFR_contrib_summary <- make_contrib_summary(list(
    scale_QDS = SWAFR_QDS_saved_BRTs$saved_BRTs,
    scale_HDS = SWAFR_HDS_saved_BRTs$saved_BRTs,
    scale_3QDS = SWAFR_3QDS_saved_BRTs$saved_BRTs
))
SWAFR_contrib_summary_top_5 <-
    foreach(scale = SWAFR_contrib_summary) %do% {
        foreach(lr = scale) %do% {
            foreach(tc = lr) %do% {
                # Print the top-5 most NB vars
                tc[1:5, ]
            }
        }
    }
names(SWAFR_contrib_summary_top_5) <- names(SWAFR_contrib_summary)
for (i in seq_along(SWAFR_contrib_summary_top_5)) {
    names(SWAFR_contrib_summary_top_5[[i]]) <- c(
        "lr_0.01",
        "lr_0.005",
        "lr_0.001",
        "lr_5e_04",
        "lr_1e_04"
    )
}
SWAFR_contrib_summary_top_5


# BOTH -------------------------------------------------------------------------

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

# Explore quality
BOTH_saved_BRTs_summary <- as_tibble(rbind(
    cbind(scale = "QDS", BOTH_QDS_saved_BRTs$model_summaries),
    cbind(scale = "HDS", BOTH_HDS_saved_BRTs$model_summaries),
    cbind(scale = "3QDS", BOTH_3QDS_saved_BRTs$model_summaries)
))
make_r2_and_nt_plots(BOTH_saved_BRTs_summary)

# Explore variables' contributions
BOTH_contrib_summary <- make_contrib_summary(list(
    scale_QDS = BOTH_QDS_saved_BRTs$saved_BRTs,
    scale_HDS = BOTH_HDS_saved_BRTs$saved_BRTs,
    scale_3QDS = BOTH_3QDS_saved_BRTs$saved_BRTs
))
BOTH_contrib_summary_top_5 <-
    foreach(scale = BOTH_contrib_summary) %do% {
        foreach(lr = scale) %do% {
            foreach(tc = lr) %do% {
                # Print the top-5 most NB vars
                tc[1:5, ]
            }
        }
    }
names(BOTH_contrib_summary_top_5) <- names(BOTH_contrib_summary)
for (i in seq_along(BOTH_contrib_summary_top_5)) {
    names(BOTH_contrib_summary_top_5[[i]]) <- c(
        "lr_0.01",
        "lr_0.005",
        "lr_0.001",
        "lr_5e_04",
        "lr_1e_04"
    )
}
BOTH_contrib_summary_top_5

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
