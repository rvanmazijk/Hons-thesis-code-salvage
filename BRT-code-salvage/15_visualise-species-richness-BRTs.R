# ...
# Hons thesis
# R. van Mazijk

#brt_contrib_summary <- tibble(
#    region = ...,
#    scale = ...,
#    var_name = ...,
#    var_type = c("climate", "soils"),
#    abs_rough = c("absolute", "roughness"),
#    contrib = ...
#)
#ggplot(brt_contrib_summary) +
#    geom_point(aes(x = var_name, y = contrib, col = scale)) +
#    facet_grid(region + var_type ~ abs_rough, scales = "free") +
#    theme(...)

# Setup ------------------------------------------------------------------------

# Read-in final BRT models

GCFR_QDS_BRT_final <- readRDS(here::here(
    "Results/gbm_step_simpler_outputs_2017-10-19/",
    "GCFR_QDS_BRT_final.rds"
))
GCFR_HDS_BRT_final <- readRDS(here::here(
    "Results/gbm_step_simpler_outputs_2017-10-19/",
    "GCFR_HDS_BRT_final.rds"
))
GCFR_3QDS_BRT_final <- readRDS(here::here(
    "Results/gbm_step_simpler_outputs_2017-10-19/",
    "GCFR_3QDS_BRT_final.rds"
))

SWAFR_QDS_BRT_final <- readRDS(here::here(
    "Results/gbm_step_simpler_outputs_2017-10-19/",
    "SWAFR_QDS_BRT_final.rds"
))
SWAFR_HDS_BRT_final <- readRDS(here::here(
    "Results/gbm_step_simpler_outputs_2017-10-19/",
    "SWAFR_HDS_BRT_final.rds"
))
SWAFR_3QDS_BRT_final <- readRDS(here::here(
    "Results/gbm_step_simpler_outputs_2017-10-19/",
    "SWAFR_3QDS_BRT_final.rds"
))

BOTH_QDS_BRT_final <- readRDS(here::here(
    "Results/gbm_step_simpler_outputs_2017-10-19/",
    "BOTH_QDS_BRT_final.rds"
))
BOTH_HDS_BRT_final <- readRDS(here::here(
    "Results/gbm_step_simpler_outputs_2017-10-19/",
    "BOTH_HDS_BRT_final.rds"
))
BOTH_3QDS_BRT_final <- readRDS(here::here(
    "Results/gbm_step_simpler_outputs_2017-10-19/",
    "BOTH_3QDS_BRT_final.rds"
))
BRT_finals <- list(  # NB: same order as n_top_vars!!!
    SWAFR_QDS = SWAFR_QDS_BRT_final,
    SWAFR_HDS = SWAFR_HDS_BRT_final,
    SWAFR_3QDS = SWAFR_3QDS_BRT_final,
    GCFR_QDS = GCFR_QDS_BRT_final,
    GCFR_HDS = GCFR_HDS_BRT_final,
    GCFR_3QDS = GCFR_3QDS_BRT_final,
    BOTH_QDS = BOTH_QDS_BRT_final,
    BOTH_HDS = BOTH_HDS_BRT_final,
    BOTH_3QDS = BOTH_3QDS_BRT_final
)


# Variable contributions plot --------------------------------------------------

# Get contribs %s
GCFR_contribs <- rbind(
    cbind(
        scale = "QDS",
        GCFR_QDS_BRT_final$contributions
    ),
    cbind(
        scale = "HDS",
        GCFR_HDS_BRT_final$contributions
    ),
    cbind(
        scale = "3QDS",
        GCFR_3QDS_BRT_final$contributions
    )
)
SWAFR_contribs <- rbind(
    cbind(
        scale = "QDS",
        SWAFR_QDS_BRT_final$contributions
    ),
    cbind(
        scale = "HDS",
        SWAFR_HDS_BRT_final$contributions
    ),
    cbind(
        scale = "3QDS",
        SWAFR_3QDS_BRT_final$contributions
    )
)
BOTH_contribs <- rbind(
    cbind(
        scale = "QDS",
        BOTH_QDS_BRT_final$contributions
    ),
    cbind(
        scale = "HDS",
        BOTH_HDS_BRT_final$contributions
    ),
    cbind(
        scale = "3QDS",
        BOTH_3QDS_BRT_final$contributions
    )
)

BRT_contribs_summary <- as_tibble(rbind(
    cbind(
        region = "GCFR",
        GCFR_contribs
    ),
    cbind(
        region = "SWAFR",
        SWAFR_contribs
    ),
    cbind(
        region = "BOTH",
        BOTH_contribs
    )
))

BRT_contribs_summary[, 1:3] %<>% map(as.character)
BRT_contribs_summary %<>%
    mutate(
        model = paste0(region, "_", scale),
        var_name_strict = var %>%
            str_replace("rough_", ""),
        var_type = var_name_strict %>%
            is_in(c(
                "elev", "NDVI",
                "MAP", "PWQ", "PDQ", "PCV",
                "MLST", "TWQ", "TCQ"
            )) %>%
            ifelse("Climate", "Soil"),
        rough_abs = var %>%
            str_detect("rough") %>%
            ifelse("Roughness", "Absolute"),
        panel_key1 = paste(region, var_type, rough_abs),
        panel_key2 = panel_key1 %>%
            as.factor() %>%
            as.numeric() %>%
            LETTERS[.] %>%
            paste0("(", ., ")")
    )


final_r2s <- foreach(model = BRT_finals) %do% {
    model %$% pseudo_r2(
        cv.statistics$deviance.mean,
        self.statistics$mean.null
    )
}
names(final_r2s) <- names(BRT_finals)
final_r2s %<>%
    as_vector() %>%
    tibble(r2 = ., model = names(final_r2s))
write.csv(final_r2s, here::here("Results", "final_r2s.csv"))

BRT_contribs_summary_plot <- ggplot(BRT_contribs_summary) +
    geom_tile(aes(x = var_name_strict, y = scale, fill = rel.inf)) +
    facet_grid(region ~ var_type + rough_abs, scales = "free_x") +
    scale_fill_distiller(
        name = "RI (%)",
        palette = "Spectral",
        lim = c(0, max(BRT_contribs_summary$rel.inf))
    ) +
    labs(
        x = "Variable",
        y = "Scale"
    ) +
    theme(
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = 90, size = 7),
        axis.text.y = element_text(size = 7)
    ) +
    geom_text(aes(label = panel_key2), x = 1, y = 1)
ggsave(
    BRT_contribs_summary_plot,
    filename = here::here(
        "Results",
        "BRT_contribs_summary.tiff"
    )
)

# Done!


# Partial-dependence plots -----------------------------------------------------

BRT_contribs_summary_top_vars <- BRT_contribs_summary %>%
    filter(rel.inf >= 5)
n_top_vars <- BRT_contribs_summary_top_vars %>%
    mutate(model = paste0(region, "_", scale)) %>%
    group_by(model) %>%
    count()
n_top_vars <-
    as.list(n_top_vars$n) %>%
    `names<-`(n_top_vars$model) %>%
    rev()

for (i in seq_along(n_top_vars)) {
    tiff(
        filename = here::here(
            "Results",
            glue("partial_dep_plots_{names(BRT_finals)[i]}.tif")
        ),
        width = 17,
        height = 17,
        units = "cm",
        res = 200
    )
    gbm.plot(
        BRT_finals[[i]],
        smooth = TRUE,
        n.plots = n_top_vars[[i]],
        common.scale = TRUE,
        plot.layout = c(3, 3),
        write.title = FALSE
    )
    dev.off()
}
# Then edit those in MS Word ;)


for (i in seq_along(n_top_vars)) {
    top_var_names <- BRT_finals[[i]]$contributions$var[1:n_top_vars[[i]]]
    top_var_index <- which(BRT_finals[[i]]$var.names %in% top_var_names)
    tiff(
        filename = here::here(
            "Results",
            glue("fit_plots_{names(BRT_finals)[i]}.tif")
        ),
        width = 17,
        height = 17,
        units = "cm",
        res = 200
    )
    gbm.plot.fits(BRT_finals[[i]], v = top_var_index)
    dev.off()
}
# Then edit those in MS Word ;)


# Check for strong interactions ------------------------------------------------

model_ints <- foreach(model = BRT_finals) %do% gbm.interactions(model)
names(model_ints) <- names(BRT_finals)

model_ints_top <- foreach(model = model_ints) %do% {
    model$rank.list %>%
            transmute(
            var1 = var1.names,
            var2 = var2.names,
            strength = int.size
        ) %>%
        arrange(-strength) %>%
        filter(strength >= 1)
}
names(model_ints_top) <- names(BRT_finals)
saveRDS(model_ints_top, here::here("Results", "model_ints_top.rds"))


# Cow-plot panel approach ------------------------------------------------------

#i <- 1
int_plots <- foreach(model = model_ints) %do% {
    #model = model_ints$GCFR_QDS
    int_data <- model$interactions %>%
        as.data.frame() %>%
        cbind(var1 = rownames(model$interactions), .) %>%
        melt() %>%
        `colnames<-`(c("var1", "var2", "int.size")) %>%
        as_tibble() %>%
        arrange(var1, var2) %>%
        mutate(
            var1 = var1 %>%
                str_detect("rough") %>%
                not() %>%
                ifelse(paste("         ", var1), paste(var1)) %>%
                str_replace_all("_", " "),
            var2 = var2 %>%
                str_detect("rough") %>%
                not() %>%
                ifelse(paste("         ", var2), paste(var2)) %>%
                str_replace_all("_", " "),
            int.size = ifelse(int.size == 0, NA, int.size)
        )
    int_plot <- ggplot(int_data) +
        geom_tile(aes(x = var1, y = var2, fill = int.size)) +
        scale_fill_distiller(
            name = "Interaction\nsize",
            palette = "Spectral",
            na.value = "white"
        ) +
        labs(x = "", y = "") +
        theme(
            axis.text.y = element_text(hjust = 0),
            axis.text.x = element_text(angle = 90, hjust = 0)
        )
    return(int_plot)
    #ggsave(
    #    plot = int_plot,
    #    filename = here::here(
    #        "Results",
    #        glue("model_ints_{names(model_ints)[i]}.tiff")
    #    ),
    #    width = 16,
    #    height = 13,
    #    units = "cm",
    #    dpi = 150
    #)
    #i <- i + 1
}
names(int_plots) <- names(BRT_finals)

saveRDS(
    int_plots,
    here::here(
        "Results",
        "int_plots.rds"
    )
)

plot_grid(
    labels = paste0("(", LETTERS[1:9], ") ", str_replace(names(int_plots), "_", " ")),
    label_x = 0, label_y = 0.25,
    plotlist = int_plots,
    rel_widths = c(1, 1, 0.75)
)

# facet approach ---------------------------------------------------------------

int_data <- foreach(model = model_ints) %do% {
    model$interactions %>%
        as.data.frame() %>%
        cbind(var1 = rownames(model$interactions), .) %>%
        melt() %>%
        `colnames<-`(c("var1", "var2", "int.size")) %>%
        as_tibble() %>%
        arrange(var1, var2) %>%
        mutate(
            var1 = var1 %>%
                str_detect("rough") %>%
                not() %>%
                ifelse(paste("         ", var1), paste(var1)) %>%
                str_replace_all("_", " "),
            var2 = var2 %>%
                str_detect("rough") %>%
                not() %>%
                ifelse(paste("         ", var2), paste(var2)) %>%
                str_replace_all("_", " ")#,
            #int.size = ifelse(int.size == 0, NA, int.size)
        )
}
names(int_data) <- names(BRT_finals)
int_data_new <- cbind(model = names(int_data)[1], int_data[[1]])
int_data_new$model %<>% as.character()
for (i in 2:length(int_data)) {
    int_data_i <- cbind(
        model = names(int_data)[i],
        int_data[[i]]
    )
    int_data_i$model %<>% as.character()
    int_data_new <- rbind(int_data_new, int_data_i)
}
int_data_new %<>%
    mutate(
        model = model %>% str_replace_all("_", " "),
        region = model %>% str_split(" ", simplify = TRUE) %>% `[`(, 1),
        scale = model %>% str_split(" ", simplify = TRUE) %>% `[`(, 2)#,
        #model = paste0("(", LETTERS[1:9], ") \\n", model)
    )
int_plot_new <- ggplot(int_data_new) +
    geom_tile(aes(x = var1, y = var2, fill = int.size)) +
    facet_wrap(region ~ scale, scales = "free") +
    scale_fill_distiller(
        name = "Interaction\nsize",
        palette = "Spectral",
        na.value = "white"
    ) +
    labs(x = "", y = "") +
    theme(
        axis.text.y = element_text(hjust = 0, size = 7.5),
        axis.text.x = element_text(angle = 90, hjust = 0, size = 7.5),
        strip.background = element_blank()
    )
ggsave(
    plot = int_plot_new,
    filename = here::here(
        "Results",
        glue("model_ints_all.tiff")
    ),
    width = 25,
    height = 25,
    units = "cm",
    dpi = 150
)
# Done!


# Interesting stuff (later) ----------------------------------------------------
# Ints ≥ size 5
{
    op <- par()
    par(mfrow = c(3, 4), mar = c(1, 1, 1, 1))
    var_names <- BRT_finals$SWAFR_HDS$data$x.order %>% colnames()
    gbm.perspec(
        BRT_finals$SWAFR_HDS,
        x = which(var_names == "rough_PDQ"),
        y = which(var_names == "MAP"),
        z.range = c(4, 8),
        perspective = TRUE,
        main = "SWAFR HDS (strength = 20.79)"
    )
    var_names <- BRT_finals$SWAFR_3QDS$data$x.order %>% colnames()
    gbm.perspec(
        BRT_finals$SWAFR_3QDS,
        x = which(var_names == "rough_PCV"),
        y = which(var_names == "MAP"),
        z.range = c(4, 8),
        perspective = TRUE,
        main = "SWAFR 3QDS (strength = 6.61)"
    )
    var_names <- BRT_finals$BOTH_QDS$data$x.order %>% colnames()
    gbm.perspec(
        BRT_finals$BOTH_QDS,
        x = which(var_names == "CLYPPT"),
        y = which(var_names == "MAP"),
        z.range = c(4, 8),
        perspective = TRUE,
        main = "BOTH QDS (strength = 9.52)"
    )
    var_names <- BRT_finals$BOTH_QDS$data$x.order %>% colnames()
    gbm.perspec(
        BRT_finals$BOTH_QDS,
        x = which(var_names == "CRFVOL"),
        y = which(var_names == "MAP"),
        z.range = c(4, 8),
        perspective = TRUE,
        main = "BOTH QDS (strength = 8.31)"
    )
    var_names <- BRT_finals$GCFR_QDS$data$x.order %>% colnames()
    gbm.perspec(
        BRT_finals$GCFR_QDS,
        x = which(var_names == "CLYPPT"),
        y = which(var_names == "NDVI"),
        z.range = c(4, 8),
        perspective = TRUE,
        main = "GCFR QDS (strength = 13.48)"
    )
    var_names <- BRT_finals$GCFR_QDS$data$x.order %>% colnames()
    gbm.perspec(
        BRT_finals$GCFR_QDS,
        x = which(var_names == "CRFVOL"),
        y = which(var_names == "NDVI"),
        z.range = c(4, 8),
        perspective = TRUE,
        main = "GCFR QDS (strength = 6.33)"
    )
    var_names <- BRT_finals$BOTH_HDS$data$x.order %>% colnames()
    gbm.perspec(
        BRT_finals$BOTH_HDS,
        x = which(var_names == "rough_MAP"),
        y = which(var_names == "MAP"),
        z.range = c(4, 8),
        perspective = TRUE,
        main = "BOTH HDS (strength = 8.91)"
    )
    var_names <- BRT_finals$BOTH_HDS$data$x.order %>% colnames()
    gbm.perspec(
        BRT_finals$BOTH_HDS,
        x = which(var_names == "rough_elev"),
        y = which(var_names == "MAP"),
        z.range = c(4, 8),
        perspective = TRUE,
        main = "BOTH HDS (strength = 7.05)"
    )
    var_names <- BRT_finals$GCFR_HDS$data$x.order %>% colnames()
    gbm.perspec(
        BRT_finals$GCFR_HDS,
        x = which(var_names == "rough_MAP"),
        y = which(var_names == "rough_elev"),
        z.range = c(4, 8),
        perspective = TRUE,
        main = "GCFR HDS (strength = 9.48)"
    )
    var_names <- BRT_finals$GCFR_3QDS$data$x.order %>% colnames()
    gbm.perspec(
        BRT_finals$GCFR_3QDS,
        x = which(var_names == "rough_CRFVOL"),
        y = which(var_names == "rough_elev"),
        z.range = c(4, 8),
        perspective = TRUE,
        main = "GCFR 3QDS (strength = 11.44)"
    )
    var_names <- BRT_finals$BOTH_3QDS$data$x.order %>% colnames()
    gbm.perspec(
        BRT_finals$BOTH_3QDS,
        x = which(var_names == "rough_PCV"),
        y = which(var_names == "MAP"),
        z.range = c(4, 8),
        perspective = TRUE,
        main = "BOTH 3QDS (strength = 21.77)"
    )
    var_names <- BRT_finals$BOTH_3QDS$data$x.order %>% colnames()
    gbm.perspec(
        BRT_finals$BOTH_3QDS,
        x = which(var_names == "CLYPPT"),
        y = which(var_names == "MAP"),
        z.range = c(4, 8),
        perspective = TRUE,
        main = "BOTH 3QDS (strength = 8.65)"
    )
    par(op)
}
