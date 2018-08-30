# ...

# Hons thesis
# Ruan van Mazijk

# created:      2017-07-28
# last edited:  2017-07-28


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

do_richness_PCA <- function(x_richness, x_tester, x_border) {
    
    # Aggregate GCFR floral richness raster -----------------------------------
    # (to QDS, HDS, 3/4DS, & DS)
    
    agg_richness_x <- custom_aggregate_loop(x_richness$raster, facts = 2:4)
    agg_richness_x %<>% c(x_richness$raster, .)
    names(agg_richness_x) <- c("QDS", "HDS", "TQDS", "DS")
    
    op <- par(mfrow = c(1, 1), mar = c(5, 4, 4, 1))
    par(mfrow = c(2, 2))
    plot_from_list(agg_richness_x)
    par(op)
    
    
    # Extracted pts ------------------------------------------------------------
    
    set.seed(57701)
    rand_pts_x <- x_tester %>%
        sampleRandom(size = 1000, xy = TRUE, sp = TRUE, na.rm = TRUE)
    extracted_agg_richness_x <- agg_richness_x %>%
        map(raster::extract, rand_pts_x, sp = TRUE) %>% 
        map(as.data.frame) %>%                          
        map(dplyr::select, x, y, layer)            
    for (i in 1:(length(2:4) + 1)) {
        names(extracted_agg_richness_x[[i]]) <-
            c("lon", "lat", "floral_richness")
    }
    extracted_agg_richness_x %<>%
        map(function(x) { x %<>% cbind(pt_ID = rownames(.), .) })
    for (i in 1:4) {
        extracted_agg_richness_x[[i]] %<>%
            cbind(., fact = names(extracted_agg_richness_x)[i])
    }
    extracted_agg_richness_x %<>%
        reshape2::melt(
            id.vars = c("lon", "lat", "pt_ID", "fact"),
            value.name = "floral_richness"
        ) %>%
        dplyr::select(lon, lat, pt_ID, fact, floral_richness)
    
    extracted_agg_richness_x_df_tbl <- extracted_agg_richness_x %>%
        spread(key = fact, value = floral_richness)
    any(c(
        extracted_agg_richness_x_df_tbl %$% {QDS  != HDS } %>% any(),
        extracted_agg_richness_x_df_tbl %$% {QDS  != TQDS} %>% any(),
        extracted_agg_richness_x_df_tbl %$% {QDS  != DS  } %>% any(),
        extracted_agg_richness_x_df_tbl %$% {HDS  != TQDS} %>% any(),
        extracted_agg_richness_x_df_tbl %$% {HDS  != DS  } %>% any(),
        extracted_agg_richness_x_df_tbl %$% {TQDS != DS  } %>% any()
    ))
    # Oh dear... (?) (IS this bad?)
    # FIXME
    
    
    # PCA ----------------------------------------------------------------------
    
    richness_pca <- prcomp(extracted_agg_richness_x_df_tbl[, -c(1:3)])
    par(mfrow = c(1, 1))
    extracted_agg_richness_x_df_tbl %<>% cbind(
        pc1 = richness_pca$x[, 1],
        pc2 = richness_pca$x[, 2]
    )
    
    # Plot biplot
    biplot <- TRUE
    if (biplot) {
        plot(
            richness_pca$x,
            ylim = c(
                -max(abs(richness_pca$x[, 2])), max(abs(richness_pca$x[, 2]))
            ),
            xlim = c(
                -max(abs(richness_pca$x[, 1])), max(abs(richness_pca$x[, 1]))
            )
        )
        abline(h = 0, lty = 2)
        abline(v = 0, lty = 2)
    }
    
    # Plot PC1/2 loadings
    loadings <- TRUE
    if (loadings) {
      par(mfrow = c(1, 2))
      plot_PC_loadings(x = richness_pca, facts = 2:4)
      par(op)
    }
    
    # Plot in geospace
    geospace <- TRUE
    if (geospace) {
      pt_size <- 2
      p <- 
          ggplot(
              aes(x = lon, y = lat),
              dat = extracted_agg_richness_x_df_tbl
          ) +
          geom_polygon(
              aes(x = long, y = lat, group = group),
              colour = "black",
              fill = NA,
              data = x_border
          ) +
          theme_classic() +
          theme(legend.position = "top")
      p_pc1 <- p +
          geom_point(
              aes(col = pc1),
              size = pt_size,
              data = extracted_agg_richness_x_df_tbl
          ) +
          scale_color_distiller(
              palette = "Spectral",
              name = "PC1"
          )
      p_pc2 <- p +
          geom_point(
              aes(col = pc2),
              size = pt_size,
              data = extracted_agg_richness_x_df_tbl
          ) +
          scale_color_distiller(
              palette = "Spectral",
              name = "PC2"
          )
      plot(arrangeGrob(
          p_pc1, p_pc2,
          ncol = 2, widths = c(15, 15), heights = 10
      ))
    }
    
    return(list(
        agg_richness           = agg_richness_x,
        extracted_agg_richness = extracted_agg_richness_x_df_tbl,
        richness_pca           = richness_pca,
        geospace_grob          = arrangeGrob(p_pc1, p_pc2,
                                             ncol = 2,
                                             widths = c(15, 15),
                                             heights = 10)
    ))
    
}


# Individually PCAs ------------------------------------------------------------

GCFR_richness_pca <- do_richness_PCA(
    GCFR_richness,
    GCFR_border,
    x_tester = MAP_GCFR_0.05
)
summary(GCFR_richness_pca)
par(mfrow = c(1, 2))
plot_PC_loadings(x = GCFR_richness_pca$richness_pca, facts = 2:4)
par(op)
plot(GCFR_richness_pca$geospace_grob)

SWAFR_richness_pca <- do_richness_PCA(
    SWAFR_richness, 
    SWAFR_border,
    x_tester = MAP_SWAFR_0.05
)
summary(SWAFR_richness_pca)
par(mfrow = c(1, 2))
plot_PC_loadings(x = SWAFR_richness_pca$richness_pca, facts = 2:4)
par(op)
plot(SWAFR_richness_pca$geospace_grob)

# SWAFR & GCFR ARE REVERSE ITO scale vs richness!
# viz. GCFR:
#   PC2 high = fine scale,
#   And ...


# COmbo-time! ------------------------------------------------------------------

GCFR_richness_pca$extracted_agg_richness
SWAFR_richness_pca$extracted_agg_richness

both_regions_extracted_agg_richness <-
    rbind(cbind(
            GCFR_richness_pca$extracted_agg_richness,
            region_name = rep("GCFR", times = 1000)
        ),
        cbind(
            SWAFR_richness_pca$extracted_agg_richness,
            region_name = rep("SWAFR", times = 1000)
        )
    )
both_regions_richness_pca <-
    prcomp(both_regions_extracted_agg_richness[, -c(1:3, 8:10)])
plot(
    both_regions_richness_pca$x,
    col = both_regions_extracted_agg_richness$region_name,
    ylim = c(
        -max(abs(both_regions_richness_pca$x[, 2])),
         max(abs(both_regions_richness_pca$x[, 2]))
    ),
    xlim = c(
        -max(abs(both_regions_richness_pca$x[, 1])),
         max(abs(both_regions_richness_pca$x[, 1]))
    )
)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
plot_PC_loadings(both_regions_richness_pca, 2:4)
plot_PC_geospace(
    both_regions_extracted_agg_richness[, -c(8:9)],
    region_a = "GCFR",      region_b = "SWAFR",
    border_a = GCFR_border, border_b = SWAFR_border,
    pc1 = both_regions_richness_pca$x[, 1],
    pc2 = both_regions_richness_pca$x[, 2]
)

