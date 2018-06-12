# Preliminary analysis function

# Hons thesis
# Ruan van Mazijk

# created:      2017-06-22
# last edited:  2017-06-22


# E.g. ----------------------------------------------------------------------------------

#   do_prelim_anal(
#     x                 = MAP_GCFR_buffered, 
#     border            = GCFR_border,
#     border_buffer     = GCFR_border_buffered,
#     facts             = 2:15,
#     var_name          = "MAP",
#     region_name       = "GCFR", 
#     reswd_temp        = paste0(reswd, "foo/"),
#     x_tester          = MAP_GCFR_0.05,
#     how_many_rand_pts = 1000
#   )


# do_prelim_anal() ----------------------------------------------------------------------

do_prelim_anal <- function(x           = NULL, # <raster>
                           agg_x       = NULL, # <list of rasters>
                                               # provide if want to skip straight to rough_agg_x
                           rough_agg_x = NULL, # leave as null!
                                               # if you supply agg_x & rough_x separately,
                                               # there is risk that they have not been
                                               # randomised identically (differing set.seed()).
                                               # <DEPREC> "<list of rasters>
                                               #           provide if want to skip straight to rough_agg_x_df"
                                               #
                           border,             # <sp.pts.df> <polygon>
                           border_buffer,      # <sp.pts.df> <polygon>
                           facts,              # <num vector> for `agg()`
                                               # (>=2 (strict); <20 (ideal))
                                               # e.g. `2:15`
                           var_name,           # <char> of variable concerned, for labelling cols
                           region_name,        # <char> duh
                           x_tester,           # <raster> from which to sample `rand_pts`
                           how_many_rand_pts,  # <num> duh
                           reswd_temp) {       # <char> dir for outputs

  
  pb <- txtProgressBar(min = 0, max = 6, style = 3)
  

  # Aggregate ---------------------------------------------------------------------------
  
  if (is.null(agg_x) && is.null(rough_agg_x)) {

    message("Aggregating raster...")
    
    agg_x <- custom_aggregate_loop(x, facts = facts)
    agg_x %<>% c(x, .)
    names(agg_x)[1] <- "1"
    
  }

  agg_x_df <- agg_x %>%
    as_df_raster_vals_list(n = c(1, facts)) 
  names(agg_x_df) <- c(var_name, "fact")
  par(mar = c(1, 1, 1, 1), mfrow = c(3, 3))
  
  setTxtProgressBar(pb, 0.5)
  
  dir.create(
    paste0(reswd_temp, "raster_lists_", var_name, "_", region_name, "/"),
    showWarnings = TRUE
  )
  
  pdf(
    paste0(
      reswd_temp, "raster_lists_", var_name, "_", region_name, "/",
      "agg_rasters_", var_name, "_", region_name, "_",
      format(Sys.time(), "%Y-%m-%d"), ".pdf"
    ),
    width = 10, height = 10
  )
  plot_raster_list2(r = agg_x, b = border_buffer)
  par(op)
  dev.off()
  
  setTxtProgressBar(pb, 1)
  message("Aggregated raster to all resolution factors given")
  

  # Roughness ---------------------------------------------------------------------------
  
  if(is.null(rough_agg_x)) {
    
    message("Calculating roughnesses...")
    
    rough_agg_x <- agg_x %>%
      map(terrain, "roughness")
  
  }
  
  rough_agg_x_df <- rough_agg_x %>%
    as_df_raster_vals_list(n = c(1, facts))
  names(rough_agg_x_df) <- c(paste0("rough_agg_", var_name), "fact")
  par(mar = c(1, 1, 1, 1), mfrow = c(3, 3))
  
  setTxtProgressBar(pb, 1.5)
  
  dir.create(
    paste0(reswd_temp, "raster_lists_", var_name, "_", region_name, "/"),
    showWarnings = TRUE
  )
  pdf(
    paste0(
      reswd_temp, "raster_lists_", var_name, "_", region_name, "/",
      "rough_agg_rasters_", var_name, "_", region_name, "_",
      format(Sys.time(), "%Y-%m-%d"), ".pdf"
    ),
    width = 10, height = 10
  )
  plot_raster_list2(r = rough_agg_x, b = border_buffer)
  par(op)
  dev.off()
  
  setTxtProgressBar(pb, 2)
  message("Roughness calculated from aggregated rasters")
  
  
  # Tidy abs rough agg df ---------------------------------------------------------------
  
  abs_vs_rough_x_df <-
    cbind(
      agg_x_df["fact"],
      agg_x_df[var_name],
      rough_agg_x_df[paste0("rough_agg_", var_name)]
    ) %>%
    as.data.frame()
  names(abs_vs_rough_x_df) <-
    c("fact", var_name, paste0("rough_agg_", var_name))
  dir.create(
    paste0(reswd_temp, "dfs_", var_name, "_", region_name, "/"),
    showWarnings = TRUE
  )
  write.csv(abs_vs_rough_x_df, paste0(
    reswd_temp, "dfs_", var_name, "_", region_name, "/",
    "abs_vs_rough_", var_name, "_", region_name, "_df.csv"
  ))
  
  setTxtProgressBar(pb, 3)
  message("Tidy `abs_vs_rough_x_df` written")
  
  # Extracted pts -----------------------------------------------------------------------
  
  message("Extracting at random points...")
  
  set.seed(57701)
  rand_pts_x <- x_tester %>%
    sampleRandom(size = how_many_rand_pts, xy = T, sp = T, na.rm = T)
  
  setTxtProgressBar(pb, 3.5)
  
  extracted_rough_agg_x <- rough_agg_x %>%
    map(raster::extract, rand_pts_x, sp = T) %>% 
    map(as.data.frame) %>%                          
    map(dplyr::select, x, y, roughness)            
  for (i in 1:(length(facts) + 1)) {
    names(extracted_rough_agg_x[[i]]) <-
      c("lon", "lat", paste0("rough_agg_", var_name))
  }
  extracted_rough_agg_x %<>%
    map(function(x) { x %<>% cbind(pt_ID = rownames(.), .) })
  for (i in 1:(length(facts) + 1)) {
    extracted_rough_agg_x[[i]] %<>%
      cbind(., fact = names(extracted_rough_agg_x)[i])
  }
  extracted_rough_agg_x %<>%
    reshape2::melt(
      id.vars = c("lon", "lat", "pt_ID", "fact"),
      value.name = paste0("rough_agg_", var_name)
    ) %>%
    dplyr::select(lon, lat, pt_ID, fact, paste0("rough_agg_", var_name))
  
  class(extracted_rough_agg_x$fact) <- "numeric"
  dir.create(
    paste0(reswd_temp, "dfs_", var_name, "_", region_name, "/"),
    showWarnings = TRUE
  )
  write.csv(extracted_rough_agg_x, paste0(
    reswd_temp, "dfs_", var_name, "_", region_name, "/",
    "extracted_abs_vs_rough_", var_name, "_", region_name, "_df.csv"
  ))
  
  setTxtProgressBar(pb, 4)
  message("Roughness values extracted at random points")
  
  # Models ------------------------------------------------------------------------------
  
  message("Fitting models...")
  
  . <- extracted_rough_agg_x
    m1c_linear <- lm(formula = paste0(    "rough_agg_", var_name,  " ~ fact"),                         data = .)
    m1c_logged <- lm(formula = paste0(    "rough_agg_", var_name,  " ~ log(fact)"),                    data = .)
    m1c_expntl <- lm(formula = paste0("log(rough_agg_", var_name, ") ~ fact"),                         data = .)
    m1c_loglog <- lm(formula = paste0("log(rough_agg_", var_name, ") ~ log(fact)"),                    data = .)
    m1c_qudrtc <- lm(formula = paste0("    rough_agg_", var_name,  " ~ fact + I(fact ^ 2)"),           data = .)
    m1c_qu_exp <- lm(formula = paste0("log(rough_agg_", var_name, ") ~ fact + I(fact ^ 2)"),           data = .)
    m1c_qu_log <- lm(formula = paste0("    rough_agg_", var_name,  " ~ log(fact) + I(log(fact) ^ 2)"), data = .)
    m1c_qu_ll1 <- lm(formula = paste0("log(rough_agg_", var_name, ") ~ log(fact) + I(log(fact) ^ 2)"), data = .)
  rm(.)

  aics <- AIC(
    m1c_linear,
    m1c_logged,
    m1c_expntl,
    m1c_loglog,
    m1c_qudrtc,
    m1c_qu_exp,
    m1c_qu_log,
    m1c_qu_ll1
  )
  delta_aics <- aics$AIC - min(aics$AIC)
  w_i <- 
    exp(-0.5 * delta_aics) /
    sum(exp(-0.5 * delta_aics))
  formulae <-
    list(
      m1c_linear,
      m1c_logged,
      m1c_expntl,
      m1c_loglog,
      m1c_qudrtc,
      m1c_qu_exp,
      m1c_qu_log,
      m1c_qu_ll1
    ) %>%
    map(formula) %>%
    as.character()
  aics %<>% cbind(formulae, ., delta_aics, w_i)
  aics

  dir.create(
    paste0(reswd_temp, "models_", var_name, "_", region_name, "/"),
    showWarnings = TRUE
  )

  # model objects
  models <- c(
    "m1c_linear",
    "m1c_logged",
    "m1c_expntl",
    "m1c_loglog",
    "m1c_qudrtc",
    "m1c_qu_exp",
    "m1c_qu_log",
    "m1c_qu_ll1"
  )
  for (i in 1:length(models)) {
    save(
      list = (models[i]),
      file = paste0(
        reswd_temp, "models_", var_name, "_", region_name, "/",
        models[i], ".RData"
      )
    )
  }

  # aics
  write.csv(aics, paste0(
    reswd_temp, "models_", var_name, "_", region_name, "/",
    "models_buffered_",
    format(Sys.time(), "%Y-%m-%d"), ".csv"
  ))

  setTxtProgressBar(pb, 4.5)
  
  pdf(
    paste0(
      reswd_temp, "models_", var_name, "_", region_name, "/",
      "m1c_qu_ll1_quantreg_combo_plot_buffered_",
      format(Sys.time(), "%Y-%m-%d"), ".pdf"
    ),
    width = 5, height = 5
  )
  extracted_rough_agg_x %$%
    quant_combo_plot(
      x        = fact,
      y        = eval(parse(text = paste0("rough_agg_", var_name))), 
      dat      = extracted_rough_agg_x,
      x_lab    = "Aggregation factor",
      y_lab    = paste0("roughness(", var_name, "(", region_name, "))"),
      main_lab = expression(paste(
        "log(roughness) ~ log(factor) + [log(factor)]" ^ 2, " (buffered)"
      )),
      sub_lab  = "lm() [black], & 95, 75, 50, 25, & 5% rq() [red]"
    )
  dev.off()

  setTxtProgressBar(pb, 5)
  message("Model suite ran and results stored")
  
  
  # Plotting the `rough_agg` vs `fact` for each pt separately ---------------------------
  
  #filtered_dfs_list <- vector("list", length = 1000)
  #pb <- txtProgressBar(min = 0, max = 1000, style = 3)
  #for (i in 1:1000) {
  #  fil <-
  #    extracted_rough_agg_x %>%
  #    filter(pt_ID == i)
  #  fil %<>% na.exclude()
  #  filtered_dfs_list[[i]] <- fil
  #  setTxtProgressBar(pb, i)
  #}
  #beep(2)
  #close(pb)
  #
  #names(filtered_dfs_list) <- 1:1000
  #has_length_0 <- c()
  #for (i in 1:1000) {
  #  if (length(filtered_dfs_list[[i]]$rough_agg_mean_annual_LST) == 0) {
  #    has_length_0 %<>% c(i)
  #  }
  #}
  #
  #has_length_0
  #filtered_dfs_list <- filtered_dfs_list[-has_length_0]
  #
  #has_length_not0 <- as.numeric((names(filtered_dfs_list)))
  #reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_mean_annual_LST_GCFR/")
  #pdf(
  #  paste0(
  #    reswd_temp, "each_sampled_pt's_loglogquad_fit_buffered_",
  #    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  #  ),
  #  width = 7, height = 7
  #)
  #par(mfrow = c(6, 6), mar = c(0, 0, 0, 0))
  #for (i in 1:100) {
  #  plot(
  #    rough_agg_mean_annual_LST ~ fact,
  #    data = extracted_rough_agg_x[
  #      extracted_rough_agg_x$pt_ID %in% has_length_not0[i],
  #      ],
  #    xlim = c(0, 16),
  #    ylim = c(0, 16),
  #    xaxt = "n",
  #    yaxt = "n"
  #    #,pch = " "
  #  )
  #  # And then plot the fit (quadratic, non-log-logged) on each pt_ID's plot:
  #  filtered_dfs_list[i] %>%
  #    map(
  #      function(x) { 
  #        fit <- lm(log(rough_agg_mean_annual_LST) ~ log(fact) + I(log(fact) ^ 2), data = x)
  #        newdat <- data.frame(fact = seq(1, 20, length.out = 100))
  #        newdat$pred <- predict(fit, newdata = newdat)
  #        with(newdat, lines(x = fact, y = exp(pred)))
  #        #text(x = min(x$fact) + 1, y = median(x$rough_agg_MAP) - 25, paste0(i))
  #        text(x = 1, y = 600, paste0(i), cex = 2, pos = 4)
  #      }
  #    ) %>%
  #    map(abline)
  #}
  #par(op)
  #dev.off()
  
  
  # PCA ---------------------------------------------------------------------------------
  
  message("Running PCA...")
  
  my_key <- paste0("fact")
  my_value <- paste0("rough_agg_", var_name)
  extracted_rough_agg_x_tbl <- 
    extracted_rough_agg_x %>%
    spread_(key_col = my_key, value_col = my_value) %>%
    na.omit()
  rough_pca_utp <- prcomp(extracted_rough_agg_x_tbl[, -c(1:3)])
  
  dir.create(
    paste0(reswd_temp, "PCA_", var_name, "_", region_name, "/"),
    showWarnings = TRUE
  )
  save(
    rough_pca_utp,
    file = paste0(
      reswd_temp, "models_", var_name, "_", region_name, "/",
      "rough_pca_utp", ".RData"
    )
  )

  setTxtProgressBar(pb, 5.33)
  
  
  pdf(
    paste0(
      reswd_temp, "PCA_", var_name, "_", region_name, "/",
      "Screeplot_buffered_",
      format(Sys.time(), "%Y-%m-%d"), ".pdf"
    ),
    width = 5, height = 5
  )
  plot(
    rough_pca_utp,
    main = paste0("PCA(roughness(", var_name, "(GCFR)))")
  )
  dev.off()

  pdf(
    paste0(
      reswd_temp, "PCA_", var_name, "_", region_name, "/",
      "biplot_buffered_",
      format(Sys.time(), "%Y-%m-%d"), ".pdf"
    ),
    width = 5, height = 5
  )
  biplot(rough_pca_utp)
  dev.off()

  pdf(
    paste0(
      reswd_temp, "PCA_", var_name, "_", region_name, "/",
      "biplot_clear_buffered_",
      format(Sys.time(), "%Y-%m-%d"), ".pdf"
    ),
    width = 5, height = 5
  )
  plot(
    rough_pca_utp$x,
    ylim = c(-max(abs(rough_pca_utp$x[, 2])), max(abs(rough_pca_utp$x[, 2]))),
    xlim = c(-max(abs(rough_pca_utp$x[, 1])), max(abs(rough_pca_utp$x[, 1])))
  )
  abline(h = 0, lty = 2)
  abline(v = 0, lty = 2)
  dev.off()

  extracted_rough_agg_x_tbl %<>% cbind(
    utp_pc1 = rough_pca_utp$x[, 1],
    utp_pc2 = rough_pca_utp$x[, 2]
  )

  
  setTxtProgressBar(pb, 5.66)
  
  
  pt_size <- 2
  p <- ggplot(
    aes(x = lon, y = lat),
    dat = extracted_rough_agg_x_tbl
  ) +
    geom_polygon(
      aes(x = long, y = lat, group = group),
      colour = "black",
      fill = NA,
      data = border
    ) +
    theme_classic() +
    theme(legend.position = "top")
  
  p_utp_pc1 <- p + geom_point(
    aes(col = utp_pc1),
    size = pt_size,
    data = extracted_rough_agg_x_tbl
  ) +
    scale_color_distiller(
      palette = "Spectral",
      name = "PC1",
      labels = NULL
    )
  
  p_utp_pc2 <- p + geom_point(
    aes(col = utp_pc2),
    size = pt_size,
    data = extracted_rough_agg_x_tbl
  ) +
    scale_color_distiller(
      palette = "Spectral",
      name = "PC2",
      labels = NULL
    )
  
  ggsave(
    paste0(
      reswd_temp, "PCA_", var_name, "_", region_name, "/",
      "In_geographic_space_buffered_",
      format(Sys.time(), "%Y-%m-%d"), ".pdf"
    ),
    arrangeGrob(
      p_utp_pc1, p_utp_pc2,
      ncol = 2, widths = c(15, 15), heights = 10
    ),
    width = 10, height = 5
  )
  
  

  pdf(
    paste0(
      reswd_temp, "PCA_", var_name, "_", region_name, "/",
      "PC1&2_eigens_buffered",
      format(Sys.time(), "%Y-%m-%d"), ".pdf"
    ),
    width = 10, height = 5
  )
  par(mfrow = c(1, 2))
  plot(
    rough_pca_utp$rotation[, 1],
    ylim = c(-max(abs(rough_pca_utp$rotation[, 1])), max(abs(rough_pca_utp$rotation[, 1]))),
    main = "PC1 eigenvectors (= variable loadings)",
    ylab = "Rotation",
    xlab = "Aggregation factor",
    xaxt = "n"
  )
  axis(1, labels = c(1, facts), at = c(1, facts))
  abline(h = 0, lty = 2)
  fit <- lm(rough_pca_utp$rotation[, 1] ~ c(1, facts) + I(c(1, facts) ^ 2))
  newdat <- data.frame(fact = seq(1, (length(facts) + 1), length.out = (length(facts) + 1)))
  newdat$fit <- predict(fit, newdata = newdat, interval = "confidence")[, 1]
  newdat$lwr <- predict(fit, newdata = newdat, interval = "confidence")[, 2]
  newdat$upr <- predict(fit, newdata = newdat, interval = "confidence")[, 3]
  with(newdat, lines(x = fact, y = upr))
  with(newdat, lines(x = fact, y = fit))
  with(newdat, lines(x = fact, y = lwr))
  plot(
    rough_pca_utp$rotation[, 2],
    ylim = c(-max(abs(rough_pca_utp$rotation[, 2])), max(abs(rough_pca_utp$rotation[, 2]))),
    main = "PC2 eigenvectors (= variable loadings)",
    ylab = "Rotation",
    xlab = "Aggregation factor",
    xaxt = "n"
  )
  axis(1, labels = c(1, facts), at = c(1, facts))
  abline(h = 0, lty = 2)
  fit <- lm(rough_pca_utp$rotation[, 2] ~ c(1, facts) + I(c(1, facts) ^ 2))
  summary(fit)
  newdat <- data.frame(fact = seq(1, (length(facts) + 1), length.out = (length(facts) + 1)))
  newdat$fit <- predict(fit, newdata = newdat, interval = "confidence")[, 1]
  newdat$lwr <- predict(fit, newdata = newdat, interval = "confidence")[, 2]
  newdat$upr <- predict(fit, newdata = newdat, interval = "confidence")[, 3]
  with(newdat, lines(x = fact, y = upr))
  with(newdat, lines(x = fact, y = fit))
  with(newdat, lines(x = fact, y = lwr))
  par(op)
  dev.off()
  
  
  setTxtProgressBar(pb, 6)
  message("PCA ran and results stored")
  
  
  # </> ---------------------------------------------------------------------------------
  
  message("DONE")

}


# </> -----------------------------------------------------------------------------------

