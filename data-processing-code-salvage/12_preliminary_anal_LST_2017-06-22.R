# Preliminary analyses (buffered only; facts 1:15) on mean annual LST (MODIS)

# Hons thesis
# Ruan van Mazijk

# created:      2017-06-22
# last edited:  2017-06-22


# Setup ---------------------------------------------------------------------------------

rm(list = ls())
source("Scripts/i_my_funs.R")
source("Scripts/ii_my_objs.R")
#quartz()

monthly_LST_GCFR_0.05_buffered
MA_LST_GCFR_0.05_buffered


# aggregation() and roughness(aggregation()) calcs --------------------------------------

facts <- 2:15
agg_MA_LST_GCFR_buffered <- custom_aggregate_loop(MA_LST_GCFR_0.05_buffered, facts = facts)
agg_MA_LST_GCFR_buffered %<>% c(MA_LST_GCFR_0.05_buffered, .)
names(agg_MA_LST_GCFR_buffered)[1] <- "1"
agg_MA_LST_GCFR_buffered_df <- agg_MA_LST_GCFR_buffered %>%
  as_df_raster_vals_list(n = c(1, facts)) 
names(agg_MA_LST_GCFR_buffered_df) <- c("mean_annual_LST", "fact")

par(mar = c(1, 1, 1, 1), mfrow = c(3, 3))
plot_raster_list2(r = agg_MA_LST_GCFR_buffered, b = GCFR_border)
par(op)

rough_agg_MA_LST_GCFR_buffered <- agg_MA_LST_GCFR_buffered %>%
  map(terrain, "roughness")
rough_agg_MA_LST_GCFR_buffered_df <- rough_agg_MA_LST_GCFR_buffered %>%
  as_df_raster_vals_list(n = c(1, facts))
names(rough_agg_MA_LST_GCFR_buffered_df) <- c("rough_agg_mean_annual_LST", "fact")

par(mar = c(1, 1, 1, 1), mfrow = c(3, 3))
plot_raster_list2(r = rough_agg_MA_LST_GCFR_buffered, b = GCFR_border)
par(op)


# `rough_agg()` dataframe---write to disc & read-in -----------------------------------------

if (!"abs_vs_rough_MA_LST_GCFR_buffered_df" %in% ls()) {
  
  abs_vs_rough_MA_LST_GCFR_buffered_df <-
    cbind(
      agg_MA_LST_GCFR_buffered_df$fact,
      agg_MA_LST_GCFR_buffered_df$mean_annual_LST,
      rough_agg_MA_LST_GCFR_buffered_df$rough_agg_mean_annual_LST
    ) %>%
    as.data.frame()
  names(abs_vs_rough_MA_LST_GCFR_buffered_df) <-
    c("fact", "mean_annual_LST", "rough_agg_mean_annual_LST")
  summary(abs_vs_rough_MA_LST_GCFR_buffered_df)
  
  reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_mean_annual_LST_GCFR/")
 
  write.csv(
    abs_vs_rough_MA_LST_GCFR_buffered_df,
    paste0(reswd_temp, "abs_vs_rough_MA_LST_GCFR_buffered_df.csv")
  )
  
  abs_vs_rough_MA_LST_GCFR_buffered_df <- 
    read.csv(
      paste0(reswd_temp, "abs_vs_rough_MA_LST_GCFR_buffered_df.csv")
    ) %>%
    dplyr::select(fact, mean_annual_LST, rough_agg_mean_annual_LST)
  
}


# Now for the `extract()`-ed version ----------------------------------------------------

if (!"extracted_rough_MA_LST_GCFR_buffered" %in% ls()) {
  
  set.seed(57701)
  rand_pts_GCFR <- MAP_GCFR_0.05 %>%
    sampleRandom(size = 1000, xy = T, sp = T, na.rm = T)
  
  extracted_rough_MA_LST_GCFR_buffered <- rough_agg_MA_LST_GCFR_buffered %>%
    map(raster::extract, rand_pts_GCFR, sp = T) %>% 
    map(as.data.frame) %>%                          
    map(dplyr::select, x, y, roughness)             
  for (i in 1:15) {
    names(extracted_rough_MA_LST_GCFR_buffered[[i]]) <-
      c("lon", "lat", "rough_agg_mean_annual_LST")
  }
  extracted_rough_MA_LST_GCFR_buffered %<>%
    map(function(x) { x %<>% cbind(pt_ID = rownames(.), .) })
  for (i in 1:15) { # and then loop-in a column for "fact".
    extracted_rough_MA_LST_GCFR_buffered[[i]] %<>%
      cbind(., fact = names(extracted_rough_MA_LST_GCFR_buffered)[i])
  }
  extracted_rough_MA_LST_GCFR_buffered %<>%
    # `melt()` this list into a dataframe, and tidy it up!
    reshape2::melt(
      id.vars = c("lon", "lat", "pt_ID", "fact"),
      value.name = "rough_agg_mean_annual_LST"
    ) %>%
    dplyr::select(lon, lat, pt_ID, fact, rough_agg_mean_annual_LST)
  
  summary(extracted_rough_MA_LST_GCFR_buffered)
  class(extracted_rough_MA_LST_GCFR_buffered$fact) <- "numeric"
  summary(extracted_rough_MA_LST_GCFR_buffered)
  
  reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_mean_annual_LST_GCFR/")
  write.csv(
    extracted_rough_MA_LST_GCFR_buffered,
    paste0(reswd_temp, "extracted_abs_vs_rough_MA_LST_GCFR_buffered_df.csv")
  )
  
  extracted_rough_MA_LST_GCFR_buffered <- 
    read.csv(
      paste0(reswd_temp, "extracted_abs_vs_rough_MA_LST_GCFR_buffered_df.csv")
    ) %>%
    dplyr::select(lon, lat, pt_ID, fact, rough_agg_mean_annual_LST)
  
}


# `rough_agg_MA_LST_extracted models` ---------------------------------------------------

. <- extracted_rough_MA_LST_GCFR_buffered
  m1c_linear <- lm(    rough_agg_mean_annual_LST  ~     fact,                     data = .)
  m1c_logged <- lm(    rough_agg_mean_annual_LST  ~ log(fact),                    data = .)
  m1c_expntl <- lm(log(rough_agg_mean_annual_LST) ~     fact,                     data = .)
  m1c_loglog <- lm(log(rough_agg_mean_annual_LST) ~ log(fact),                    data = .)
  m1c_qudrtc <- lm(    rough_agg_mean_annual_LST  ~     fact  + I(    fact  ^ 2), data = .)
  m1c_qu_exp <- lm(log(rough_agg_mean_annual_LST) ~     fact  + I(    fact  ^ 2), data = .)
  m1c_qu_log <- lm(    rough_agg_mean_annual_LST  ~ log(fact) + I(log(fact) ^ 2), data = .)
  m1c_qu_ll1 <- lm(log(rough_agg_mean_annual_LST) ~ log(fact) + I(log(fact) ^ 2), data = .)
  summary(m1c_linear)
  summary(m1c_logged)
  summary(m1c_expntl)
  summary(m1c_loglog)
  summary(m1c_qudrtc)
  summary(m1c_qu_exp)
  summary(m1c_qu_log)
  summary(m1c_qu_ll1)
  par(mfrow = c(2, 2))
  visreg(m1c_linear, jitter = T)
  visreg(m1c_logged, jitter = T)
  visreg(m1c_expntl, jitter = T)
  visreg(m1c_loglog, jitter = T)
  visreg(m1c_qudrtc, jitter = T)
  visreg(m1c_qu_exp, jitter = T)
  visreg(m1c_qu_log, jitter = T)
  visreg(m1c_qu_ll1, jitter = T)
  par(op)
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

reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_mean_annual_LST_GCFR/")
write.csv(
  aics,
  paste0(
    reswd_temp, "models_buffered_",
    format(Sys.time(), "%Y-%m-%d"), ".csv"
  )
)

pdf(
  paste0(
    reswd_temp, "m1c_qu_ll1_quantreg_combo_plot_buffered_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
extracted_rough_MA_LST_GCFR_buffered %$%
  quant_combo_plot(
    x = fact, y = rough_agg_mean_annual_LST, 
    dat = extracted_rough_MA_LST_GCFR_buffered,
    x_lab = "Aggregation factor",
    y_lab = "roughness(MA_LST(GCFR))",
    main_lab = expression(
      paste("log(roughness) ~ log(factor) + [log(factor)]" ^ 2, " (buffered)")
    ),
    sub_lab = "lm() [black], & 95, 75, 50, 25, & 5% rq() [red]"
  )
dev.off()


# Plotting the `rough_agg` vs `fact` for each pt separately -----------------------------

filtered_dfs_list <- vector("list", length = 1000)
pb <- txtProgressBar(min = 0, max = 1000, style = 3)
for (i in 1:1000) {
  fil <-
    extracted_rough_MA_LST_GCFR_buffered %>%
    filter(pt_ID == i)
  fil %<>% na.exclude()
  filtered_dfs_list[[i]] <- fil
  setTxtProgressBar(pb, i)
}
beep(2)
close(pb)

names(filtered_dfs_list) <- 1:1000
has_length_0 <- c()
for (i in 1:1000) {
  if (length(filtered_dfs_list[[i]]$rough_agg_mean_annual_LST) == 0) {
    has_length_0 %<>% c(i)
  }
}

has_length_0
filtered_dfs_list <- filtered_dfs_list[-has_length_0]

has_length_not0 <- as.numeric((names(filtered_dfs_list)))
reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_mean_annual_LST_GCFR/")
pdf(
  paste0(
    reswd_temp, "each_sampled_pt's_loglogquad_fit_buffered_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 7, height = 7
)
par(mfrow = c(6, 6), mar = c(0, 0, 0, 0))
for (i in 1:100) {
  plot(
    rough_agg_mean_annual_LST ~ fact,
    data = extracted_rough_MA_LST_GCFR_buffered[
      extracted_rough_MA_LST_GCFR_buffered$pt_ID %in% has_length_not0[i],
      ],
    xlim = c(0, 16),
    ylim = c(0, 16),
    xaxt = "n",
    yaxt = "n"
    #,pch = " "
  )
  # And then plot the fit (quadratic, non-log-logged) on each pt_ID's plot:
  filtered_dfs_list[i] %>%
    map(
      function(x) { 
        fit <- lm(log(rough_agg_mean_annual_LST) ~ log(fact) + I(log(fact) ^ 2), data = x)
        newdat <- data.frame(fact = seq(1, 20, length.out = 100))
        newdat$pred <- predict(fit, newdata = newdat)
        with(newdat, lines(x = fact, y = exp(pred)))
        #text(x = min(x$fact) + 1, y = median(x$rough_agg_MAP) - 25, paste0(i))
        text(x = 1, y = 600, paste0(i), cex = 2, pos = 4)
      }
    ) %>%
    map(abline)
}
par(op)
dev.off()


# PCA -----------------------------------------------------------------------------------

extracted_rough_MA_LST_GCFR_buffered_tbl <- 
  extracted_rough_MA_LST_GCFR_buffered %>%
  spread(key = fact, value = rough_agg_mean_annual_LST) %>%
  na.omit()
rough_pca_utp <- prcomp(extracted_rough_MA_LST_GCFR_buffered_tbl[, -c(1:3)])

reswd_temp <- paste0(reswd, "PCA_rough_agg_mean_annual_LST_GCFR/")

pdf(
  paste0(
    reswd_temp, "Screeplot_buffered_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
plot(
  rough_pca_utp,
  main = "PCA(roughness(elev(GCFR)))"
)
dev.off()

pdf(
  paste0(
    reswd_temp, "biplot_buffered_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
biplot(rough_pca_utp)
dev.off()

pdf(
  paste0(
    reswd_temp, "biplot_clear_buffered_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
plot(
  rough_pca_utp$x,
  ylim = c(-15, 15),
  xlim = c(-15, 15)
)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
dev.off()

extracted_rough_MA_LST_GCFR_buffered_tbl %<>% cbind(
  utp_pc1 = rough_pca_utp$x[, 1],
  utp_pc2 = rough_pca_utp$x[, 2]
)

pt_size <- 2
p <- ggplot(
  aes(x = lon, y = lat),
  dat = extracted_rough_MA_LST_GCFR_buffered_tbl
) +
  geom_polygon(
    aes(x = long, y = lat, group = group),
    colour = "black",
    fill = NA,
    data = GCFR_border
  ) +
  theme_classic() +
  theme(legend.position = "top")

p_utp_pc1 <- p + geom_point(
  aes(col = utp_pc1),
  size = pt_size,
  data = extracted_rough_MA_LST_GCFR_buffered_tbl
) +
  scale_color_distiller(
    palette = "Spectral",
    name = "PC1",
    labels = NULL
  )

p_utp_pc2 <- p + geom_point(
  aes(col = utp_pc2),
  size = pt_size,
  data = extracted_rough_MA_LST_GCFR_buffered_tbl
) +
  scale_color_distiller(
    palette = "Spectral",
    name = "PC2",
    labels = NULL
  )

p_utp_pc1
p_utp_pc2

reswd_temp <- paste0(reswd, "PCA_rough_agg_mean_annual_LST_GCFR/")

ggsave(
  paste0(
    reswd_temp, "In_geographic_space_buffered_",
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
    reswd_temp, "PC1&2_eigens_buffered_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 10, height = 5
)
par(mfrow = c(1, 2))
plot(
  rough_pca_utp$rotation[, 1],
  ylim = c(-0.4, 0.4),
  main = "PC1 eigenvectors (= variable loadings)",
  ylab = "Rotation",
  xlab = "Aggregation factor",
  xaxt = "n"
)
axis(1, labels = 1:15, at = 1:15)
abline(h = 0, lty = 2)
fit <- lm(rough_pca_utp$rotation[, 1] ~ c(1:15) + I(c(1:15) ^ 2))
newdat <- data.frame(fact = seq(1, 15, length.out = 15))
newdat$fit <- predict(fit, newdata = newdat, interval = "confidence")[, 1]
newdat$lwr <- predict(fit, newdata = newdat, interval = "confidence")[, 2]
newdat$upr <- predict(fit, newdata = newdat, interval = "confidence")[, 3]
with(newdat, lines(x = fact, y = upr))
with(newdat, lines(x = fact, y = fit))
with(newdat, lines(x = fact, y = lwr))
plot(
  rough_pca_utp$rotation[, 2],
  ylim = c(-0.5, 0.5),
  main = "PC2 eigenvectors (= variable loadings)",
  ylab = "Rotation",
  xlab = "Aggregation factor",
  xaxt = "n"
)
axis(1, labels = 1:15, at = 1:15)
abline(h = 0, lty = 2)
fit <- lm(rough_pca_utp$rotation[, 2] ~ c(1:15) + I(c(1:15) ^ 2))
summary(fit)
newdat <- data.frame(fact = seq(1, 15, length.out = 15))
newdat$fit <- predict(fit, newdata = newdat, interval = "confidence")[, 1]
newdat$lwr <- predict(fit, newdata = newdat, interval = "confidence")[, 2]
newdat$upr <- predict(fit, newdata = newdat, interval = "confidence")[, 3]
with(newdat, lines(x = fact, y = upr))
with(newdat, lines(x = fact, y = fit))
with(newdat, lines(x = fact, y = lwr))
par(op)
dev.off()


# </> -----------------------------------------------------------------------------------

