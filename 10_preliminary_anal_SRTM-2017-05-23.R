# Working with STRM (a DEM) --- preliminary analyses (buffered & non-buffered)

# Hons thesis
# Ruan van Mazijk

# created:      2017-05-17
# last edited:  2017-05-23


# Setup ---------------------------------------------------------------------------------

rm(list = ls())
source("Scripts/i_my_funs.R")
source("Scripts/ii_my_objs.R")
#quartz()

SRTM <- raster::getData(
  "alt",
  country = "ZAF",
  path = paste0(giswd, "SRTM/")
)[[1]]
SRTM %<>% projectRaster(crs = std_CRS)
proj4string(SRTM)
plot(SRTM)


# Mask-crop SRTM to the GCFR ------------------------------------------------------------

if (!"SRTM_GCFR_0.05" %in% ls()) {

  SRTM_GCFR <- SRTM %>%
    mask(GCFR_border) %>%
    crop(GCFR_border)
  
  # Bilinear resampling of 0.0083 deg SRTM raster to 0.05 deg:
  SRTM_GCFR_0.05 <- custom_resample(SRTM_GCFR, res = 0.05)
  par(mfrow = c(1, 2))
  plot(SRTM_GCFR)
  plot(SRTM_GCFR_0.05)
  par(op)

  writeRaster(
    SRTM_GCFR_0.05,
    filename = paste0(reswd, "SRTM_GCFR_0.05.tif"),
    overwrite = T
  )
  writeRaster(
    SRTM_GCFR_0.05,
    filename = paste0(reswd, "SRTM_GCFR_0.05.grd"),
    overwrite = T
  )
}


# Aggregation and roughness calcs -------------------------------------------------------

facts <- 2:20 # the factors to which we want to `agg()`
agg_SRTM_GCFR <- custom_aggregate_loop(SRTM_GCFR_0.05, facts = facts)
agg_SRTM_GCFR %<>% c(SRTM_GCFR_0.05, .)
names(agg_SRTM_GCFR)[1] <- "1"
agg_SRTM_GCFR_df <- agg_SRTM_GCFR %>%
  as_df_raster_vals_list(n = c(1, facts)) 
names(agg_SRTM_GCFR_df) <- c("SRTM", "fact")

par(mar = c(1, 1, 1, 1), mfrow = c(3, 3))
plot_raster_list2(r = agg_SRTM_GCFR, b = GCFR_border)
par(op)

rough_agg_SRTM_GCFR <- agg_SRTM_GCFR %>%
  map(terrain, "roughness")
rough_agg_SRTM_GCFR_df <- rough_agg_SRTM_GCFR %>%
  as_df_raster_vals_list(n = c(1, facts))
names(rough_agg_SRTM_GCFR_df) <- c("rough_agg_SRTM", "fact")

par(mar = c(1, 1, 1, 1), mfrow = c(3, 3))
plot_raster_list2(r = rough_agg_SRTM_GCFR, b = GCFR_border)
par(op)

names(agg_SRTM_GCFR_df)
names(rough_agg_SRTM_GCFR_df)


# `rough_agg()` dataframe---write to disc & read-in -----------------------------------------

if (!"abs_vs_rough_SRTM_GCFR_df" %in% ls()) {
  
  abs_vs_rough_SRTM_GCFR_df <-
    cbind(
      agg_SRTM_GCFR_df$fact,
      agg_SRTM_GCFR_df$SRTM,
      rough_agg_SRTM_GCFR_df$rough_agg_SRTM
    ) %>%
    as.data.frame()
  names(abs_vs_rough_SRTM_GCFR_df) <- c("fact", "SRTM", "rough_agg_SRTM")
  summary(abs_vs_rough_SRTM_GCFR_df)
  
  reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_SRTM_GCFR/")
  write.csv(
    abs_vs_rough_SRTM_GCFR_df,
    paste0(reswd_temp, "abs_vs_rough_SRTM_GCFR_df.csv")
  )
  
  abs_vs_rough_SRTM_GCFR_df <- 
    read.csv(
      paste0(reswd_temp, "abs_vs_rough_SRTM_GCFR_df.csv")
    ) %>%
    dplyr::select(fact, SRTM, rough_agg_SRTM)
  
}


# Now for the `extract()`-ed version ----------------------------------------------------

if (!"extracted_rough_SRTM_GCFR" %in% ls()) {

  set.seed(57701)
  rand_pts_GCFR <- MAP_GCFR_0.05 %>%
    sampleRandom(size = 1000, xy = T, sp = T, na.rm = T)
  
  extracted_rough_SRTM_GCFR <- rough_agg_SRTM_GCFR %>%
    map(raster::extract, rand_pts_GCFR, sp = T) %>% 
    map(as.data.frame) %>%                          
    map(dplyr::select, x, y, roughness)             
  for (i in 1:20) {
    names(extracted_rough_SRTM_GCFR[[i]]) <- c("lon", "lat", "rough_agg_SRTM")
  }
  extracted_rough_SRTM_GCFR %<>%
    map(function(x) { x %<>% cbind(pt_ID = rownames(.), .) })
  for (i in 1:20) { # and then loop-in a column for "fact".
    extracted_rough_SRTM_GCFR[[i]] %<>%
      cbind(., fact = names(extracted_rough_SRTM_GCFR)[i])
  }
  extracted_rough_SRTM_GCFR %<>% # `melt()` this list into a dataframe, and tidy it up!
    reshape2::melt(
      id.vars = c("lon", "lat", "pt_ID", "fact"),
      value.name = "rough_agg_SRTM"
    ) %>%
    dplyr::select(lon, lat, pt_ID, fact, rough_agg_SRTM)
  
  summary(extracted_rough_SRTM_GCFR)
  class(extracted_rough_SRTM_GCFR$fact) <- "numeric"
  summary(extracted_rough_SRTM_GCFR)
  
  reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_SRTM_GCFR/")
  write.csv(
    extracted_rough_SRTM_GCFR,
    paste0(reswd_temp, "extracted_abs_vs_rough_agg_SRTM_GCFR.csv")
  )
  
  extracted_rough_SRTM_GCFR <- 
    read.csv(
      paste0(reswd_temp, "extracted_abs_vs_rough_agg_SRTM_GCFR.csv")
    ) %>%
    dplyr::select(lon, lat, pt_ID, fact, rough_agg_SRTM)

}


# `rough_agg_SRTM_extracted models` -----------------------------------------------------

. <- extracted_rough_SRTM_GCFR
  m1c_linear <- lm(    rough_agg_SRTM  ~     fact,                     data = .)
  m1c_logged <- lm(    rough_agg_SRTM  ~ log(fact),                    data = .)
  m1c_expntl <- lm(log(rough_agg_SRTM) ~     fact,                     data = .)
  m1c_loglog <- lm(log(rough_agg_SRTM) ~ log(fact),                    data = .)
  m1c_qudrtc <- lm(    rough_agg_SRTM  ~     fact  + I(    fact  ^ 2), data = .)
  m1c_qu_exp <- lm(log(rough_agg_SRTM) ~     fact  + I(    fact  ^ 2), data = .)
  m1c_qu_log <- lm(    rough_agg_SRTM  ~ log(fact) + I(log(fact) ^ 2), data = .)
  m1c_qu_ll1 <- lm(log(rough_agg_SRTM) ~ log(fact) + I(log(fact) ^ 2), data = .)
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

reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_SRTM_GCFR/")
write.csv(
  aics,
  paste0(
    reswd_temp, "models_",
    format(Sys.time(), "%Y-%m-%d"), ".csv"
  )
)

pdf(
  paste0(
    reswd_temp, "m1c_qu_ll1_quantreg_combo_plot_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
extracted_rough_SRTM_GCFR %$%
  quant_combo_plot(
    x = fact, y = rough_agg_SRTM, 
    dat = extracted_rough_SRTM_GCFR,
    x_lab = "Aggregation factor",
    y_lab = "roughness(elev(GCFR))",
    main_lab = expression(
      paste("log(roughness) ~ log(factor) + [log(factor)]" ^ 2, " (buffered)")
    ),
    sub_lab = "lm() [black], & 95, 75, 50, 25, & 5% rq() [red]"
  )
dev.off()

# 2017-05-24 15:26 --- Very diff to MAP!!!!!!


# Plotting the `rough_agg` vs `fact` for each pt separately -----------------------------

filtered_dfs_list <- vector("list", length = 1000)
pb <- txtProgressBar(min = 0, max = 1000, style = 3)
for (i in 1:1000) {
  fil <-
    extracted_rough_SRTM_GCFR %>%
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
  if (length(filtered_dfs_list[[i]]$rough_agg_SRTM) == 0) {
    has_length_0 %<>% c(i)
  }
}

has_length_0
filtered_dfs_list <- filtered_dfs_list[-has_length_0]

has_length_not0 <- as.numeric((names(filtered_dfs_list)))
reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_SRTM_GCFR/")
pdf(
  paste0(
    reswd_temp, "each_sampled_pt's_loglogquad_fit_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 7, height = 7
)
par(mfrow = c(6, 6), mar = c(0, 0, 0, 0))
for (i in 1:100) {
  plot(
    rough_agg_SRTM ~ fact,
    data = extracted_rough_SRTM_GCFR[
      extracted_rough_SRTM_GCFR$pt_ID %in% has_length_not0[i],
      ],
    xlim = c(0, 21),
    ylim = c(0, 1300),
    xaxt = "n",
    yaxt = "n"
    #,pch = " "
  )
  # And then plot the fit (quadratic, non-log-logged) on each pt_ID's plot:
  filtered_dfs_list[i] %>%
    map(
      function(x) { 
        fit <- lm(log(rough_agg_SRTM) ~ log(fact) + I(log(fact) ^ 2), data = x)
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

extracted_rough_SRTM_GCFR_tbl <- 
  extracted_rough_SRTM_GCFR %>%
  spread(key = fact, value = rough_agg_SRTM) %>%
  na.omit()
rough_pca_utp <- prcomp(extracted_rough_SRTM_GCFR_tbl[, -c(1:3)])

reswd_temp <- paste0(reswd, "PCA_rough_agg_SRTM_GCFR/")

pdf(
  paste0(
    reswd_temp, "Screeplot_",
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
    reswd_temp, "biplot_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
biplot(rough_pca_utp)
dev.off()

pdf(
  paste0(
    reswd_temp, "biplot_clear_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
plot(
  rough_pca_utp$x,
  ylim = c(-1000, 1000),
  xlim = c(-1200, 1200)
)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
dev.off()

extracted_rough_SRTM_GCFR_tbl %<>% cbind(
  utp_pc1 = rough_pca_utp$x[, 1],
  utp_pc2 = rough_pca_utp$x[, 2]
)

pt_size <- 2
p <- ggplot(
  aes(x = lon, y = lat),
  dat = extracted_rough_SRTM_GCFR_tbl
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
  data = extracted_rough_SRTM_GCFR_tbl
) +
  scale_color_distiller(
    palette = "Spectral",
    name = "PC1",
    labels = NULL
  )

p_utp_pc2 <- p + geom_point(
  aes(col = utp_pc2),
  size = pt_size,
  data = extracted_rough_SRTM_GCFR_tbl
) +
  scale_color_distiller(
    palette = "Spectral",
    name = "PC2",
    labels = NULL
  )

p_utp_pc1
p_utp_pc2

reswd_temp <- paste0(reswd, "PCA_rough_agg_SRTM_GCFR/")

ggsave(
  paste0(
    reswd_temp, "In_geographic_space_",
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
    reswd_temp, "PC1&2_eigens",
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
axis(1, labels = 1:20, at = 1:20)
abline(h = 0, lty = 2)
fit <- lm(rough_pca_utp$rotation[, 1] ~ c(1:20) + I(c(1:20) ^ 2))
newdat <- data.frame(fact = seq(1, 20, length.out = 20))
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
axis(1, labels = 1:20, at = 1:20)
abline(h = 0, lty = 2)
fit <- lm(rough_pca_utp$rotation[, 2] ~ c(1:20) + I(c(1:20) ^ 2))
summary(fit)
newdat <- data.frame(fact = seq(1, 20, length.out = 20))
newdat$fit <- predict(fit, newdata = newdat, interval = "confidence")[, 1]
newdat$lwr <- predict(fit, newdata = newdat, interval = "confidence")[, 2]
newdat$upr <- predict(fit, newdata = newdat, interval = "confidence")[, 3]
with(newdat, lines(x = fact, y = upr))
with(newdat, lines(x = fact, y = fit))
with(newdat, lines(x = fact, y = lwr))
par(op)
dev.off()


# Now let's repeat ALL that with the buffer ---------------------------------------------

# Make the buffer

agg_SRTM_GCFR_borders <- vector("list", length = 20)
for (i in 1:20) {
  temp <- agg_SRTM_GCFR[[i]] > -Inf
  agg_SRTM_GCFR_borders[[i]] <- rasterToPolygons(temp, dissolve = T)
}
names(agg_SRTM_GCFR_borders) <- 1:20
par(mfrow = c(3, 3), mar = c(0, 0, 0, 0))
plot_from_list(agg_SRTM_GCFR_borders)
par(op)
raster::intersect(agg_SRTM_GCFR_borders$`1`, agg_SRTM_GCFR_borders$`20`) %>%
  plot()
par(mar = c(1, 0, 0, 0))
plot(agg_SRTM_GCFR_borders$`20`)
plot(GCFR_border, add = T)
par(op)

# Compare that to what we got with MAP_GCFR:

facts <- 2:20 # the factors to which we want to `agg()`
agg_MAP_GCFR <- custom_aggregate_loop(MAP_GCFR_0.05, fact = facts)
agg_MAP_GCFR %<>% c(MAP_GCFR_0.05, .) # include the original raster...
names(agg_MAP_GCFR)[1] <- "1"         # ... and name it, intuitively, as factor = 1
agg_MAP_GCFR_borders <- vector("list", length = 20)
for (i in 1:20) {
  temp <- agg_MAP_GCFR[[i]] > -Inf
  agg_MAP_GCFR_borders[[i]] <- rasterToPolygons(temp, dissolve = T)
}
names(agg_MAP_GCFR_borders) <- 1:20

par(mfrow = c(1, 2), mar = c(1, 0, 0, 0))
plot(agg_SRTM_GCFR_borders$`20`)
plot(GCFR_border, add = T)
plot(agg_MAP_GCFR_borders$`20`)
plot(GCFR_border, add = T)
par(op)

# note: the buffer for SRTM is every so slightly bigger
# (one extra pixel on the lower right) --> this may be a problem
# For now I'm gonna just stick to the buffer that I made with MAP for the GCFR...
# TODO --- GCFR_buffer from MAP != one from STRM, but good enough!

# Mask-crop SRTM to the buffered-GCFR ---------------------------------------------------

if (!"SRTM_GCFR_0.05_buffered" %in% ls()) {

  SRTM_GCFR_buffered <- SRTM %>%
    mask(agg_MAP_GCFR_borders$`20`) %>%
    crop(agg_MAP_GCFR_borders$`20`)
  
  # Bilinear resampling of 0.0083 deg SRTM raster to 0.05 deg:
  SRTM_GCFR_0.05_buffered <- custom_resample(SRTM_GCFR_buffered, res = 0.05)
  par(mfrow = c(1, 2))
  plot(SRTM_GCFR_buffered)
  plot(SRTM_GCFR_0.05_buffered)
  par(op)
  
  writeRaster(
    SRTM_GCFR_0.05_buffered,
    filename = paste0(reswd, "SRTM_GCFR_0.05_buffered.tif"),
    overwrite = T
  )
  writeRaster(
    SRTM_GCFR_0.05_buffered,
    filename = paste0(reswd, "SRTM_GCFR_0.05_buffered.grd"),
    overwrite = T
  )

}

# Aggregation and roughness calcs -------------------------------------------------------

facts <- 2:20 # the factors to which we want to `agg()`
agg_SRTM_GCFR_buffered <- custom_aggregate_loop(SRTM_GCFR_0.05_buffered, facts = facts)
agg_SRTM_GCFR_buffered %<>% c(SRTM_GCFR_0.05_buffered, .)
names(agg_SRTM_GCFR_buffered)[1] <- "1"
agg_SRTM_GCFR_buffered_df <- agg_SRTM_GCFR_buffered %>%
  as_df_raster_vals_list(n = c(1, facts)) 
names(agg_SRTM_GCFR_buffered_df) <- c("SRTM", "fact")

par(mar = c(1, 1, 1, 1), mfrow = c(3, 3))
plot_raster_list2(r = agg_SRTM_GCFR_buffered, b = GCFR_border)
par(op)

rough_agg_SRTM_GCFR_buffered <- agg_SRTM_GCFR_buffered %>%
  map(terrain, "roughness")
rough_agg_SRTM_GCFR_buffered_df <- rough_agg_SRTM_GCFR_buffered %>%
  as_df_raster_vals_list(n = c(1, facts))
names(rough_agg_SRTM_GCFR_buffered_df) <- c("rough_agg_SRTM", "fact")

reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_SRTM_GCFR/")
pdf(
  paste0(
    reswd_temp, "rough_agg_SRTM_GCFR_buffered_by_fact_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 15, height = 15
)
par(mar = c(1, 1, 1, 1), mfrow = c(3, 3))
plot_raster_list2(r = rough_agg_SRTM_GCFR_buffered, b = GCFR_border)
par(op)
dev.off()


# `rough_agg()` dataframe---write to disc & read-in -----------------------------------------

if (!"abs_vs_rough_SRTM_GCFR_buffered_df" %in% ls()) {
  
  abs_vs_rough_SRTM_GCFR_buffered_df <-
    cbind(
      agg_SRTM_GCFR_buffered_df$fact,
      agg_SRTM_GCFR_buffered_df$SRTM,
      rough_agg_SRTM_GCFR_buffered_df$rough_agg_SRTM
    ) %>%
    as.data.frame()
  names(abs_vs_rough_SRTM_GCFR_buffered_df) <- c("fact", "SRTM", "rough_agg_SRTM")
  summary(abs_vs_rough_SRTM_GCFR_buffered_df)
  
  reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_SRTM_GCFR/")
  write.csv(
    abs_vs_rough_SRTM_GCFR_buffered_df,
    paste0(reswd_temp, "abs_vs_rough_SRTM_GCFR_buffered_df.csv")
  )
  
  abs_vs_rough_SRTM_GCFR_buffered_df <- 
    read.csv(
      paste0(reswd_temp, "abs_vs_rough_SRTM_GCFR_buffered_df.csv")
    ) %>%
    dplyr::select(fact, SRTM, rough_agg_SRTM)

}

# Now for the `extract()`-ed version ----------------------------------------------------

if (!"extracted_rough_SRTM_GCFR_buffered" %in% ls()) {
  
  set.seed(57701)
  rand_pts_GCFR <- MAP_GCFR_0.05 %>%
    sampleRandom(size = 1000, xy = T, sp = T, na.rm = T)
  
  extracted_rough_SRTM_GCFR_buffered <- rough_agg_SRTM_GCFR_buffered %>%
    map(raster::extract, rand_pts_GCFR, sp = T) %>% 
    map(as.data.frame) %>%
    map(dplyr::select, x, y, roughness)             
  for (i in 1:20) {
    names(extracted_rough_SRTM_GCFR_buffered[[i]]) <- c("lon", "lat", "rough_agg_SRTM")
  }
  extracted_rough_SRTM_GCFR_buffered %<>%
    map(function(x) { x %<>% cbind(pt_ID = rownames(.), .) })
  for (i in 1:20) { # and then loop-in a column for "fact".
    extracted_rough_SRTM_GCFR_buffered[[i]] %<>%
      cbind(., fact = names(extracted_rough_SRTM_GCFR_buffered)[i])
  }
  extracted_rough_SRTM_GCFR_buffered %<>%
  # `melt()` this list into a dataframe, and tidy it up!
    reshape2::melt(
      id.vars = c("lon", "lat", "pt_ID", "fact"),
      value.name = "rough_agg_SRTM"
    ) %>%
    dplyr::select(lon, lat, pt_ID, fact, rough_agg_SRTM)
  
  summary(extracted_rough_SRTM_GCFR_buffered)
  class(extracted_rough_SRTM_GCFR_buffered$fact) <- "numeric"
  summary(extracted_rough_SRTM_GCFR_buffered)
  
  reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_SRTM_GCFR/")
  write.csv(
    extracted_rough_SRTM_GCFR_buffered,
    paste0(reswd_temp, "extracted_abs_vs_rough_agg_SRTM_GCFR_buffered.csv")
  )
  
  extracted_rough_SRTM_GCFR_buffered <- 
    read.csv(
      paste0(reswd_temp, "extracted_abs_vs_rough_agg_SRTM_GCFR_buffered.csv")
    ) %>%
    dplyr::select(lon, lat, pt_ID, fact, rough_agg_SRTM)

}

# `rough_agg_SRTM_extracted models` -----------------------------------------------------

. <- extracted_rough_SRTM_GCFR_buffered
  m1c_linear <- lm(    rough_agg_SRTM  ~     fact,                     data = .)
  m1c_logged <- lm(    rough_agg_SRTM  ~ log(fact),                    data = .)
  m1c_expntl <- lm(log(rough_agg_SRTM) ~     fact,                     data = .)
  m1c_loglog <- lm(log(rough_agg_SRTM) ~ log(fact),                    data = .)
  m1c_qudrtc <- lm(    rough_agg_SRTM  ~     fact  + I(    fact  ^ 2), data = .)
  m1c_qu_exp <- lm(log(rough_agg_SRTM) ~     fact  + I(    fact  ^ 2), data = .)
  m1c_qu_log <- lm(    rough_agg_SRTM  ~ log(fact) + I(log(fact) ^ 2), data = .)
  m1c_qu_ll1 <- lm(log(rough_agg_SRTM) ~ log(fact) + I(log(fact) ^ 2), data = .)
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

reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_SRTM_GCFR/")
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
extracted_rough_SRTM_GCFR_buffered %$%
  quant_combo_plot(
    x = fact, y = rough_agg_SRTM, 
    dat = extracted_rough_SRTM_GCFR_buffered,
    x_lab = "Aggregation factor",
    y_lab = "roughness(elev(GCFR))",
    main_lab = expression(
      paste("log(roughness) ~ log(factor) + [log(factor)]" ^ 2, " (buffered)")
    ),
    sub_lab = "lm() [black], & 95, 75, 50, 25, & 5% rq() [red]"
  )
dev.off()

# 2017-05-24 15:26 --- Very diff to MAP!!!!!!


# Plotting the `rough_agg` vs `fact` for each pt separately -----------------------------

filtered_dfs_list <- vector("list", length = 1000)
pb <- txtProgressBar(min = 0, max = 1000, style = 3)
for (i in 1:1000) {
  fil <-
    extracted_rough_SRTM_GCFR_buffered %>%
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
  if (length(filtered_dfs_list[[i]]$rough_agg_SRTM) == 0) {
    has_length_0 %<>% c(i)
  }
}

has_length_0
filtered_dfs_list <- filtered_dfs_list[-has_length_0]

has_length_not0 <- as.numeric((names(filtered_dfs_list)))
reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_SRTM_GCFR/")
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
    rough_agg_SRTM ~ fact,
    data = extracted_rough_SRTM_GCFR_buffered[
      extracted_rough_SRTM_GCFR_buffered$pt_ID %in% has_length_not0[i],
      ],
    xlim = c(0, 21),
    ylim = c(0, 1300),
    xaxt = "n",
    yaxt = "n"
    #,pch = " "
  )
  # And then plot the fit (quadratic, non-log-logged) on each pt_ID's plot:
  filtered_dfs_list[i] %>%
    map(
      function(x) { 
        fit <- lm(log(rough_agg_SRTM) ~ log(fact) + I(log(fact) ^ 2), data = x)
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

extracted_rough_SRTM_GCFR_buffered_tbl <- 
  extracted_rough_SRTM_GCFR_buffered %>%
  spread(key = fact, value = rough_agg_SRTM) %>%
  na.omit()
rough_pca_utp_buffered <- prcomp(extracted_rough_SRTM_GCFR_buffered_tbl[, -c(1:3)])

reswd_temp <- paste0(reswd, "PCA_rough_agg_SRTM_GCFR/")

pdf(
  paste0(
    reswd_temp, "Screeplot_buffered_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
plot(
  rough_pca_utp_buffered,
  main = "PCA(roughness(elev(GCFR))) (buffered)"
)
dev.off()

pdf(
  paste0(
    reswd_temp, "biplot_buffered_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
biplot(rough_pca_utp_buffered)
dev.off()

pdf(
  paste0(
    reswd_temp, "biplot_clear_buffered_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
plot(
  rough_pca_utp_buffered$x,
  ylim = c(-1000, 1000),
  xlim = c(-1200, 1200)
)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
dev.off()

extracted_rough_SRTM_GCFR_buffered_tbl %<>% cbind(
  utp_pc1 = rough_pca_utp_buffered$x[, 1],
  utp_pc2 = rough_pca_utp_buffered$x[, 2]
)

pt_size <- 2
p <- ggplot(
  aes(x = lon, y = lat),
  dat = extracted_rough_SRTM_GCFR_buffered_tbl
) +
  geom_polygon(
    aes(x = long, y = lat, group = group),
    colour = "black",
    fill = NA,
    data = GCFR_border
  ) +
  theme_classic() +
  theme(legend.position = "top")

p_utp_buffered_pc1 <- p + geom_point(
  aes(col = utp_pc1),
  size = pt_size,
  #alpha = 0.5,
  data = extracted_rough_SRTM_GCFR_buffered_tbl
) +
  scale_color_distiller(
    palette = "Spectral",
    name = "PC1",
    labels = NULL
  )

p_utp_buffered_pc2 <- p + geom_point(
  aes(col = utp_pc2),
  size = pt_size,
  #alpha = 0.5,
  data = extracted_rough_SRTM_GCFR_buffered_tbl
) +
  scale_color_distiller(
    palette = "Spectral",
    name = "PC2",
    labels = NULL
  )

p_utp_buffered_pc1
p_utp_buffered_pc2

reswd_temp <- paste0(reswd, "PCA_rough_agg_SRTM_GCFR/")

ggsave(
  paste0(
    reswd_temp, "In_geographic_space_buffered_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  arrangeGrob(
    p_utp_buffered_pc1, p_utp_buffered_pc2,
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
  rough_pca_utp_buffered$rotation[, 1],
  ylim = c(-0.4, 0.4),
  main = "PC1 eigenvectors (= variable loadings) (buffered)",
  ylab = "Rotation",
  xlab = "Aggregation factor",
  xaxt = "n"
)
axis(1, labels = 1:20, at = 1:20)
abline(h = 0, lty = 2)
fit <- lm(rough_pca_utp_buffered$rotation[, 1] ~ c(1:20) + I(c(1:20) ^ 2))
newdat <- data.frame(fact = seq(1, 20, length.out = 20))
newdat$fit <- predict(fit, newdata = newdat, interval = "confidence")[, 1]
newdat$lwr <- predict(fit, newdata = newdat, interval = "confidence")[, 2]
newdat$upr <- predict(fit, newdata = newdat, interval = "confidence")[, 3]
with(newdat, lines(x = fact, y = upr))
with(newdat, lines(x = fact, y = fit))
with(newdat, lines(x = fact, y = lwr))
plot(
  rough_pca_utp_buffered$rotation[, 2],
  #ylim = c(-0.5, 0.5),
  main = "PC2 eigenvectors (= variable loadings) (buffered)",
  ylab = "Rotation",
  xlab = "Aggregation factor",
  xaxt = "n"
)
axis(1, labels = 1:20, at = 1:20)
abline(h = 0, lty = 2)
fit <- lm(rough_pca_utp_buffered$rotation[, 2] ~ c(1:20) + I(c(1:20) ^ 2))
summary(fit)
newdat <- data.frame(fact = seq(1, 20, length.out = 20))
newdat$fit <- predict(fit, newdata = newdat, interval = "confidence")[, 1]
newdat$lwr <- predict(fit, newdata = newdat, interval = "confidence")[, 2]
newdat$upr <- predict(fit, newdata = newdat, interval = "confidence")[, 3]
with(newdat, lines(x = fact, y = upr))
with(newdat, lines(x = fact, y = fit))
with(newdat, lines(x = fact, y = lwr))
par(op)
dev.off()

# 2017-06-22 12:45 TODO --- 

plot(agg_MAP_GCFR_buffered$`1`)
plot(rough_agg_MAP_GCFR_buffered$`1`)

rough_agg_SRTM_GCFR_df <- rough_agg_SRTM_GCFR %>%
  as_df_raster_vals_list(n = c(1, facts)) 

names(agg_MAP_GCFR_buffered_df) <- c("MAP", "fact")
names(rough_agg_SRTM_GCFR_df) <- c("rough_agg_MAP", "fact")

plot(terrain(SRTM_GCFR, "roughness"))
plot(terrain(SRTM_GCFR, "tpi"))
plot(terrain(SRTM_GCFR, "tri"))

# 2017-05-23 18:18 --- 
#SRTM_GCFR_buffered <- dem %>%
#  mask(agg_MAP_GCFR_borders$`20`) %>%
#  crop(agg_MAP_GCFR_borders$`20`)
#SRTM_GCFR <- resample(0.05)
#write srtm_gcfr to disc and into ii_my_objs
#
#`rough_agg()` fn ss
#dataframe ss
#basically fn-ise everything??


# Comparing MAP & elev ------------------------------------------------------------------

as_tibble(extracted_rough_MAP_GCFR_buffered)
as_tibble(extracted_rough_SRTM_GCFR_buffered)

summary(as.factor(extracted_rough_MAP_GCFR_buffered$fact))
summary(as.factor(extracted_rough_SRTM_GCFR_buffered$fact))

abs_vs_rough_MAPSRTM_GCFR_buffered <- full_join(
  extracted_rough_MAP_GCFR_buffered,
  extracted_rough_SRTM_GCFR_buffered,
  by = c("pt_ID", "lon", "lat", "fact")
)

as_tibble(abs_vs_rough_MAPSRTM_GCFR_buffered)
summary(as.factor(abs_vs_rough_MAPSRTM_GCFR_buffered$fact))

ggplot(
  aes(x = log(rough_agg_SRTM), y = log(rough_agg_MAP)),
  dat = abs_vs_rough_MAPSRTM_GCFR_buffered
) +
  geom_point(
    aes(col = fact),
    size = 3,
    alpha = 0.25
  ) +
  theme_classic() +
  theme(legend.position = "top") +
  scale_color_distiller(palette = "Spectral")

log_rough_MAP_vs_elev <- ggplot(
  aes(x = log(rough_agg_SRTM), y = log(rough_agg_MAP)),
  dat = abs_vs_rough_MAPSRTM_GCFR_buffered
) +
  facet_wrap(
    ~ fact,
    labeller = label_bquote(.(fact * 0.05))
  ) +
  geom_point(
    aes(col = fact),
    alpha = 0.05
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_distiller(palette = "Spectral")

ggsave(
  paste0(
    reswd, substitute(log_rough_MAP_vs_elev), "_",
    format(Sys.time(), "%Y-%m-%d"), ".png"
  ),
  log_rough_MAP_vs_elev,
  width = 7, height = 6
)


# Junk ----------------------------------------------------------------------------------

pt_size <- 0.5

ggplot(
  aes(x = fact, y = rough_agg_SRTM),
  dat = abs_vs_rough_MAPSRTM_GCFR_buffered
    %>% filter(rough_agg_MAP < 600)
) +
  geom_jitter(
    aes(col = rough_agg_MAP),
    size = pt_size, width = 0.5, height = 0
  ) +
  theme_classic() +
  theme(legend.position = "top") +
  scale_color_distiller(palette = "Spectral")

ggplot(
  aes(x = fact, y = rough_agg_MAP),
  dat = abs_vs_rough_MAPSRTM_GCFR_buffered
    #%>% filter(rough_agg_MAP < 600)
) +
  geom_jitter(
    aes(col = rough_agg_SRTM),
    size = pt_size, width = 0.5, height = 0
  ) +
  theme_classic() +
  theme(legend.position = "top") +
  scale_color_distiller(palette = "Spectral")

stat_quantile(
  quantiles = c(0.25, 0.5, 0.75),
  formula = y ~ log(x),
  method = "rq"
)

ggplot(
  aes(x = lon, y = lat),
  dat = abs_vs_rough_MAPSRTM_GCFR_buffered
    %>% filter(rough_agg_MAP < 600)
    #%>% filter(fact == 1)
) +
  geom_polygon(
    aes(x = long, y = lat, group = group),
    colour = "black",
    fill = NA,
    data = GCFR_border
  ) +
  geom_point(aes(col = rough_agg_MAP)) +
  #geom_point(aes(col = rough_agg_MAP, size = rough_agg_SRTM)) +
  facet_wrap(~ fact) +
  #scale_size_continuous(range = c(0.01, 0.1)) +
  scale_color_distiller(palette = "Spectral") +
  theme_classic() +
  theme(legend.position = "top")

ggplot(
  aes(x = lon, y = lat),
  dat = abs_vs_rough_MAPSRTM_GCFR_buffered
    #%>% filter(rough_agg_MAP < 600)
    #%>% filter(fact == 1)
) +
  geom_polygon(
    aes(x = long, y = lat, group = group),
    colour = "black",
    fill = NA,
    data = GCFR_border
  ) +
  geom_point(aes(col = rough_agg_SRTM)) +
  #geom_point(aes(col = rough_agg_MAP, size = rough_agg_SRTM)) +
  facet_wrap(~ fact) +
  #scale_size_continuous(range = c(0.01, 0.1)) +
  scale_color_distiller(palette = "Spectral") +
  theme_classic() +
  theme(legend.position = "top")

  
# </> -----------------------------------------------------------------------------------

