# Remaking MAP_GCFR, & the rest of the preliminary analyses, with a buffered raster

# Hons thesis
# Ruan van Mazijk

# created:      2017-05-12
# last edited:  2017-05-19


# Preamble ------------------------------------------------------------------------------

# Here, I want make the CHIRPS raster have some pixels around the edge extra, so that
# the `raster::terrain(..., "roughness")` function doesn't chop bits of the layer off at
# the edges!


# Setup ---------------------------------------------------------------------------------

rm(list = ls())
source("i_my_funs.R")
source("ii_my_objs.R")
#quartz()


# Import data from disc & remake objects I need -----------------------------------------

facts <- 2:20 # the factors to which we want to `agg()`
agg_MAP_GCFR <- custom_aggregate_loop(MAP_GCFR_0.05, fact = facts)
agg_MAP_GCFR %<>% c(MAP_GCFR_0.05, .) # include the original raster...
names(agg_MAP_GCFR)[1] <- "1"         # ... and name it, intuitively, as factor = 1


# raster to polygon ---------------------------------------------------------------------

# Let's visualise how big the edge of the aggregated raster actually gets, and use
# that as a gague for how much extra pixel to leave in `MAP_GCFR_0.05` to start w/.

agg_MAP_GCFR_borders <- vector("list", length = 20)
for (i in 1:20) {
  temp <- agg_MAP_GCFR[[i]] > -Inf
  agg_MAP_GCFR_borders[[i]] <- rasterToPolygons(temp, dissolve = T)
}
names(agg_MAP_GCFR_borders) <- 1:20
par(mfrow = c(3, 3), mar = c(0, 0, 0, 0))
plot_from_list(agg_MAP_GCFR_borders)
par(op)
raster::intersect(agg_MAP_GCFR_borders$`1`, agg_MAP_GCFR_borders$`20`) %>%
  plot()


# Comparing `agg_MAP_GCFR_borders$`20`` and `GCFR_border` -------------------------------

SA_vegmap_CCAB <- readOGR(
  dsn = paste0(datwd, "CCAB_current_biome/"),
  layer = "Current_biome"
)
SA_vegmap_CCAB %<>% subset(!is.na(LA_CURRENT)) # removes this one pesky NA
GCFR_border <- SA_vegmap_CCAB %>%
  subset(LA_CURRENT %in% c("Fynbos", "Succulent Karoo"))

# cf this to the border of the lowest res MAP_GCFR aggregation:
par(mar = c(1, 0, 0, 0))
plot(agg_MAP_GCFR_borders$`20`)
plot(GCFR_border, add = T)
par(op)

# Therefore, gonna use `agg_MAP_GCFR_borders$`20`` as my crop-poly! (my cookie-cutter!),
# a.o.t. `GCFR_border` as before


# Creating the "buffered" CHIRPS_annual stack -------------------------------------------

# 2017-05-19 15:50 --- Have already written the result to disc, so needn't run again :)

if (F) {
  year <- as.character(1981:2016) # note, there is no 2017 annual---yet! duh!
  stacked_chirps <- stack()
  if (!file.exists(paste0(reswd, "CHIRPS_annual_GCFR_buffered.grd"))) {
    for (i in 1:36) {
      x <-
        paste0(
          giswd, "CHIRPS_v2.0/global_annual/chirps-v2.0.",
          year[i], ".tif"
        ) %>% 
        raster() %>%
        mask(agg_MAP_GCFR_borders$`20`) %>%
        crop(agg_MAP_GCFR_borders$`20`)
      stacked_chirps %<>% stack(x)
      paste0("chirps-v2.0.", year[i], " is finished.") %>%
        print()
    }
    beep(2)
    writeRaster(
      stacked_chirps,
      filename = paste0(reswd, "CHIRPS_annual_GCFR_buffered.grd"),
      bandorder = "BIL",
      overwrite = T
    )
  }
}

stacked_chirps_from_file <- stack()
for (i in 1:36) {
  x <-
    paste0(reswd, "CHIRPS_annual_GCFR_buffered.grd") %>%
    raster(band = i)
  stacked_chirps_from_file %<>% stack(x)
}

stacked_chirps_from_file
chirps_annual_GCFR_buffered <- stacked_chirps_from_file


# Creating the "buffered" MAP_GCFR raster -----------------------------------------------

# 2017-05-19 15:52 --- Already written to disc, so just import below :)

if (F) {
  
  MAP_GCFR_buffered <-
    chirps_annual_GCFR_buffered %>%
    mean() %T>%
    plot() # = "BIO1" in BioClim
  
  MAP_GCFR_buffered[MAP_GCFR_buffered < -1000] <- NA # removes the "-9999" ocean
  
  # Write to disc (two formats!; both work!)
  writeRaster(
    MAP_GCFR_buffered,
    filename = paste0(reswd, "MAP_GCFR_buffered.tif"),
    overwrite = T
  )
  writeRaster(
    MAP_GCFR_buffered,
    filename = paste0(reswd, "MAP_GCFR_buffered.grd"),
    overwrite = T
  )

}

# Imports MAP_GCFR from disc
MAP_GCFR_buffered <-
   paste0(reswd, "MAP_GCFR_buffered.grd") %>%
   raster() %T>%
   plot()

# Yay!


# Now do `rough_agg()` on it! -----------------------------------------------------------

facts <- 2:20

agg_MAP_GCFR_buffered <- custom_aggregate_loop(MAP_GCFR_buffered, fact = facts)
agg_MAP_GCFR_buffered %<>% c(MAP_GCFR_buffered, .)
names(agg_MAP_GCFR_buffered)[1] <- "1"
agg_MAP_GCFR_buffered_df <- agg_MAP_GCFR_buffered %>%
  as_df_raster_vals_list(n = c(1, facts)) 

rough_agg_MAP_GCFR_buffered <- agg_MAP_GCFR_buffered %>%
  map(terrain, "roughness")
rough_agg_MAP_GCFR_buffered_df <- rough_agg_MAP_GCFR_buffered %>%
  as_df_raster_vals_list(n = c(1, facts)) 

names(agg_MAP_GCFR_buffered_df) <- c("MAP", "fact")
names(rough_agg_MAP_GCFR_buffered_df) <- c("rough_agg_MAP", "fact")

abs_vs_rough_MAP_GCFR_buffered_df <-
  cbind(
    agg_MAP_GCFR_buffered_df$fact,
    agg_MAP_GCFR_buffered_df$MAP,
    rough_agg_MAP_GCFR_buffered_df$rough_agg_MAP
  ) %>%
  as.data.frame()
names(abs_vs_rough_MAP_GCFR_buffered_df) <- c("fact", "MAP", "rough_agg_MAP")
summary(abs_vs_rough_MAP_GCFR_buffered_df)

reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_MAP_GCFR/")
write.csv(
  abs_vs_rough_MAP_GCFR_buffered_df,
  paste0(reswd_temp, "abs_vs_rough_MAP_GCFR_buffered_df.csv")
)

plot(agg_MAP_GCFR_buffered$`1`)
plot(rough_agg_MAP_GCFR_buffered$`1`)

abs_vs_rough_MAP_GCFR_buffered_df <- 
  read.csv(
    paste0(reswd_temp, "abs_vs_rough_MAP_GCFR_buffered_df.csv")
  ) %>%
  dplyr::select(fact, MAP, rough_agg_MAP)


# Now for the `extract()`-ed version ----------------------------------------------------

set.seed(57701)
rand_pts_GCFR <- MAP_GCFR_0.05 %>%
  sampleRandom(size = 1000, xy = T, sp = T, na.rm = T)
points(rand_pts_GCFR)

extracted_rough_MAP_GCFR_buffered <- rough_agg_MAP_GCFR_buffered %>%
  map(raster::extract, rand_pts_GCFR, sp = T) %>% 
  map(as.data.frame) %>%                          
  map(dplyr::select, x, y, roughness)             
for (i in 1:20) {
  names(extracted_rough_MAP_GCFR_buffered[[i]]) <- c("lon", "lat", "rough_agg_MAP")
}
extracted_rough_MAP_GCFR_buffered %<>%
  map(function(x) { x %<>% cbind(pt_ID = rownames(.), .) })
for (i in 1:20) { # and then loop-in a column for "fact".
  extracted_rough_MAP_GCFR_buffered[[i]] %<>%
    cbind(., fact = names(extracted_rough_MAP_GCFR_buffered)[i])
}
extracted_rough_MAP_GCFR_buffered %<>% # `melt()` this list into a dataframe, and tidy it up!
  reshape2::melt(
    id.vars = c("lon", "lat", "pt_ID", "fact"),
    value.name = "rough_agg_MAP"
  ) %>%
  dplyr::select(lon, lat, pt_ID, fact, rough_agg_MAP)

summary(extracted_rough_MAP_GCFR_buffered)
class(extracted_rough_MAP_GCFR_buffered$fact) <- "numeric"
summary(extracted_rough_MAP_GCFR_buffered)

reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_MAP_GCFR/")
write.csv(
  extracted_rough_MAP_GCFR_buffered,
  paste0(reswd_temp, "extracted_rough_MAP_GCFR_buffered_df.csv")
)

extracted_rough_MAP_GCFR_buffered <- 
  read.csv(
    paste0(reswd_temp, "extracted_rough_MAP_GCFR_buffered_df.csv")
  ) %>%
  dplyr::select(lon, lat, pt_ID, fact, rough_agg_MAP)


# `rough_agg_MAP_extracted models` ------------------------------------------------------

. <- extracted_rough_MAP_GCFR_buffered
  m1c_linear <- lm(    rough_agg_MAP  ~     fact,                     data = .)
  m1c_logged <- lm(    rough_agg_MAP  ~ log(fact),                    data = .)
  m1c_expntl <- lm(log(rough_agg_MAP) ~     fact,                     data = .)
  m1c_loglog <- lm(log(rough_agg_MAP) ~ log(fact),                    data = .)
  m1c_qudrtc <- lm(    rough_agg_MAP  ~     fact  + I(    fact  ^ 2), data = .)
  m1c_qu_exp <- lm(log(rough_agg_MAP) ~     fact  + I(    fact  ^ 2), data = .)
  m1c_qu_log <- lm(    rough_agg_MAP  ~ log(fact) + I(log(fact) ^ 2), data = .)
  m1c_qu_ll1 <- lm(log(rough_agg_MAP) ~ log(fact) + I(log(fact) ^ 2), data = .)
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

reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_MAP_GCFR/")
write.csv(
  aics,
  paste0(
    reswd_temp, "models_buffered_",
    format(Sys.time(), "%Y-%m-%d"), ".csv"
  )
)

#pdf(
#  paste0(
#    reswd_temp, "m1c_qu_ll1_buffered_",
#    format(Sys.time(), "%Y-%m-%d"), ".pdf"
#  ),
#  width = 5, height = 5
#)
#. <- extracted_rough_MAP_GCFR_buffered
#  visreg(m1c_qu_ll1, jitter = T)
#  plot(
#    rough_agg_MAP ~ jitter(fact, 2.25),
#    ylab = "roughness(MAP(GCFR))",
#    xlab = "Aggregation factor",
#    main = expression(paste("log(roughness) ~ log(factor) + [log(factor)]"^2)),
#    data = .,
#    pch = ".",
#    col = "grey"
#  )
#  newdat <- data.frame(fact = seq(1, 20, length.out = 20))
#  newdat$fit <- predict(m1c_qu_ll1, newdata = newdat, interval = "confidence")[, 1]
#  newdat$lwr <- predict(m1c_qu_ll1, newdata = newdat, interval = "confidence")[, 2]
#  newdat$upr <- predict(m1c_qu_ll1, newdata = newdat, interval = "confidence")[, 3]
#  with(newdat, lines(x = fact, y = exp(upr), col = "red", lwd = 2))
#  with(newdat, lines(x = fact, y = exp(fit), col = "red", lwd = 2))
#  with(newdat, lines(x = fact, y = exp(lwr), col = "red", lwd = 2))
#rm(.)
#dev.off()

reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_MAP_GCFR/")
pdf(
  paste0(
    reswd_temp, "m1c_qu_ll1_quantreg_combo_plot_buffered_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
extracted_rough_MAP_GCFR_buffered %$%
  quant_combo_plot(
    x = fact, y = rough_agg_MAP, 
    dat = extracted_rough_MAP_GCFR_buffered,
    x_lab = "Aggregation factor",
    y_lab = "roughness(MAP(GCFR))",
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
    extracted_rough_MAP_GCFR_buffered %>%
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
  if (length(filtered_dfs_list[[i]]$rough_agg_MAP) == 0) {
    has_length_0 %<>% c(i)
  }
}

has_length_0
filtered_dfs_list <- filtered_dfs_list[-has_length_0]

has_length_not0 <- as.numeric((names(filtered_dfs_list)))
reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_MAP_GCFR/")
pdf(
  paste0(
    reswd_temp, "each_sampled_pt's_loglogquad_fit_buffered",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 7, height = 7
)
par(mfrow = c(6, 6), mar = c(0, 0, 0, 0))
for (i in 1:100) {
  plot(
    rough_agg_MAP ~ fact,
    data = extracted_rough_MAP_GCFR_buffered[
      extracted_rough_MAP_GCFR_buffered$pt_ID %in% has_length_not0[i],
    ],
    xlim = c(0, 21),
    ylim = c(0, 700),
    xaxt = "n",
    yaxt = "n"
    #,pch = " "
  )
  # And then plot the fit (quadratic, non-log-logged) on each pt_ID's plot:
  filtered_dfs_list[i] %>%
    map(
      function(x) { 
        fit <- lm(log(rough_agg_MAP) ~ log(fact) + I(log(fact) ^ 2), data = x)
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

extracted_rough_MAP_GCFR_buffered_tbl <- 
  extracted_rough_MAP_GCFR_buffered %>%
  spread(key = fact, value = rough_agg_MAP) %>%
  na.omit()
rough_pca_utp_buffered <- prcomp(extracted_rough_MAP_GCFR_buffered_tbl[, -c(1:3)])

reswd_temp <- paste0(reswd, "PCA_rough_agg_MAP_GCFR/")

pdf(
  paste0(
    reswd_temp, "Screeplot_buffered_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
plot(
  rough_pca_utp_buffered,
  main = "PCA(roughness(MAP(GCFR))) (buffered)"
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

extracted_rough_MAP_GCFR_buffered_tbl <- cbind(
  extracted_rough_MAP_GCFR_buffered_tbl,
  utp_pc1 = rough_pca_utp_buffered$x[, 1],
  utp_pc2 = rough_pca_utp_buffered$x[, 2]
)

# Ugly

#plot(GCFR_border)
#extracted_rough_MAP_GCFR_buffered_tbl %$%
#  points(
#    lon, lat,
#    pch = 20,
#    cex = (utp_pc1 / 1000) + (-min(utp_pc1) / 1000) + 0.2
#  )

# /Ugly

pt_size <- 2
p <- ggplot(
  aes(x = lon, y = lat),
  dat = extracted_rough_MAP_GCFR_buffered_tbl
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
  data = extracted_rough_MAP_GCFR_buffered_tbl
) +
  scale_color_distiller(
    palette = "Spectral",
    name = "PC1",
    labels = NULL
  )

p_utp_buffered_pc2 <- p + geom_point(
  aes(col = utp_pc2),
  size = pt_size,
  data = extracted_rough_MAP_GCFR_buffered_tbl
) +
  scale_color_distiller(
    palette = "Spectral",
    name = "PC2",
    labels = NULL
  )

p_utp_buffered_pc1
p_utp_buffered_pc2

reswd_temp <- paste0(reswd, "PCA_rough_agg_MAP_GCFR/")
p_load(gridExtra)
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
    reswd_temp, "PC1&2_eigens_buffered",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 10, height = 5
)
par(mfrow = c(1, 2))
plot(
  rough_pca_utp_buffered$rotation[, 1],
  ylim = c(-0.325, 0.325),
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
  ylim = c(-0.325, 0.325),
  main = "PC2 eigenvectors (= variable loadings) (buffered)",
  ylab = "Rotation",
  xlab = "Aggregation factor",
  xaxt = "n"
)
axis(1, labels = 1:20, at = 1:20)
abline(h = 0, lty = 2)
fit <- lm(rough_pca_utp_buffered$rotation[, 2] ~ c(1:20) + I(c(1:20) ^ 2))
# `+ I(c(1:20) ^ 3)`?? cubic seems to fit REALLY well???
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

pdf(
  paste0(
    reswd_temp, "biplot_clear_buffered_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
plot(
  rough_pca_utp_buffered$x,
  ylim = c(-400, 400),
  xlim = c(-1200, 1200)
)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
dev.off()

#smoothScatter(
#  rough_pca_utp_buffered$x,
#  ylim = c(-400, 400),
#  xlim = c(-1200, 1200)
#)
#abline(h = 0, lty = 2)
#abline(v = 0, lty = 2)

# That concludes redo-ing what I had already seen in the un-buffered MAP_GCFR PCA...
# Nice that the range of points expanded!!!


# [New PCA plots] -----------------------------------------------------------------------

hist(
  rough_pca_utp_buffered$x[, 1],
  xlim = c(-1500, 1500),
  breaks = 20,
  xlab = "PC1"
)
abline(v = 0, lty = 2)

plot(
  stats::density(
    rough_pca_utp_buffered$x[, 1],
    bw = 100
  ),
  xlim = c(-1500, 1500),
  xlab = "PC1"
)
abline(v = 0, lty = 2)

pdf(
  paste0(
    reswd_temp, "PC1_aligned_plots_buffered_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 12, height = 4
)

par(mfrow = c(1, 3))

plot(
  rough_pca_utp_buffered$rotation[, 1],
  ylim = c(-0.325, 0.325),
  main = "PC1 eigenvectors (= variable loadings)",
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

dens_rotation1 <- stats::density(
  rough_pca_utp_buffered$rotation[, 1],
  bw = 0.02
)
plot(
  dens_rotation1$x ~ dens_rotation1$y,
  ylim = c(-0.325, 0.325),
  ylab = "Rotation",
  type = "l"
)
abline(h = 0, lty = 2)

dens_pc1 <- stats::density(
  rough_pca_utp_buffered$x[, 1],
  bw = 100
)
plot(
  dens_pc1$x ~ dens_pc1$y,
  ylim = c(-1500, 1500),
  ylab = "PC1",
  type = "l"
)
abline(h = 0, lty = 2)

#plot(dens_pc1$y * 10000 ~ dens_rotation1$y, type = "l", ylim = c(0, 10), xlim = c(0, 10))
#abline(0, 1, lty = 2)
#plot(dens_rotation1$y / 10000 ~ dens_pc1$y, type = "l", ylim = c(0, 10e-4), xlim = c(0, 10e-04))
#abline(0, 1, lty = 2)

par(op)

dev.off()

pdf(
  paste0(
    reswd_temp, "PC2_aligned_plot_buffered",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 12, height = 4
)

par(mfrow = c(1, 3))

plot(
  rough_pca_utp_buffered$rotation[, 2],
  ylim = c(-0.325, 0.325),
  main = "PC2 eigenvectors (= variable loadings)",
  ylab = "Rotation",
  xlab = "Aggregation factor",
  xaxt = "n"
)
axis(1, labels = 1:20, at = 1:20)
abline(h = 0, lty = 2)
fit <- lm(rough_pca_utp_buffered$rotation[, 2] ~ c(1:20) + I(c(1:20) ^ 2))
newdat <- data.frame(fact = seq(1, 20, length.out = 20))
newdat$fit <- predict(fit, newdata = newdat, interval = "confidence")[, 1]
newdat$lwr <- predict(fit, newdata = newdat, interval = "confidence")[, 2]
newdat$upr <- predict(fit, newdata = newdat, interval = "confidence")[, 3]
with(newdat, lines(x = fact, y = upr))
with(newdat, lines(x = fact, y = fit))
with(newdat, lines(x = fact, y = lwr))

dens_rotation2 <- stats::density(
  rough_pca_utp_buffered$rotation[, 2],
  bw = 0.05
)
plot(
  dens_rotation2$x ~ dens_rotation2$y,
  ylim = c(-0.325, 0.325),
  ylab = "Rotation",
  type = "l"
)
abline(h = 0, lty = 2)

dens_pc2 <- stats::density(
  rough_pca_utp_buffered$x[, 2],
  bw = 10
)
plot(
  dens_pc2$x ~ dens_pc1$y,
  ylim = c(-700, 700),
  ylab = "PC2",
  type = "l"
)
abline(h = 0, lty = 2)

#plot(dens_pc2$y * 500 ~ dens_rotation2$y, type = "l", ylim = c(0, 2.5), xlim = c(0, 2.5))
#abline(0, 1, lty = 2)
#plot(dens_rotation2$y / 500 ~ dens_pc2$y, type = "l", ylim = c(0, 0.005), xlim = c(0, 0.005))
#abline(0, 1, lty = 2)

par(op)

dev.off()


# [More new plots] ----------------------------------------------------------------------
# 2017-05-18 10:46 --- Are these plots nonsense??? OR is there actually something here?

if (F) {

df_dens <- data.frame(
  index = 1:length(dens_pc1$y),
  pc1 = dens_pc1$y,
  pc2 = dens_pc2$y,
  rot1 = dens_rotation1$y, 
  rot2 = dens_rotation2$y
)

ggplot(aes(y = pc1, x = index), data = df_dens) +
  geom_point(aes(col = rot1)) +
  scale_color_distiller(
    palette = "Spectral",
    name = "Rotation",
    labels = NULL
  ) +
  theme_bw() +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  ylab("Density") +
  xlab("PC1")
ggplot(aes(y = pc2, x = index), data = df_dens) +
  geom_point(aes(col = rot2)) +
  scale_color_distiller(
    palette = "Spectral",
    name = "Rotation",
    labels = NULL
  ) +
  theme_bw() +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  ylab("Density") +
  xlab("PC2")

ggplot(aes(y = pc1, x = index), data = rough_pca_utp_buffered) +
  geom_point(aes(col = pc2)) +
  scale_color_distiller(
    palette = "Spectral",
    name = "PC2",
    labels = NULL
  ) +
  theme_bw() +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  ylab("Density") +
  xlab("PC1 (numbers are wrong)")

temp_df <- data.frame(
  pc1 = rough_pca_utp_buffered$x[, 1],
  pc2 = rough_pca_utp_buffered$x[, 2]
)
ggplot(aes(x = pc1, y = pc2), data = temp_df) +
  #geom_point() +
  #geom_density(kernel = "gaussian")
  #geom_dotplot(binwidth = 30)
  geom_density2d()


# [????] --------------------------------------------------------------------------------

dens_rot1_sim <- approx(
  cumsum(dens_rotation1$y)/sum(dens_rotation1$y),
  dens_rotation1$x,
  runif(length(rough_pca_utp_buffered$x[, 1]))
)$y

dens_rot2_sim <- approx(
  cumsum(dens_rotation2$y)/sum(dens_rotation2$y),
  dens_rotation2$x,
  runif(length(rough_pca_utp_buffered$x[, 2]))
)$y

df_dens2 <- data.frame(
  pc1 = rough_pca_utp_buffered$x[, 1],
  pc2 = rough_pca_utp_buffered$x[, 2],
  rot1 = dens_rot1_sim,
  rot2 = dens_rot2_sim
)

ggplot(data = df_dens2) +
  geom_hex(aes(pc1, rot1), bins = 10) +
  theme_bw()
ggplot(data = df_dens2) +
  geom_bin2d(aes(pc1, rot1), binwidth = c(200, 0.05)) +
  theme_bw()
ggplot(data = df_dens2) +
  geom_density2d(aes(pc1, rot1)) +
  theme_bw()

ggplot(data = df_dens2) +
  geom_hex(aes(pc2, rot2), bins = 10) +
  theme_bw()
ggplot(data = df_dens2) +
  geom_bin2d(aes(pc2, rot2), binwidth = c(50, 0.05)) +
  theme_bw()
ggplot(data = df_dens2) +
  geom_density2d(aes(pc2, rot2)) +
  theme_bw()


# </> -----------------------------------------------------------------------------------

}