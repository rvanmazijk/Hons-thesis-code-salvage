# Formal "preliminary" analyses on MAP

# Hons thesis
# Ruan van Mazijk

# created:      2017-05-07
# last edited:  2017-05-19


# Setup ---------------------------------------------------------------------------------

rm(list = ls())
source("i_my_funs.R")
source("ii_my_objs.R")


# Importing dfs from disc ---------------------------------------------------------------

reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_MAP_GCFR/")

abs_vs_rough_MAP_GCFR_df <-
  read.csv(
    paste0(reswd_temp, "abs_vs_rough_MAP_GCFR_df.csv")
  ) %>%
  dplyr::select(fact, MAP, rough_agg_MAP)

extracted_rough_MAP_GCFR <-
  read.csv(
    paste0(reswd_temp, "extracted_rough_MAP_GCFR.csv")
  ) %>%
  dplyr::select(lon, lat, pt_ID, fact, rough_agg_MAP)


# `rough_agg_MAP_extracted models` ------------------------------------------------------

# Let's look at the relationship now:
. <- extracted_rough_MAP_GCFR
m1c_linear <- lm(rough_agg_MAP ~ fact, data = .)
summary(m1c_linear)
visreg(m1c_linear, jitter = T)
rm(.)

# 2017-05-10 22:50 --- and now some stuff akin to the BETTER `m1a_qu_log` (above):
. <- extracted_rough_MAP_GCFR
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
    reswd_temp, "models_",
    format(Sys.time(), "%Y-%m-%d"), ".csv"
  )
)

pdf(
  paste0(
    reswd_temp, "m1c_qu_ll1_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
. <- extracted_rough_MAP_GCFR
  visreg(m1c_qu_ll1, jitter = T)
  plot(
    rough_agg_MAP ~ jitter(fact, 2.25),
    ylab = "roughness(MAP(GCFR))",
    xlab = "Aggregation factor",
    main = expression(paste("log(roughness) ~ log(factor) + [log(factor)]"^2)),
    data = .,
    pch = ".",
    col = "grey"
  )
  newdat <- data.frame(fact = seq(1, 20, length.out = 20))
  newdat$fit <- predict(m1c_qu_ll1, newdata = newdat, interval = "confidence")[, 1]
  newdat$lwr <- predict(m1c_qu_ll1, newdata = newdat, interval = "confidence")[, 2]
  newdat$upr <- predict(m1c_qu_ll1, newdata = newdat, interval = "confidence")[, 3]
  with(newdat, lines(x = fact, y = exp(upr), col = "red", lwd = 2))
  with(newdat, lines(x = fact, y = exp(fit), col = "red", lwd = 2))
  with(newdat, lines(x = fact, y = exp(lwr), col = "red", lwd = 2))
  rm(.)
dev.off()


# Quantreg fiddling ---------------------------------------------------------------------
# (some code salvaged from SDM project script `SDM_analysis_final.R` (by Mike))

reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_MAP_GCFR/")
pdf(
  paste0(
    reswd_temp, "Quantreg_fiddling_and_connected_lines_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  )
)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# Using `rq()` on the non-pt-sampled data:
. <- abs_vs_rough_MAP_GCFR_df
  m1q1 <- rq(
    log(rough_agg_MAP + 1) ~ log(fact) + I(log(fact) ^ 2), # A normal model formula
    tau = 0.75,                  # Specifies the n-quantile you want to regress to
    data = .,                    # (i.e. 0.75 = 75% quantile, 0.50 = median, etc.)
    method = "br" # This is the default algorithm (investigate?)
  )
  summary(m1q1)
  visreg(
    m1q1,
    jitter = T,
    xlab = "Aggregation factor",
    ylab = "log[ rough(MAP(GCFR)) + 1 ]",
    main = "Raw pixels' values \n (log(y + 1) b.c. of NAs)"
  )
rm(.)

#. <- 
#  extracted_rough_MAP_GCFR %>%
#  filter(fact %in% 5:15)

# Now let's do that for the pt-sampled data (i.e. `extracted...`):
. <- extracted_rough_MAP_GCFR
  m1q2 <- rq(
    log(rough_agg_MAP) ~ log(fact) + I(log(fact) ^ 2),
    tau = 0.75,
    data = .,
    method = "br"
  )
  summary(m1q2)
  visreg(
    m1q2,
    jitter = T,
    xlab = "Aggregation factor",
    ylab = "log[ rough(MAP(GCFR)) ]",
    main = "1000 sample points' values"
  )
rm(.)

# I want to now plot the connected lines that represent the roughness value of a given
# point across changing fact:
extracted_rough_MAP_GCFR %>% # let's try just with pt no. 1...
  filter(pt_ID == 1) %>%
  plot(
    rough_agg_MAP ~ fact,
    type = "l",
    data = .,
    ylim = c(0, 1000),
    xlab = "Aggregation factor",
    ylab = "rough(MAP(GCFR))",
    main = "Connected lines for each pt"
  )
for (i in 2:1000) { # now let's do that with the other 999 pts (loop 2:1000)
  extracted_rough_MAP_GCFR %>%
    filter(pt_ID == i) %$%
    lines(rough_agg_MAP ~ fact)
}
# Interesting plot!
# 2017-05-09 --- Tony notes that there appear to be two distinct-ish "streams" of pts:
# one with low roughness, and one with greater roughness. He recommends a PCA of this
# (see PCA§ below!) 

par(op)
dev.off()


# Mike's quantreg-plotting function: modified and applied! ------------------------------

reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_MAP_GCFR/")

pdf(
  paste0(
    reswd_temp, 
    #"quantreg_equiv_to_m1c_qu_ll1_with_m1c_qu_ll1_",
    "m1c_qu_ll1_quantreg_combo_plot_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
extracted_rough_MAP_GCFR %$%
  quant_combo_plot(
    x = fact, y = rough_agg_MAP,
    dat = extracted_rough_MAP_GCFR,
    x_lab = "Aggregation factor",
    y_lab = "roughness(MAP(GCFR))",
    main_lab = expression(paste("log(roughness) ~ log(factor) + [log(factor)]"^2)),
    sub_lab = "lm() [black], & 95, 75, 50, 25, & 5% rq() [red]"
  )
dev.off()

if (F) { # Don't need this plot anymore...
  pdf(
    paste0(
      reswd_temp, "quantreg_equiv_to_m1c_qu_ll1_",
      format(Sys.time(), "%Y-%m-%d"), ".pdf"
    ),
    width = 5, height = 5
  )
  extracted_rough_MAP_GCFR %$%
    quant_plot(
      x = fact, y = rough_agg_MAP,
      dat = extracted_rough_MAP_GCFR,
      x_lab = "Aggregation factor",
      y_lab = "roughness(MAP(GCFR))",
      main_lab = expression(paste("log(roughness) ~ log(factor) + [log(factor)]"^2)),
      sub_lab = "95, 75, 50, 25, & 5 % quantile regressions"
    )
  dev.off()
}

# Plotting the `rough_agg` vs `fact` for each pt separately -----------------------------

filtered_dfs_list <- vector("list", length = 1000)
pb <- txtProgressBar(min = 0, max = 1000, style = 3)
for (i in 1:1000) {
  fil <-
    extracted_rough_MAP_GCFR %>%
    filter(pt_ID == i)
  fil %<>% na.exclude()
  filtered_dfs_list[[i]] <- fil
  setTxtProgressBar(pb, i)
}
beep(2)
close(pb)
names(filtered_dfs_list) <- 1:1000

# Identify the pts that, unfortunately, have no data, b.c. of the pixels that get
# cut out by the roughness function and the shifting aggregation process:
has_length_0 <- c()
for (i in 1:1000) {
  if (length(filtered_dfs_list[[i]]$rough_agg_MAP) == 0) {
    has_length_0 %<>% c(i)
  }
}

# Plot the `rough_agg` vs `fact` for (the 1st 100 of) those pts that *have* data,
# in a separate plot for each pt_ID:
has_length_not0 <- as.numeric((names(filtered_dfs_list)))
pdf(
  paste0(
    reswd_temp, "each_sampled_pt's_loglogquad_fit",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 7, height = 7
)
par(mfrow = c(6, 6), mar = c(0, 0, 0, 0))
for (i in 1:100) {
  plot(
    rough_agg_MAP ~ fact,
    data = extracted_rough_MAP_GCFR[
      extracted_rough_MAP_GCFR$pt_ID %in% has_length_not0[i],
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

extracted_rough_MAP_GCFR_tbl <- 
  extracted_rough_MAP_GCFR %>%
  spread(key = fact, value = rough_agg_MAP) %>%
  na.omit()

rough_pca_utp <- prcomp(extracted_rough_MAP_GCFR_tbl[, -c(1:3)])

reswd_temp <- paste0(reswd, "PCA_rough_agg_MAP_GCFR/")
pdf(
  paste0(
    reswd_temp, "biplot_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
biplot(rough_pca_utp)
dev.off()

extracted_rough_MAP_GCFR_tbl <- cbind(
  extracted_rough_MAP_GCFR_tbl,
  utp_pc1 = rough_pca_utp$x[, 1],
  utp_pc2 = rough_pca_utp$x[, 2]
)
summary(extracted_rough_MAP_GCFR_tbl)

SA_vegmap_CCAB <- readOGR(
  dsn = paste0(datwd, "CCAB_current_biome/"),
  layer = "Current_biome"
)
summary(SA_vegmap_CCAB)
SA_vegmap_CCAB_narm <- SA_vegmap_CCAB[1:9, ] # removes this one pesky NA
GCFR_border <- SA_vegmap_CCAB_narm[
  SA_vegmap_CCAB_narm$LA_CURRENT == "Fynbos" |
    SA_vegmap_CCAB_narm$LA_CURRENT == "Succulent Karoo",
  ]

pt_size <- 2
p <- ggplot(
  aes(x = lon, y = lat),
  dat = extracted_rough_MAP_GCFR_tbl
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
  data = extracted_rough_MAP_GCFR_tbl
) +
  scale_color_distiller(
    palette = "Spectral",
    name = "PC1",
    labels = NULL
  )

p_utp_pc2 <- p + geom_point(
  aes(col = utp_pc2),
  size = pt_size,
  data = extracted_rough_MAP_GCFR_tbl
) +
  scale_color_distiller(
    palette = "Spectral",
    name = "PC2",
    labels = NULL
  )

reswd_temp <- paste0(reswd, "PCA_rough_agg_MAP_GCFR/")

p_load(gridExtra)
grid.arrange(p_utp_pc1, p_utp_pc2, ncol = 2)
# OR
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
    reswd_temp, "Screeplot_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
plot(
  rough_pca_utp,
  main = "PCA(roughness(MAP(GCFR)))"
)
dev.off()

pdf(
  paste0(
    reswd_temp, "PC1&2_eigens_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 10, height = 5
)
par(mfrow = c(1, 2))
plot(
  rough_pca_utp$rotation[, 1],
  ylim = c(-0.1, 0.30),
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
  main = "PC2 eigenvectors (= variable loadings)",
  ylab = "Rotation",
  xlab = "Aggregation factor",
  xaxt = "n"
)
axis(1, labels = 1:20, at = 1:20)
abline(h = 0, lty = 2)
fit <- lm(rough_pca_utp$rotation[, 2] ~ c(1:20) + I(c(1:20) ^ 2))
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
    reswd_temp, "biplot_clear_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
plot(rough_pca_utp$x, ylim = c(-350, 350), xlim = c(-1050, 1050))
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
dev.off()


# Autocorrelation -----------------------------------------------------------------------

pdf(
  paste0(
    reswd, "Autocorrelation/",
    "LocalMoran'sI_vs_MAP_GCFR_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 7, height = 7
)
par(mfrow = c(2, 2), mar = c(3, 3, 2, 2))
MAP_GCFR_0.05 %>%
  MoranLocal(.) %>%
  plot(main = "Local Moran's I MAP_GCFR")
plot(rough_agg_MAP_GCFR$`1`, main = "MAP_GCFR")
par(mar = c(4, 4, 1, 1))
plot(
  rough_agg_MAP_GCFR$`1`[] ~ MoranLocal(MAP_GCFR_0.05)[],
  xlab = "Local Moran's I MAP_GCFR",
  ylab = "MAP_GCFR"
)
par(op)
dev.off()


# </> -----------------------------------------------------------------------------------

