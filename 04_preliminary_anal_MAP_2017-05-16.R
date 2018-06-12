# Formal "preliminary" analyses on MAP

# Hons thesis
# Ruan van Mazijk

# created:      2017-05-07
# last edited:  2017-05-16


# Setup ---------------------------------------------------------------------------------

# Here, I load all the setup info and my own defined functions (in `11_my_funs.R`),
# and all the saved objects (mostly rasters) (in `12_my_objs.R`).

rm(list = ls())
source("i_my_funs.R")
source("ii_my_objs.R")
#quartz()

# (Using `MAP_GCFR_0.05` for this)

#MAP_GCFR_0.05 %>%
#  plot_abs_vs_rough() %>%
#  summary()


# Applying `aggregate()` and then roughness calc ----------------------------------------

# 2017-05-08 --- In pseudo-`R`-code:
#   MAP %>% aggregate() -> agg_MAP %>% h(.[i]) -> rough_agg_MAP
# (See Fig. 1a in my notes from my meeting with Tony (2017-05-08), which I had *after*
# I wrote this script)

# 2017-05-09 --- Refactored this § of code today
# 2017-05-16 --- And again today!

facts <- 2:20 # the factors to which we want to `agg()`
agg_MAP_GCFR <- custom_aggregate_loop(MAP_GCFR_0.05, fact = facts)
agg_MAP_GCFR %<>% c(MAP_GCFR_0.05, .) # include the original raster...
names(agg_MAP_GCFR)[1] <- "1"         # ... and name it, intuitively, as factor = 1

agg_MAP_GCFR_df <- agg_MAP_GCFR %>%
  as_df_raster_vals_list(n = c(1, facts)) # makes a data.frame of `agg_MAP_GCFR`

rough_agg_MAP_GCFR <- agg_MAP_GCFR %>% # calc roughness of each raster in agg_MAP_GCFR
  map(terrain, "roughness") # Notice, when `plot()`-ing these rasters, that the edgemost
                            # pixels are lost, as the roughnes function doesn't have the
                            # 8 neighbouring pixels required, and thus cannot compute
                            # a score!

rough_agg_MAP_GCFR_df <- rough_agg_MAP_GCFR %>%
  as_df_raster_vals_list(n = c(1, facts)) # makes a data.frame of `rough_agg_MAP_GCFR`

summary(agg_MAP_GCFR_df)
summary(rough_agg_MAP_GCFR_df)

names(agg_MAP_GCFR_df) <- c("MAP", "fact")             # for ease of use
names(rough_agg_MAP_GCFR_df) <- c("rough_agg_MAP", "fact") # ''

# Make a data.frame out of fact, MAP, and rough_MAP, for analysis purposes:
abs_vs_rough_MAP_GCFR_df <-
  cbind(
    agg_MAP_GCFR_df$fact,
    agg_MAP_GCFR_df$MAP,
    rough_agg_MAP_GCFR_df$rough_agg_MAP
  ) %>%
  as.data.frame()
names(abs_vs_rough_MAP_GCFR_df) <- c("fact", "MAP", "rough_agg_MAP")
summary(abs_vs_rough_MAP_GCFR_df)
dim(abs_vs_rough_MAP_GCFR_df)

reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_MAP_GCFR/")
write.csv(
  abs_vs_rough_MAP_GCFR_df,
  paste0(
    reswd_temp, "abs_vs_rough_MAP_GCFR_df.csv"
    #format(Sys.time(), "%Y-%m-%d"), ".csv"
  )
)

# Junk

#abs_vs_rough_MAP_GCFR_df %>%
#  ggplot(aes(x = fact, y = rough_MAP)) +
#  geom_point(aes(col = MAP), position = "jitter") +
#  theme_classic()
#
#abs_vs_rough_MAP_GCFR_df %>%
#  ggplot(aes(x = fact, y = MAP)) +
#  geom_point(aes(col = rough_MAP)) +
#  theme_classic()

#abs_vs_rough_MAP_GCFR_df %$%
#  scatterplot3d::scatterplot3d(
#    x = fact, y = MAP, z = rough_agg_MAP,
#    angle = 45,
#    grid = T,
#    type = "h",
#    highlight.3d = T
#  )

# /Junk


# Importing that again ------------------------------------------------------------------

reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_MAP_GCFR/")
abs_vs_rough_MAP_GCFR_df <-
  read.csv(
    paste0(
      reswd_temp, "abs_vs_rough_MAP_GCFR_df.csv"
      #format(Sys.time(), "%Y-%m-%d"), ".csv"
    )
  ) %>%
  dplyr::select(fact, MAP, rough_agg_MAP)


# `rough_agg_MAP` models ----------------------------------------------------------------

# Fit various linear models to the data in that data.frame:
# (hopefully the formulae are intuitive!)
# ( # I would use `%>%` but the model assignations don't like this haha)

# 2017-05-11 14:02 --- So, last night night I had a realisation about *why* the curves for
# some of the "rough ~ fact %by% pt" plots are DO NOT look right... stuff like a perfect
# negative parabola of pts, then fit by a *struggling* positive parabola...
# The reason is syntactical. When fitting interactions in `lm()`, if code `X1 * X2`, then
# the function automatically also fits for those terms in isolation too!
# But
# This is *not* the case with fitting polynomials in `lm()`... coding `I(X1 ^ 2)` does NOT
# cause the function to automatically code for X1 in isolation also... meaning that the
# reason those curves were struggling is that I was, accidentally, *forcing* them to
# fit positive parabolas to the data...

#rough_agg_MAP_model <- vector("list", length = 13)
. <- abs_vs_rough_MAP_GCFR_df
  m1a_linear <- lm(    rough_agg_MAP  ~     fact,                     data = .)
  m1a_qudrtc <- lm(    rough_agg_MAP  ~     fact  + I(    fact  ^ 2), data = .)
  m1a_qu_log <- lm(    rough_agg_MAP  ~ log(fact) + I(log(fact) ^ 2), data = .)
  m1a_logged <- lm(    rough_agg_MAP  ~ log(fact),                    data = .)
  m1a_expntl <- lm(log(rough_agg_MAP) ~     fact,                     data = .)
  m1a_loglog <- lm(log(rough_agg_MAP) ~ log(fact),                    data = .)
  m2a_linear <- lm(    rough_agg_MAP  ~     MAP,                      data = .)
  m3a_linear <- lm(    MAP            ~     fact,                     data = .)
  m4a_linear <- lm(    rough_agg_MAP  ~     fact  +     MAP,          data = .)
  m4a_intctn <- lm(    rough_agg_MAP  ~     fact  *     MAP,          data = .)
  m4a_in_exp <- lm(log(rough_agg_MAP) ~     fact  *     MAP,          data = .)
  m4a_in_log <- lm(    rough_agg_MAP  ~ log(fact) *     MAP,          data = .)
  m4a_in_ll1 <- lm(log(rough_agg_MAP) ~ log(fact) *     MAP,          data = .)
  m4a_in_ll2 <- lm(log(rough_agg_MAP) ~ log(fact) * log(MAP),         data = .)
rm(.)

summary(m1a_linear)
summary(m1a_qudrtc)
summary(m1a_qu_log)
summary(m1a_logged)
summary(m1a_expntl)
summary(m1a_loglog)
summary(m2a_linear)
summary(m3a_linear)
summary(m4a_linear)
summary(m4a_intctn)
summary(m4a_in_exp)
summary(m4a_in_log)
summary(m4a_in_ll1)
summary(m4a_in_ll2)

. <- abs_vs_rough_MAP_GCFR_df
  visreg(m1a_linear) # Very NB rel.!
  visreg(m1a_qudrtc)
  visreg(m1a_qu_log)
  visreg(m1a_logged)
  visreg(m1a_expntl)
  visreg(m2a_linear)
  visreg(m3a_linear)
  visreg(m4a_linear, "fact", by = "MAP",  breaks = 2, overlay = F) # Very NB rel.!
  visreg(m4a_intctn, "fact", by = "MAP",  breaks = 2, overlay = F) # Very NB rel.!
  visreg(m4a_in_exp, "fact", by = "MAP",  breaks = 2, overlay = F)
  visreg(m4a_in_log, "fact", by = "MAP",  breaks = 2, overlay = F) # Very NB rel.!
  visreg(m4a_in_log, "MAP",  by = "fact", breaks = 2, overlay = F) # Very NB rel.!
  visreg(m4a_in_ll1, "fact", by = "MAP",  breaks = 2, overlay = F)
  visreg(m4a_in_ll2, "MAP",  by = "fact", breaks = 2, overlay = F)
  par(mfrow = c(2, 2))
    plot(MAP ~ fact, data = .)
    plot(fact ~ MAP, data = .)
    visreg(
      m4a_in_ll2, "fact", by = "MAP",
      breaks = 20, overlay = T,
      points.par = c(pch = ""), legend = F
    )
    visreg(
      m4a_in_ll2, "MAP", by = "fact",
      breaks = 20, overlay = T,
      points.par = c(pch = ""), legend = F
    )
  par(op)
rm(.)

# 2017-05-09 10:34 --- For interest, and possibly useful later?:
# 2017-05-10 22:44 --- (added more)
aics <- AIC(
  m1a_linear, m1a_qudrtc, m1a_qu_log, m1a_logged #,
  #m4a, m4a_i, m4a_i_exp, m4a_i_log, m4a_i_ll, m4a_i_lll
)
delta_aics <- aics$AIC - min(aics$AIC)
w_i <- 
  exp(-0.5 * delta_aics) /
  sum(exp(-0.5 * delta_aics))
aics %<>% cbind(., delta_aics, w_i)
aics
par(mfrow = c(2, 2))
aics %>%
  map(plot)
par(op)
par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))
for (i in 3:4) {
  graphics::barplot(
    height = aics[, i],
    ylab = colnames(aics)[i],
    names.arg = rownames(aics),
    las = 2
  )
}
par(op)


# 2017-05-09 10:37 TODO --- `plot()` model assumption validations!


# `extract()`-ing @ pts from the `rough_agg_MAP` approach (a) ---------------------------
# (random points even!)

# 2017-05-10 --- Refactored this § of code today

summary(abs_vs_rough_MAP_GCFR_df)

plot(MAP_GCFR_0.05) # Plot the raster
# Sample 1000 random points, and save a spatial points data frame
set.seed(57701)
rand_pts_GCFR <- MAP_GCFR_0.05 %>%
  sampleRandom(size = 1000, xy = T, sp = T, na.rm = T)
points(rand_pts_GCFR) # Plot the points on the raster

# Test
MAP_GCFR_0.05_test_samp <- MAP_GCFR_0.05 %>%
  raster::extract(rand_pts_GCFR) # Extract the raster value at the co-ords of the pts
# /Test

# Now to apply it *properly* to `rough_agg...` (complicated):

# <piping
  # I want to make an object called `extracted_rough_MAP_GCFR`
  extracted_rough_MAP_GCFR <- rough_agg_MAP_GCFR %>% # So I take `rough_agg...`,
    map(raster::extract, rand_pts_GCFR, sp = T) %>% # `extract()` the vals, keeping it in
    map(as.data.frame) %>%                          # sp.pts.df's in the `map()`-made list,
    map(dplyr::select, x, y, roughness)             # which I then make e/ of into a df,
                                                    # (and keep only the cols I want).
# />
for (i in 1:20) { # quick loop to rename the cols of e/ df in the list.
  names(extracted_rough_MAP_GCFR[[i]]) <- c("lon", "lat", "rough_agg_MAP")
}
# <piping
  extracted_rough_MAP_GCFR %<>% # `map()` to incl. a "pt_ID" column with `cbind()`.
    map(function(x) { x %<>% cbind(pt_ID = rownames(.), .) })
# />
for (i in 1:20) { # and then loop-in a column for "fact".
  extracted_rough_MAP_GCFR[[i]] %<>%
    cbind(., fact = names(extracted_rough_MAP_GCFR)[i])
}
# <piping
  extracted_rough_MAP_GCFR %<>% # `melt()` this list into a dataframe, and tidy it up!
    reshape2::melt(
      id.vars = c("lon", "lat", "pt_ID", "fact"),
      value.name = "rough_agg_MAP"
    ) %>%
    dplyr::select(lon, lat, pt_ID, fact, rough_agg_MAP)
# />
# Now let's look at I've made:
summary(extracted_rough_MAP_GCFR)
# note symmetric dbn of "fact" vals = equal amnt of e/ = yay!
class(extracted_rough_MAP_GCFR$fact) <- "numeric" # small fix haha
summary(extracted_rough_MAP_GCFR)

reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_MAP_GCFR/")
write.csv(
  extracted_rough_MAP_GCFR,
  paste0(
    reswd_temp, "extracted_rough_MAP_GCFR.csv"
    #format(Sys.time(), "%Y-%m-%d"), ".csv"
  )
)

# Junk

#for (i in 1:20) {
#  extracted_rough_MAP_GCFR[[i]] %<>% cbind(rand_pts_GCFR$x, rand_pts_GCFR$y, .)
#  names(extracted_rough_MAP_GCFR[[i]])[1] <- c("lon")
#  names(extracted_rough_MAP_GCFR[[i]])[2] <- c("lat")
#}

#extracted_rough_MAP_GCFR <-
#  rough_agg_MAP_GCFR %>%
#  map(raster::extract, rand_pts_GCFR) %>%
#  make_df_of_list_pts(val = "rough_agg_MAP")

#make_df_of_list_pts <- function(x) { ... } # The rest of this fn def is in `11_my_funs.R`
#make_df_of_list_pts(val = "rough_agg_MAP") %>%
#map(na.exclude)

# /Junk


# Importing that again ------------------------------------------------------------------

reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_MAP_GCFR/")
extracted_rough_MAP_GCFR <-
  read.csv(
    paste0(
      reswd_temp, "extracted_rough_MAP_GCFR.csv"
      #format(Sys.time(), "%Y-%m-%d"), ".csv"
    )
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
  width = 10, height = 5
)
. <- extracted_rough_MAP_GCFR
  par(mfrow = c(1, 2))
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

# 2017-05-10 23:07 --- </>

# Compare that to the non-rarefifed rough_agg data:
. <- abs_vs_rough_MAP_GCFR_df
  visreg(m1a_linear, jitter = T)
rm(.)
summary(extracted_rough_MAP_GCFR)
summary(abs_vs_rough_MAP_GCFR_df[, c(1, 3)])
summary(m1a_linear)
summary(m1c_linear)

# Junk

#for (i in 1:20) {
#  print(i)
#  extracted_rough_MAP_GCFR %>%
#    filter(fact == i) %$%
#    fact %>%
#    length() %>%
#    print()
#}
#
#while (i == T) {
#  while (j < 50) {
#    j <- j + 1
#    print(j)
#  }
#}

# /Junk

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


# Quantreg fiddling ---------------------------------------------------------------------
# (some code salvaged from SDM project script `SDM_analysis_final.R` (by Mike))

reswd_temp <- paste0(reswd, "extracted_abs_vs_rough_agg_MAP_GCFR/")
pdf(
  paste0(
    reswd_temp, "Quantreg_fiddling_&_connected_lines_",
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
    ylab = "rough(MAP(GCFR))",
    main = "Raw pixels' values (log(y + 1) b.c. of NAs)"
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
    ylab = "rough(MAP(GCFR))",
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

if (F) { # Don't need this, as is catered for by the previous plot above
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

# Let's filter each pt's data into separate data.frames in a list:
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
has_length_0
# Remove those points from the list:
filtered_dfs_list <- filtered_dfs_list[-has_length_0]

summary(filtered_dfs_list)

# Compute linear models (sensu linear) and check that they are all of the same length
# (just in case, b.c. I had a problem with model length in older versions of this code!)
filtered_dfs_list %>%
  map(function(x) { lm(rough_agg_MAP ~ fact, data = x) }) %>%
  map(length) %>%
  melt() %>%
  summary()

# 2017-05-11 14:02 --- <copied from above "`rough_agg_MAP` models" §>
# So, last night night I had a realisation about *why* the curves for
# some of the "rough ~ fact %by% pt" plots are DO NOT look right... stuff like a perfect
# negative parabola of pts, then fit by a *struggling* positive parabola...
# The reason is syntactical. When fitting interactions in `lm()`, if code `X1 * X2`, then
# the function automatically also fits for those terms in isolation too!
# But
# This is *not* the case with fitting polynomials in `lm()`... coding `I(X1 ^ 2)` does NOT
# cause the function to automatically code for X1 in isolation also... meaning that the
# reason those curves were struggling is that I was, accidentally, *forcing* them to
# fit positive parabolas to the data...
# </>

# Test

#x <- 1:100
#y <- x ^ 2
#plot(y ~ x)
#fit <- lm(y ~ I(x ^ 2))
#newdat <- data.frame(x = seq(min(x), max(x), length.out = 100))
#newdat$pred <- predict(fit, newdata = newdat)
#plot(y ~ x)
#with(newdat, lines(x = x, y = pred))

# /Test

#par(mfrow = c(2, 2))
#for (i in 1:10) {
#  plot(
#    rough_agg_MAP ~ fact,
#    data = extracted_rough_MAP_GCFR[extracted_rough_MAP_GCFR$pt_ID %in% i, ]
#  )
#  filtered_dfs_list[i] %>%
#    map(function(x) { lm(rough_agg_MAP ~ fact, data = x) }) %>%
#    map(abline)
#}
#par(op)

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
# And then plot the fit (quadratic, log-logged) on each pt_ID's plot:
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

# 2017-05-19 15:14 --- Don't need the plots below:

if (F) {

# 2017-05-10 23:08 --- Do the same as the lines just above, but plotting all the fits on
# the same plot, with no points, and only plotting a random 10 of the pt_IDs' fits:
plot(
  rough_agg_MAP ~ fact,
  data = extracted_rough_MAP_GCFR[extracted_rough_MAP_GCFR$pt_ID %in% has_length_not0[1], ],
  xlim = c(0, 21),
  ylim = c(0, 700),
  pch = " "
)
has_length_not0 <- as.numeric((names(filtered_dfs_list)))
i_rand <- sample(x = has_length_not0, size = 10, replace = F)
for (i in i_rand) {
  filtered_dfs_list[i] %>%
    map(
      function(x) { 
        fit <- lm(log(rough_agg_MAP) ~ log(fact) + I(log(fact) ^ 2), data = x)
        newdat <- data.frame(fact = seq(1, 20, length.out = 100))
        newdat$pred <- predict(fit, newdata = newdat)
        newdat %$%
          lines(x = fact, y = exp(pred))
        #with(newdat, lines(x = fact, y = exp(pred)))
      }
    ) %>%
    map(abline)
}

# 2017-05-11 13:31 --- is quantile regression useful when plotting
# by each pt_ID separately???
plot(
  rough_agg_MAP ~ fact,
  data = extracted_rough_MAP_GCFR[extracted_rough_MAP_GCFR$pt_ID %in% has_length_not0[1], ],
  xlim = c(0, 21),
  ylim = c(0, 700),
  pch = " "
)
#has_length_not0 <- as.numeric((names(filtered_dfs_list)))
#i_rand <- sample(x = has_length_not0, size = 100, replace = F)
for (i in 1:100) {
  filtered_dfs_list[i] %>%
    map(
      function(x) { 
        fit <- rq(
          log(rough_agg_MAP) ~ log(fact) + I(log(fact) ^ 2),
          tau = 0.5,
          method = "br",
          data = x
        )
        newdat <- data.frame(fact = seq(1, 20, length.out = 100))
        newdat$pred <- predict(fit, newdata = newdat)
        with(newdat, lines(x = fact, y = exp(pred)))
      }
    ) %>%
    map(abline)
}

# Test

#x <- 1:100
#y <- x ^ 2
#noise <- rnorm(length(x), mean = 10, sd = 1000)
#noisy_y <- y + noise
#plot(noisy_y ~ x)
#visreg(
#  lm(
#    noisy_y ~ I(x ^ 2)
#  )
#)

# /Test


# 2017-05-10 --- Some "debugging" attempts... Back when I *didn't* understand that you
# have to code the X terms ^ 1 AND ^ 2 , I was trying to figure out WHY-OH-WHY the
# quadratic curves seemed to be REFUSING to fit downward! SO... I used one of the fits
# from the pt_ID-plotting above, and simulated data from it, and tried to test fitting
# to that data!
# Cool code, pointless excercise in hind-sight.
. <- extracted_rough_MAP_GCFR[extracted_rough_MAP_GCFR$pt_ID %in% has_length_not0[23], ]
  plot(rough_agg_MAP ~ fact, data = .)
  fit <- lm(rough_agg_MAP ~ fact + I(fact ^ 2), data = .)
  visreg(fit)
  plot(fit$residuals)
  meta_fit <- lm(
    res ~ I(index ^ 2),
    data = data.frame(
      cbind(
        index = 1:20,
        res = fit$residuals
      )
    )
  )
  visreg(meta_fit)
  sim_meta_fit <- vector("list", length = 10)
  for (i in 1:100) {
    sim_meta_fit[[i]] <- simulate(meta_fit, nsim = 1)
    sim_meta_fit[[i]] %<>% cbind(index = 1:20, .)
    names(sim_meta_fit[[i]])[2] <- "res"
  }
  par(mfrow = c(4, 3), mar = c(1, 1, 1, 1))
  sim_meta_fit %>%
    map(plot, formula = res ~ index)
  par(op)
  . <- sim_meta_fit
    for (i in 1:length(sim_meta_fit)) {
      fit <- lm(res ~ index + I(index ^ 2), data = .[[i]])
      visreg(fit)
    }
rm(.)

# Junk

#visreg(
#  lm(
#    rough_agg_MAP ~ log(fact),
#    data = extracted_rough_MAP_GCFR[extracted_rough_MAP_GCFR$pt_ID %in% has_length_not0[22], ]
#  )
#)

# Still doesn't work:
#models_foo <- vector("list", length = 1000)
#for (i in 1:1000) {
#  fil <-
#    extracted_rough_MAP_GCFR %>%
#    filter(pt_ID == i)
#  m <- lm(
#    rough_agg_MAP ~ fact,
#    data = fil
#  )
#  models_foo[[i]] <- m
#}
#models_foo %>%
#  map(visreg)
#visreg(models_foo[[3]])

# /Junk

}

# PCA -----------------------------------------------------------------------------------

# Making a PCA of the `rough_agg_MAP` extracted sample pt values, with the `fact` as an
# explanatory/label variable

# 2017-05-12 07:35 --- 
# (The code & notes below are largely copied from `pca_question_for_tony.Rmd`, as this
# version of the code and notes is the cleanest!)

# 2017-05-19 15:21 --- Don't run this, run the version in the script
# `06_preliminary_anal_trimmed_down_2017-05-16.R`'s PCA §.

if (F) {

# <copied>

summary(extracted_rough_MAP_GCFR)

extracted_rough_MAP_GCFR_tbl <- 
  extracted_rough_MAP_GCFR %>%
  spread(key = fact, value = rough_agg_MAP) %>%
  na.omit()

head(     extracted_rough_MAP_GCFR_tbl)  # Compare the table I made there...
summary(t(extracted_rough_MAP_GCFR_tbl)) # ... to the transposed table.

# I will do a PCA on both here, for explanation. We originally ran it on the transposed
# data (hereafter `rough_pca_tp`), but I think it may actually be that we *want* to run
# it on the untransposed data (hereafter `rough_pca_untp`).
rough_pca_utp <- prcomp(  extracted_rough_MAP_GCFR_tbl[, -c(1:3)])
rough_pca_tp  <- prcomp(t(extracted_rough_MAP_GCFR_tbl[, -c(1:3)]))
# (exclude the lon, lat, and pt_ID columns!)

#Let's have a look at each:
par(mfrow = c(1, 2))
plot(rough_pca_utp)
plot(rough_pca_tp)
biplot(rough_pca_utp)
biplot(rough_pca_tp)
par(op)

# You will notice that the `biplot()` function, when applied to the PCA we originally
# devised, reveals that the `fact` variable has been missattributed, for our purposes,
# to the points, and that each point ID has been treated as a variable! For this reason,
# I think we should be using the untransposed data!

# Interestingly, pattern of points in the `biplot()` of the *un*transposed data is
# alternatively reproduced by:
plot(rough_pca_utp$x[, 1], rough_pca_utp$x[, 2])

# While the the pattern in the `biplot()` of the transposed data is alternatively seen in:
plot(rough_pca_tp$x[, 1], rough_pca_tp$x[, 2])

# Thus, it also seems that `$x` (a.k.a. `[[5]]`) in a `prcomp()` output is in fact
# the PC scores table, while `$rotation` is the eigen vector coefficients for
# each label variable in the PCA's input table.
# (The `prcomp()` package help file concurs with this...)

# But! Fear not! Our **frontier breaking** and **exciting** interpretations from
# today (2017-05-11) still hold! The effect of this (I think it is fair to say) error may
# have been great on the absolute values of the PC scores, etc. But the pattern in
# PC1 vs PC2 is similar! And translates into geographical space in a way that I think
# leads to essentially the same conclusions we reached today with the transposed-data-PCA.

# Take a look:

# Add the various PC-scores to the data frame of roughness values and point IDs
extracted_rough_MAP_GCFR_tbl <- cbind(
  extracted_rough_MAP_GCFR_tbl,
  tp_pc1  = rough_pca_tp$rotation[, 1], # What we did before
  tp_pc2  = rough_pca_tp$rotation[, 2], # ''
  utp_pc1 = rough_pca_utp$x[, 1],       # What I have done with the utp data's PCA
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

# Here is the PC1 score of the transposed data's PCA, represented as the size of the
# point in it's location in the GCFR (as we saw today (2017-05-11))...
par(mar = c(0, 0, 0, 0))
plot(GCFR_border)
extracted_rough_MAP_GCFR_tbl %$%
  points(
    lon, lat,
    pch = 20,
    cex = (tp_pc1 + min(tp_pc1) + 0.01) * 10 # makes the point size ~ pc1, visibly
  )

# ... and the PC2 scores (also an interesting pattern)...
plot(GCFR_border)
extracted_rough_MAP_GCFR_tbl %$%
  points(
    lon, lat,
    pch = 20,
    cex = (tp_pc2 + min(tp_pc2) + 0.2) * 10
  )

# And now for the PC1 scores from the *un*transposed data's PCA:
plot(GCFR_border)
extracted_rough_MAP_GCFR_tbl %$%
points(
  lon, lat,
  pch = 20,
  cex = (utp_pc1 / 1000) + (-min(utp_pc1) / 1000) + 0.2
)

# (And it's PC2 as well:)
plot(GCFR_border)
extracted_rough_MAP_GCFR_tbl %$%
  points(
    lon, lat,
    pch = 20,
    cex = (utp_pc2 / 1000) + (-min(utp_pc2) / 1000) + 0.2
  )
par(op)

# </copied>

# 2017-05-11 16:20 --- Interpretation of these map, after meeting with Tony today:
# - The PC1 scores appear to be spatially segregated in the GCFR's MAP layer.
#     - A low PC1 score here means that the pt is to the left along PC1
#     - Interpreting *that* in conjunction with the 


par(mfrow = c(3, 3), mar = c(rep(1, 4)))
plot_raster_list2(rough_agg_MAP_GCFR, GCFR_border)
par(op)

# Junk

#plot(
#  pca$rotation[, 1] ~
#    na.omit(extracted_rough_MAP_GCFR$rough_agg_MAP)
#    [
#      extracted_rough_MAP_GCFR$pt_ID %in% droplevels(extracted_rough_MAP_GCFR_tbl$pt_ID)
#    ]
#)

# /Junk


# Saving the PCA plots nicely -----------------------------------------------------------

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

require(gridExtra)
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
    reswd_temp, "PC1&2_plot_",
    format(Sys.time(), "%Y-%m-%d"), ".pdf"
  ),
  width = 5, height = 5
)
plot(rough_pca_utp$x, ylim = c(-350, 350), xlim = c(-1050, 1050))
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
dev.off()

}

# </> -----------------------------------------------------------------------------------

