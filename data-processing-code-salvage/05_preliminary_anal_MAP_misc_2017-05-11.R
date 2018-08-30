# Formal "preliminary" analyses on MAP --- misc. & "non-core" analyses

# Hons thesis
# Ruan van Mazijk

# created:      2017-05-11
# last edited:  2017-05-11


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


# Applying `resample()` -----------------------------------------------------------------

# 2017-05-08 15:59 ---

#   After my meeting with Tony today, he agrees that `aggregate()` is
#   better, as the mean-style way of doing things is more intuitive.

#   And this preserves the pixel/grid-cell structure-alignment, whereas `resample()`-ed
#   pixels are not necessarily aligned with the input raster's pixels.

#   Though, this function may come in hand if I have to force some other raster, say
#   MODIS, to be a specific starting resolution (say 0.05 deg) if it comes from NASA in,
#   say, 0.06 deg resolution, in order to then use `aggregate()` on it with its pixels
#   aligned with those of all my other environmental vars.

ress <- seq(0.05, 1.00, by = 0.05) # as before
resampled_MAP_GCFR <- custom_resample_loop(MAP_GCFR_0.05, ress)

par(mfrow = c(2, 3))
plot_raster_list(resampled_MAP_GCFR)
par(op)

resampled_MAP_GCFR_vals <- as_df_raster_vals_list(resampled_MAP_GCFR, n = ress)
plot(val ~ res, data = resampled_MAP_GCFR_vals)

par(mfrow = c(2, 3))
resampled_MAP_GCFR %>%
  map(plot_abs_vs_rough)
par(op)


# Applying roughness calc and then `aggregate()` ----------------------------------------

# 2017-05-08 --- In pseudo-`R`-code:
#   MAP %>% h() -> roughMAP %>% aggregate(%both%) -> agg_rough_MAP; agg_MAP
# (See Fig. 1b in my notes from my meeting with Tony (2017-05-08), which I had *after*
# I wrote this script)

# 2017-05-09 --- Refactored this § of code today

rough_MAP_GCFR_0.05 <- 
  MAP_GCFR_0.05 %>%
  terrain("roughness")

# `agg_MAP_GCFR` already exists, and is identical in this case b) to what I made in a).

# Apply `agg()` to `rough_MAP_GCFR_0.05` /first/:
agg_rough_MAP_GCFR <- custom_aggregate_loop(rough_MAP_GCFR_0.05, fact = facts)
agg_rough_MAP_GCFR %<>% c(rough_MAP_GCFR_0.05, .) # incl. the original raster...
names(agg_roughMAP_GCFR)[1] <- "1"                # ... name it factor = 1

# `agg_MAP_GCFR_df` already exists too,
# and is identical in this case b) to what I made in a).

# Make the `_df` for `agg_rough_MAP_GCFR`:
agg_rough_MAP_GCFR_df <-
  agg_rough_MAP_GCFR %>%
  as_df_raster_vals_list(n = c(1, facts))

summary(agg_MAP_GCFR_df)
summary(agg_rough_MAP_GCFR_df)

names(agg_MAP_GCFR_df) <- c("MAP", "fact")
names(agg_rough_MAP_GCFR_df) <- c("agg_rough_MAP", "fact")

# Add `agg_rough`'s `agg_rough_MAP` column to my data.frame from a)
length(abs_vs_rough_MAP_GCFR_df[, 1]) # to check...
length(agg_rough_MAP_GCFR_df[, 1])    # ... same no. rows!!
abs_vs_rough_MAP_GCFR_df %<>%
  cbind(agg_rough_MAP_GCFR_df$agg_rough_MAP)
names(abs_vs_rough_MAP_GCFR_df)[4] <- "agg_rough_MAP"


# `agg_rough_MAP` models ----------------------------------------------------------------

. <- abs_vs_rough_MAP_GCFR_df
  m1b      <- lm(agg_rough_MAP ~ fact,        data = .)
  m1b_quad <- lm(agg_rough_MAP ~ I(fact ^ 2), data = .)
  m1b_log  <- lm(agg_rough_MAP ~ log(fact),   data = .)
  m2b      <- lm(agg_rough_MAP ~ MAP,         data = .)
  m3b      <- lm(MAP           ~ fact,        data = .)
  m4b      <- lm(agg_rough_MAP ~ fact + MAP,  data = .)
  m4b_i    <- lm(agg_rough_MAP ~ fact * MAP,  data = .)
rm(.)

summary(m1b     )
summary(m1b_quad)
summary(m1b_log )
summary(m2b     )
summary(m3b     )
summary(m4b     )
summary(m4b_i   )

. <- abs_vs_rough_MAP_GCFR_df
  visreg(m1b     )
  visreg(m1b_quad)
  visreg(m1b_log )
  visreg(m2b     )
  visreg(m3b     )
  visreg(m4b,   "fact", by = "MAP", breaks = 4, overlay = F)
  visreg(m4b_i, "fact", by = "MAP", breaks = 2, overlay = F)
rm(.)

AIC(m1b, m1b_quad, m1b_log, m4b, m4b_i) 

# 2017-05-09 11:29 TODO --- `plot()` model assumption validations!


# Comparing the 1st `aggregate()` method with the 2nd, ----------------------------------
# and the effect it has on the "{E -> h(E)} -- scale relationship"

# I.e. 1st method = Fig. 1a* = h(agg(...)) = `xx`
#      2nd method = Fig. 1b* = agg(h(...)) = `xx_2`

# * In my notes from my meeting with Tony (2017-05-08), which I had *after*
# I wrote this script

# cf_1_2 <- function(a, b) { ... } # The rest of this fn def is in `11_my_funs.R`

. <- abs_vs_rough_MAP_GCFR_df

  cf_1_2(m1a, m1b)
  cf_1_2(m1a_quad, m1b_quad)
  cf_1_2(m1a_log, m1b_log)
  
  cf_1_2(m2a, m2b) # not good models, per se, as I have multiplied the number of obs b.c.
  # of the 20 factors in that data.frame
  
  cf_1_2(m3a, m3b) # identical! as expected
  
  #cf_1_2(m4a, m4b)
  ## can't see ints nicely... will do this model and `_i` by hand:
  summary(m4a)
  summary(m4b)
  visreg(m4a, "fact", by = "MAP", breaks = 2, overlay = F)
  visreg(m4b, "fact", by = "MAP", breaks = 2, overlay = F) # hectic!
  
  summary(m4a_i)
  summary(m4b_i)
  visreg(m4a_i, "fact", by = "MAP", breaks = 2, overlay = F)
  visreg(m4b_i, "fact", by = "MAP", breaks = 2, overlay = F) # hectic!
  
  AIC(m1a, m1a_quad, m1a_log, m4a, m4a_i)
  AIC(m1b, m1b_quad, m1b_log, m4b, m4b_i)

rm(.)


# Fiddling: agg_rough vs rough_agg ------------------------------------------------------

# now for factor 2:20 only. I.e. removes the 1:1 strip,
# as rough_agg(fact = 1) = agg_rough(fact = 1)
. <-
  abs_vs_rough_MAP_GCFR_df %>%
  filter(fact != 1)
  m5 <- lm(rough_agg_MAP ~ agg_rough_MAP, data = .)
  visreg(m5)
  summary(m5)
rm(.)


# Junk ----------------------------------------------------------------------------------

par(mfrow = c(2, 3))
aggregated_MAP_GCFR %T>%
  plot_raster_list() %T>%
  plot_raster_vals_list() %T>%
  as_df_raster_vals_list(n = facts)
par(op)

par(mfrow = c(2, 3))
aggregated_MAP_GCFR %>%
  map(plot_abs_vs_rough)
par(op)

fact_df <- as_df_mini_abs_vs_rough(aggregated_MAP_GCFR, fact = facts)
summary(fact_df)

par(mfrow = c(1, 3))
plot(fact ~ ., data = fact_df) # Plotted x & y swapped, sorry. (but actually ok?)
par(op)

plot(p.value ~ estimate, data = fact_df)

ggplot(aes(x = fact, y = estimate), data = fact_df) +
  geom_point(aes(size = p.value, col = adj.r.squared))


# </> -----------------------------------------------------------------------------------

