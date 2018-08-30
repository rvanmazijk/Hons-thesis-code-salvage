# <Raster randomisation fiddling>

# Hons thesis
# Ruan van Mazijk

# created:      2017-06-22
# last edited:  2017-06-27


# Setup ---------------------------------------------------------------------------------

rm(list = ls())
source("Scripts/i_my_funs.R")
source("Scripts/ii_my_objs.R")
source("Scripts/v_prelim_anal_fn.R")

# Typology of agg rough shuff operation orders ------------------------------------------

if (FALSE) {
  
                                                                              # Name    Needed 
                                                                              # - - - - - - - -
  MAP_GCFR_buffered                                                           # R
  
  shuff_MAP_GCFR <- shuffle_raster(MAP_GCFR_buffered)                         # SR
  
  agg_shuff_MAP_GCFR <- custom_aggregate_loop(shuff_MAP_GCFR, facts = 2:15)   # ASR
  agg_shuff_MAP_GCFR %<>% c(shuff_MAP_GCFR, .)
  names(agg_shuff_MAP_GCFR)[1] <- "1"
  
  rough_agg_shuff_MAP_GCFR <- agg_shuff_MAP_GCFR %>%                          # HASR      *
    map(terrain, "roughness")
  
  agg_MAP_GCFR <- custom_aggregate_loop(MAP_GCFR_buffered, facts = 2:15)      # AR
  agg_MAP_GCFR %<>% c(MAP_GCFR_buffered, .)
  names(agg_MAP_GCFR)[1] <- "1"
  #agg_MAP_GCFR_df <- agg_MAP_GCFR %>%
  #  as_df_raster_vals_list(n = c(1:15)) 
  #names(agg_MAP_GCFR_df) <- c("MAP", "fact")
  
  rough_agg_MAP_GCFR <- agg_MAP_GCFR %>%                                      # HAR       *
    map(terrain, "roughness")
  
  shuff_rough_agg_MAP_GCFR <- shuffle_raster(rough_agg_MAP_GCFR)              # SHAR      *
  
  shuff_agg_MAP_GCFR <- shuffle_raster(agg_MAP_GCFR)                          # SAR
  
  rough_shuff_agg_MAP_GCFR <- shuff_agg_MAP_GCFR %>%                          # HSAR      *
    map(terrain, "roughness")
  
  
  # Summary:
  rough_agg_MAP_GCFR        # HAR:   rough( agg(          r   ) )
  rough_agg_shuff_MAP_GCFR  # HASR:  rough( agg(   shuff( r ) ) )
  rough_shuff_agg_MAP_GCFR  # HSAR:  rough( shuff( agg(   r ) ) )
  shuff_rough_agg_MAP_GCFR  # SHAR:  shuff( rough( agg(   r ) ) )
  
  # Plot & compare
  par(mfrow = c(4, 4))
  plot_from_list(rough_agg_MAP_GCFR)
  plot.new()
  plot_from_list(rough_agg_shuff_MAP_GCFR)
  plot.new()
  plot_from_list(rough_shuff_agg_MAP_GCFR)
  plot.new()
  plot_from_list(shuff_rough_agg_MAP_GCFR)
  plot.new()
  par(op)

}

# HASRs ---------------------------------------------------------------------------------

if (FALSE) {
  
  # Concl: HASR [= rough(agg(shuff(r)))] is the one of interest
  # t.f. cf prelim anal on HASR (tbd) vs HAR (on disc)
  # 2017-06-22 14:49 <done> --- make 100x HASRs
  
  #HASR_list_MAP_GCFR_buffered <- many_HASRs(
  #  x        = MAP_GCFR_buffered,
  #  how_many = 100, 
  #  facts    = 2:15
  #)
  # (don't need bc the do_prelim_anal() fn i wrote does the agg() and h() istelf! )
  # So, tf, I just need an SR (a shuffled raster):
  SR_list_MAP_GCFR_buffered <- many_SRs(x = MAP_GCFR_buffered, how_many = 100)
  
  # 2017-06-22 17:54 <done> --- Now let's run it on each of the (HA)SRs!
  
  summary(SR_list_MAP_GCFR_buffered)
  
  rand_dirs <- c(1:length(SR_list_MAP_GCFR_buffered)) %>% as.character()
  
  dir.create("/Volumes/RUAN_UCT/hons_null_raster_test/")
  for (i in 1:length(rand_dirs)) {
    dir.create(paste0(
      "/Volumes/RUAN_UCT/hons_null_raster_test/",
      rand_dirs[i], "/"
    ))
  }
  
  # Ran through all 100: [ √ ] (as of 2017-06-23 11:38)
  for (i in 1:length(SR_list_MAP_GCFR_buffered)) {
    
    dir_i <- paste0(
      "/Volumes/RUAN_UCT/hons_null_raster_test/",
      rand_dirs[i], "/"
    )
    
    do_prelim_anal(
      x                 = SR_list_MAP_GCFR_buffered[[i]],
      # 2017-06-22 18:11 --- oops the prelim anal fn does agg and rough anyway,
      # so i only really need the base res shuffled raster in each HASR,
      # and I still will get HASRs
      border            = GCFR_border,
      border_buffer     = GCFR_border_buffered,
      facts             = 2:15,
      var_name          = "MAP",
      region_name       = "GCFR",
      reswd_temp        = dir_i,
      x_tester          = MAP_GCFR_0.05,
      how_many_rand_pts = 1000
    )
    
    print(paste("prelim_anal of HASR permutation no.", i, "of 100 completed"))
  
  }
  
  
  # Now let's look at the pattern of roughness vs fact (with the assoc. akaike weight of
  # the m1c_qu_ll1 model) for each of these randomisations:
  
  # The randomised rasters' models' w_i's:
  rand_wis <- vector("list", length = length(SR_list_MAP_GCFR_buffered))
  
  for (i in 1:length(SR_list_MAP_GCFR_buffered)) {
    
    f <- list.files(
      path = paste0(
        "/Volumes/RUAN_UCT/hons_null_raster_test/",
        rand_dirs[i], "/", "models_MAP_GCFR/"
      ),
      pattern = "models_buffered_........23\\.csv"
    )
    
    dir_i <- paste0(
      "/Volumes/RUAN_UCT/hons_null_raster_test/",
      rand_dirs[i], "/", "models_MAP_GCFR/",
      f
    )
    
    rand_wis[[i]] <- read.csv(file = dir_i)
    
  }
  
  rand_wis %>%
    reshape2::melt() %>%
    filter(
      X == "m1c_qu_ll1",
      variable == "w_i"
    ) %>%
    dplyr::select(value) %>%
    as_vector() %>%
    hist()
    
  # Now let's look at the pattern of roughness vs fact (with the model outputs & summaries
  # of the m1c_qu_ll1 model for each of these randomisations (using `broom::`!):
  
  rand_models <- vector("list", length = length(SR_list_MAP_GCFR_buffered))
  
  for (i in 1:length(SR_list_MAP_GCFR_buffered)) {
    
    f <- list.files(
      path = paste0(
        "/Volumes/RUAN_UCT/hons_null_raster_test/",
        rand_dirs[i], "/", "models_MAP_GCFR/"
      ),
      pattern = "m1c_qu_ll1.RData"
    )
    
    dir_i <- paste0(
      "/Volumes/RUAN_UCT/hons_null_raster_test/",
      rand_dirs[i], "/", "models_MAP_GCFR/",
      f
    )
    
    load(file = dir_i)
    rand_models[[i]] <- m1c_qu_ll1
    
  }
  
  rand_p <- vector("numeric", length = length(rand_betas))
  for (i in 1:length(rand_models)) {
    rand_p[i] <- summary(rand_models[[i]])$coefficients[12]
  }
  hist(rand_p)
  
  # 2017-06-23 15:49 --- Stop, this is the wrong kind of randomisation! see § below:

}


# HSARs ---------------------------------------------------------------------------------

# 2017-06-23 16:24 --- 
# As of my meeting w/ Mike & Tony (on 2017-06-23, @ 14:00), it was revealed to me that
# I should actually only shuffled the rasters AFTER the aggregation, and then h() etc.
# This is because i want to preserve the VALUES of cells across aggregation factors,
# and THEN shuffle them around to remove the autocorrelation in them, to generate a null
# landscape.
# PS I only need to do this once, not many times (though that may change < TODO >),
# and generate one null raster (for which i will set.seed(57701)!).
# So what I need is an HSAR, not a set of HASRs. So I tweaked the functions a bit, and
# ta-dah:
# (I also tweaked do_prelim_anal() st if I provide agg_x or rough_agg_x,
# it skips the things already done to x)


# Let's try just one:
set.seed(57701)
do_prelim_anal(
  agg_x             = many_SARs(MAP_GCFR_buffered, facts = 2:15)[[1]],
  #rough_agg_x = many_HSARs(MAP_GCFR_buffered, facts = 2:15)[[1]],
                    # DON'T DO or agg_x ≠ rough_agg_x
                    # (see deprecation note in function's script)
  border            = GCFR_border,
  border_buffer     = GCFR_border_buffered,
  facts             = 2:15,
  var_name          = "MAP",
  region_name       = "GCFR",
  reswd_temp        = paste0(reswd, "null_raster_HSAR_MAP_GCFR_buffered/"),
  x_tester          = MAP_GCFR_0.05,
  how_many_rand_pts = 1000
)

if (FALSE) {
  
  # Looking at the output in the output folders, looks good,
  # but I wanna repeat it some, as I seem to be getting some artefacts
  # in the random rasters... (as expected, but I wanna avg away from them)
  
  set.seed(Sys.time()) # to "undo" `set.seed(57701)`
  SAR_list_MAP_GCFR_buffered <- many_SARs(MAP_GCFR_buffered, how_many = 100, facts = 2:15)
  rand_dirs <- c(1:length(SAR_list_MAP_GCFR_buffered)) %>% as.character()
  
  dir.create("/Volumes/RUAN_UCT/hons_null_raster_HSAR_MAP_GCFR_buffered_many/")
  for (i in 1:length(rand_dirs)) {
    dir.create(paste0(
      "/Volumes/RUAN_UCT/hons_null_raster_HSAR_MAP_GCFR_buffered_many/",
      rand_dirs[i], "/"
    ))
  }
  
  for (i in 1:length(SAR_list_MAP_GCFR_buffered)) {
    
    dir_i <- paste0(
      "/Volumes/RUAN_UCT/hons_null_raster_HSAR_MAP_GCFR_buffered_many/",
      rand_dirs[i], "/"
    )
    
    do_prelim_anal(
      agg_x             = SAR_list_MAP_GCFR_buffered[[i]],
      border            = GCFR_border,
      border_buffer     = GCFR_border_buffered,
      facts             = 2:15,
      var_name          = "MAP",
      region_name       = "GCFR",
      reswd_temp        = dir_i,
      x_tester          = MAP_GCFR_0.05,
      how_many_rand_pts = 1000
    )
  
    print(paste("prelim_anal of HSAR permutation no.", i, "of 100 completed"))
    
  }
  
}

# 2017-06-26 12:53 --- Okay, so the 100 null rasters seem fine. For now,
# let's continue with the analysis on the 1 that I stored in
# "reswd/null_raster_HSAR_MAP_GCFR_buffered/", though.

extracted_HSAR_MAP_GCFR <- as_tibble(read.csv(paste0(
  reswd, "null_raster_HSAR_MAP_GCFR_buffered/", "dfs_MAP_GCFR/",
  "extracted_abs_vs_rough_MAP_GCFR_df.csv"
)))
extracted_HSAR_MAP_GCFR

f <- list.files(
  path = paste0(
    reswd, "null_raster_HSAR_MAP_GCFR_buffered/",
    "models_MAP_GCFR"
  ),
  pattern = ".RData"
)

models_HSAR_MAP_GCFR <- vector("list", length = 9)
for (i in 1:9) {
  dir_i <- paste0(
    reswd, "null_raster_HSAR_MAP_GCFR_buffered/",
    "models_MAP_GCFR/",
    f[i]
  )
  models_HSAR_MAP_GCFR[[i]] <- get(load(file = dir_i))
  # get() allows the load() cmd to be parsed as an object
}
names(models_HSAR_MAP_GCFR) <- f
models_HSAR_MAP_GCFR

# not gonna deal with the PCA object just yet (no. [9])...
for (i in 1:8) print(summary(models_HSAR_MAP_GCFR[[i]])$coefficients)
for (i in 1:8) print(summary(models_HSAR_MAP_GCFR[[i]])$r.squared)

# ... here's the PCA:
summary(models_HSAR_MAP_GCFR$rough_pca_utp.RData)


# Ranking the obs raster within the null raster for each factor of aggregation ----------

extracted_HSAR_MAP_GCFR

# Read in some good old raw, abs vs rough data:
extracted_HAR_MAP_GCFR <- as_tibble(read.csv(paste0(
  reswd, "extracted_abs_vs_rough_agg_MAP_GCFR/",
  "extracted_rough_MAP_GCFR_buffered_df.csv"
)))

# Merge them:
HAR_HSAR_MAP_GCFR_join <- left_join(
  extracted_HAR_MAP_GCFR, extracted_HSAR_MAP_GCFR,
  by = c("X", "lon", "lat", "pt_ID", "fact")
)
names(HAR_HSAR_MAP_GCFR_join)[6:7] <- c("rough_agg_MAP", "rough_shuff_agg_MAP")
HAR_HSAR_MAP_GCFR_join %<>%
  gather("case", "roughness", 6:7)

HAR_HSAR_MAP_GCFR_join %$% {
  print(t.test(
    roughness[case == "rough_agg_MAP"],
    roughness[case == "rough_shuff_agg_MAP"]
  ))
  print(var.test(
    roughness[case == "rough_agg_MAP"],
    roughness[case == "rough_shuff_agg_MAP"]
  ))
}
# 2017-06-26 14:35 --- Clear that null pattern ≠ obs pattern! Yay!

# rough vs fact, for null & obs:
ggplot(data = HAR_HSAR_MAP_GCFR_join %>% filter(fact <= 15)) +
  geom_boxplot(aes(x = as.factor(fact), y = roughness, col = case)) +
  geom_smooth(
    aes(x = fact, y = roughness, col = case), 
    method = "lm"
  )
# 2017-06-26 14:33 --- Clear that null pattern ≠ obs pattern! Yay!
  
# rank(rough, by = all (i.e. out of 30k)) vs fact for null & obs:
HAR_HSAR_MAP_GCFR_join %>%
  mutate(roughness_rank_overall = rank(roughness)) %>%
  filter(fact <= 15) %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(fact), y = roughness_rank_overall, col = case)) +
  geom_smooth(
    aes(x = fact, y = roughness_rank_overall, col = case),
    method = "lm"
  )
# 2017-06-26 14:36 --- Clear that null pattern ≠ obs pattern! Yay!

# rank(rough, by = case (i.e. out of 2k (1k x 2 cases))) vs fact, for null & obs:
as_tibble(transform(
  HAR_HSAR_MAP_GCFR_join, 
  roughness_rank_fact = ave(
    HAR_HSAR_MAP_GCFR_join$roughness, HAR_HSAR_MAP_GCFR_join$fact, 
    FUN = function(x) rank(x, ties.method = "first")
  )
)) %>%
  filter(fact <= 15) %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(fact), y = roughness_rank_fact, col = case)) +
  geom_smooth(
    aes(x = fact, y = roughness_rank_fact, col = case),
    method = "lm"
  )
# 2017-06-26 14:42 --- Clear that null pattern ≠ obs pattern! Yay!


# rank(rough, by = fact (i.e. out of 15k (1k x 15 facts))) vs fact, for null & obs:
as_tibble(transform(
  HAR_HSAR_MAP_GCFR_join, 
  roughness_rank_case = ave(
    HAR_HSAR_MAP_GCFR_join$roughness, HAR_HSAR_MAP_GCFR_join$case, 
    FUN = function(x) rank(x, ties.method = "first")
  )
)) %>%
  filter(fact <= 15) %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(fact), y = roughness_rank_case, col = case)) +
  geom_smooth(
    aes(x = fact, y = roughness_rank_case, col = case), 
    method = "lm"
  )
# 2017-06-26 14:42 --- Clear that null pattern ≠ obs pattern! Yay!

# note for all these plots, the CIs are there, they are just TINY!


# Combined PCA of null raster and original raster ---------------------------------------

extracted_HSAR_MAP_GCFR
extracted_HSAR_MAP_GCFR$pt_ID %<>% paste0("_shuff")

extracted_HAR_MAP_GCFR <- as_tibble(read.csv(paste0(
  reswd, "extracted_abs_vs_rough_agg_MAP_GCFR/",
  "extracted_rough_MAP_GCFR_buffered_df.csv"
)))

null_vs_obs_roughMAP_GCFR <-
  extracted_HAR_MAP_GCFR[extracted_HAR_MAP_GCFR$fact <= 15, 4:6] %>% 
  rbind(extracted_HSAR_MAP_GCFR[, 4:6]) %>%
  spread(key = fact, value = rough_agg_MAP) %>%
  na.omit()
case <- vector(length = length(null_vs_obs_roughMAP_GCFR$pt_ID))
null_vs_obs_roughMAP_GCFR <- cbind(case, null_vs_obs_roughMAP_GCFR)
for (i in 1:length(null_vs_obs_roughMAP_GCFR$pt_ID)) {
  foo_char <- as.character(null_vs_obs_roughMAP_GCFR$pt_ID[i])
  if (stringr::str_detect(foo_char, "shuff")) {
    null_vs_obs_roughMAP_GCFR$case[i] <- "null"
  } else {
    null_vs_obs_roughMAP_GCFR$case[i] <- "obs"
  }
}
foo_pca <- prcomp(null_vs_obs_roughMAP_GCFR[, -c(1, 2)])
plot(foo_pca$x[, c(1, 2)], col = as.factor(null_vs_obs_roughMAP_GCFR$case))
plot(foo_pca$x[, c(1, 3)], col = as.factor(null_vs_obs_roughMAP_GCFR$case))
plot(foo_pca$x[, c(2, 3)], col = as.factor(null_vs_obs_roughMAP_GCFR$case))
plot(foo_pca$x[, c(1, 4)], col = as.factor(null_vs_obs_roughMAP_GCFR$case))
plot(foo_pca$x[, c(2, 4)], col = as.factor(null_vs_obs_roughMAP_GCFR$case))
plot(foo_pca$x[, c(3, 4)], col = as.factor(null_vs_obs_roughMAP_GCFR$case))
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)

  
# </> -----------------------------------------------------------------------------------

