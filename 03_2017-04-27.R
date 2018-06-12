# [...]

# Hons thesis
# Ruan van Mazijk

# created:      2017-04-18
# last edited:  2017-04-27


# Setup ---------------------------------------------------------------------------------

rm(list = ls())
if (!require(pacman)) {
  install.packages("pacman", dep = T)
  library(pacman)
} else {
  library(pacman)
}
p_load(
  magrittr, tidyverse, broom, ggplot2, visreg, beepr,
  raster, rasterVis, sp, dismo, rgdal, spatstat
)

datwd <- "/users/ruanvanmazijk/Documents/R/Hons_thesis/Data/"
giswd <- "/Volumes/RUAN'S HDD/GIS/"
reswd <- "/users/ruanvanmazijk/Documents/R/Hons_thesis/Results/"

op <- par()

# CRS -----------------------------------------------------------------------------------

std_CRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# Import CHIRPS stacks (GCFR) from disc -------------------------------------------------

# 2017-04-24 14:20 --- Only do if needed

#chirps_monthly_GCFR <- stack()
#for (i in 1:434) {
#  x <-
#    paste0(reswd, "CHIRPS_monthly_GCFR.grd") %>%
#    raster(band = i)
#  chirps_monthly_GCFR %<>% stack(x)
#}
#
chirps_annual_GCFR <- stack()
for (i in 1:36) {
  x <-
    paste0(reswd, "CHIRPS_annual_GCFR.grd") %>%
    raster(band = i)
  chirps_annual_GCFR %<>% stack(x)
}

#chirps_monthly_GCFR
#chirps_annual_GCFR


# Calculations: MAP_GCFR ----------------------------------------------------------------

# Create raster that is the mean annual ppt in each cell (a.k.a. pixel)

# 2017-04-24 14:21 --- Commented out the code that creates MAP_GCFR, as it
# has already been written to disc

#(MAP_GCFR <-
#  chirps_annual_GCFR %>%
#  mean() %T%
#  plot())                              # = "BIO1" in BioClim
#writeRaster(                           # Write to disc (two formats!; both work!)
#  MAP_GCFR,
#  filename = paste0(reswd, "MAP_GCFR.tif"),
#  overwrite = T
#)
#writeRaster(
#  MAP_GCFR,
#  filename = paste0(reswd, "MAP_GCFR.grd"),
#  overwrite = T
#)
(MAP_GCFR <-                            # 2017-04-24 14:21 --- imports MAP_GCFR from disc
   paste0(reswd, "MAP_GCFR.grd") %>%
   raster() %T>%
   plot())


# Calculations: Which indiv. yr had the lowest/highest MAP? (GCFR) ----------------------

# 2017-04-24 14:20 --- The next few lines depend on having imported the CHIRPS stacks
# that have been commented out in the prev. section.

# The goal of the next few lines is to be able figure out which year had
# the lowest (& highest) MAP on its own. Let me explain:

chirps_annual_GCFR_tbl_df <-           # Create a tibble of columns for each year,
  chirps_annual_GCFR[] %>%             # filled with all 28,063 ppt per cell p.a. values
  as_tibble()
MAP_each_yr_GCFR <-                    # Make MAP for each year
  chirps_annual_GCFR_tbl_df %>%
  na.exclude() %>%
  map(mean) %>%
  unlist() %>%
  as.data.frame()
MAP_each_yr_GCFR %<>% cbind(., row.names(.))    # Add the year names as a column
colnames(MAP_each_yr_GCFR) <- c("MAP", "Year")  # Tidy up the colnames

(max_MAP_yr_GCFR <-                   # Subset (`dplyr::filter()`) the MAPs for each year
  MAP_each_yr_GCFR %>%                # to find the years that have the min & max MAP
  filter(MAP == max(MAP)))
(min_MAP_yr_GCFR <-
  MAP_each_yr_GCFR %>%
  filter(MAP == min(MAP)))            # Not sure if I need these? But useful, lol.


# Calculations: Plotting functions/stats of ppt in each cell (GCFR) ---------------------

# Now I want to plot 3 plots:
#   - a plot of the min ppt recorded in each cell
#   -       ''      mean            ''            (a.k.a. MAP s.s.)
#   -       ''      max             ''
# all over the 36 year CHIRPS period
# To do this, the solution I came up with (b.c. piping gets angry with rasters!)
# was to write a function that can create all 3 of these plots, and then simply
# tee (`%T<%`) the chirps_annual_GCFR raster into it. Nice, simple, singular piping so that we
# don't get anything too hairy (and grumpy) from dplyr!

plot_zlim_fixed <- function(x, f = c("min", "mean", "max"), main) {
  # To get fixed zlim scale across all 3 plots
  # Given param. `f` the possible args, and with "min" as the default.
  if (f == "min") {
    plot(min(x), zlim = c(0, 1500), main = main)
  } else if (f == "mean") {
    plot(mean(x), zlim = c(0, 1500), main = main)
  } else if (f == "max") {
    plot(max(x), zlim = c(0, 1500), main = main)
  }
}

plot_zlim_vary <- function(x, f = c("min", "mean", "max"), main) {
  # To get min-to-max scale for each plot
  # (gives better resolution of ppt values)
  if (f == "min") {
    lowest <- min(x)
    plot(
      lowest,
      zlim = c(min(lowest[]), max(lowest[])),
      main = main
    )
  } else if (f == "mean") {
    avg <- mean(x)
    plot(
      avg,
      zlim = c(min(avg[]), max(avg[])),
      main = main
    )
  } else if (f == "max") {
    highest <- max(x)
    plot(
      highest,
      zlim = c(min(highest[]), max(highest[])),
      main = main
    )
  }
}

par(mfrow = c(2, 3))
chirps_annual_GCFR                                     %T>%
  plot_zlim_fixed(f = "min",  main = "min_annual_ppt") %T>%
  plot_zlim_fixed(f = "mean", main = "MAP s.s.")       %T>%
  plot_zlim_fixed(f = "max",  main = "max_annual_ppt") %T>%
  plot_zlim_vary( f = "min",  main = "min_annual_ppt") %T>%
  plot_zlim_vary( f = "mean", main = "MAP s.s.")       %T>%
  plot_zlim_vary( f = "max",  main = "max_annual_ppt") #%T>%
par(op)

# But wait, let me make this as generalised as possible!
# How about a version that can accept any function?
# Let's try :)

plot_fun_raster_char <- function(x, f) {
  #if (!is.raster(x)) {
  #  stop("x must be a raster")
  #}
  x_eval <- eval(parse(text = paste0(f, "(x)")))    # Takes the char. string in `f` as a
  plot(                                             # function to be applied to `x`
    x_eval,
    zlim = c(min(x_eval[]), max(x_eval[])),
    main = paste0(f, " ", substitute(x))            # Auto heading = f's name + x's name
  )
}

plot_fun_raster <- function(x, f) {                 # Better version where f can be a fn
  #if (!is.raster(x)) {
  #  stop("x must be a raster")
  #}
  if (!is.function(f)) {
    stop("f must be a function")
  }
  f_x <- f(x)                                       # name properly! (although my char.
  plot(                                             # parsing stuff in the old version
    x,                                              # was pretty clever ;) )
    zlim = c(min(f_x[]), max(f_x[])),
    main = paste0(substitute(f), " ", substitute(x))
  )
}

# Now let's define some functions that `purrr::map()` can apply cleanly to rasters
range_ss <- function(x) {
  range_sl <- range(x)
  max(range_sl) - min(range_sl)
}
calc_sd <- function(x) {
  calc(x, fun = sd)
}
calc_var <- function(x) {
  calc(x, fun = var)
}

# Now I pipe `chirps_annual_GCFR` into each `plot_fun_raster()` with the fn
# that I want to `map()` to it!

par(mfrow = c(2, 3))
chirps_annual_GCFR              %T>%
  plot_fun_raster(f = range_ss) %T>%
  plot_fun_raster(f = calc_sd)  %T>%
  plot_fun_raster(f = calc_var) %T>%
  plot_fun_raster(f = min)      %T>%
  plot_fun_raster(f = mean)     %T>%
  plot_fun_raster(f = max)      #%T>%
par(op)

# I can do this with even less code by making the char. vector `funs` and pipe
# `chirps_annual_GCFR` once to a looped `plot_fun_raster`

par(mfrow = c(2, 3))
funs <- c("range_ss", "calc_sd", "calc_var", "min", "mean", "max")
for (i in 1:length(funs)) {
  chirps_annual_GCFR %>% plot_fun_raster_old(f = funs[i])
  # I have to use `plot_fun_raster_char()` here because I want a full-on vector
  # of different funs, and I only know how to get this to work with chars...
}
par(op)

# Even better: I can define a new version of `plot_fun_raster_char()` w/ the loop
# included!
# Then I can nice and simply apply that to `chirps_annual_GCFR` and `fun`, or
# a char. vector of my choice!

plot_fun_raster_loop <- function(x, f) {
  #if (!is.raster(x)) {
  #  stop("x must be a raster")
  #}
  for (i in 1:length(f)) {                              # A loop through the char. /vector/ `f`!
    x_eval <- eval(parse(text = paste0(f[i], "(x)")))   # Thus you can list all the fns you want!
    plot(                                               # And you'll get a plot for e/ of 'em!!
      x_eval,
      zlim = c(min(x_eval[]), max(x_eval[])),
      main = paste0(f[i], " ", substitute(x))
    )
  }
}

par(mfrow = c(2, 3))
funs %>% 
  plot_fun_raster_loop(x = chirps_annual_GCFR, f = .) # ignore the error that says there
par(op)                                               # are too many arguments. It's b.c.
                                                      # funs is a vector...

# Now let's try some more funs!

calc_median <- function(x) {
  calc(x, fun = median)
}
diff_mean_median <- function(x) {     # Made this b.c. I want to see where mean > median
  mean(x) - calc_median(x)
}
diff_median_mean <- function(x) {     # Made this b.c. v/v
  calc_median(x) - mean(x)
}

par(mfrow = c(2, 2))
c("mean", "calc_median", "diff_mean_median", "diff_median_mean") %>% 
  plot_fun_raster_loop(x = chirps_annual_GCFR, f = .) # ignore the error that says there
par(op)                                               # are too many arguments. It's b.c.
                                                      # funs is a vector...


# Calculations: Distribution of ppt vals in each cell (GCFR) ----------------------------

# So, looking at where mean > median ppt, and v/v, that got me thinking about the
# distribution of ppt values... Like, if the mean > median, is it right-skewed? etc.

# So let's randomly sample some cells, with the goal of visualising the dbn of ppt vals.

#chirps_annual_GCFR[c(round(rnorm(700, 55, 15), 0)), c(round(rnorm(700, 105, 30), 0))] %>%
#  na.exclude() %>%
#  map(hist(x = ., breaks = 1000))

# ^-- This selects ~ N cells, thus sampling more from the centre of the GCFR...
# and that's not what I want!
# What I want is a uniform sampling of GCFR cells, as follows:

x_rand <- sample(x = c(1:113), size = 50, replace = T)
y_rand <- sample(x = c(1:211), size = 50, replace = T)
# is there a better, native `raster` way of sampling random pts from a raster?

hist_loop <- function(x) {                              # A fn that plots histograms
    hist(x, breaks = 200, main = paste0(substitute(x))) # that `map()` can use cleanly on
}                                                       # the data.frame of cell vals.
plot_from_list <- function(x) {
  for (i in 1:length(x)) {                              # A fn that plots the objects
    plot(x[[i]], main = names(x[i]))                    # stored in the list that
  }                                                     # `map()` produces.
}

quartz()
par(mfrow = c(3, 6))
chirps_annual_GCFR[x_rand, y_rand] %>%
  as.data.frame() %>%
  na.exclude() %>%
  map(hist_loop) %>%
  plot_from_list #lapply(plot)
par(op)
dev.new()

dens_loop <- function(x) {                              # same as `hist_loop()` but for
  density(x, main = paste0(substitute(x)))              # kernel density plot
}
plot_from_list_overlay <- function(x) {                 # plots all the objects from the
  plot(x[[1]], ylim = c(0, 0.007))                      # `map()` list, but overlayed
  for (i in 2:length(x)) {                              # (made for density lines of all
    #lines.default(x[[i]], col = heat.colors(34)[i])    # years all in one plot)
    lines.default(x[[i]])
  }
}
chirps_annual_GCFR[x_rand, y_rand] %>%
  as.data.frame() %>%
  na.exclude() %>%
  map(dens_loop) %>%
  #plot_from_list
  plot_from_list_overlay()                              # Fascinating plot!!!

# Lastly, what about some boxplots of the ppt vals in each yr?
# (in order to visualise the dbn)

chirps_annual_GCFR[x_rand, y_rand] %>%
  as.data.frame() %>%
  na.exclude() %>%
  gather() ->
  chirps_annual_GCFR_gather

names(chirps_annual_GCFR_gather) <- c("Year", "ppt")
chirps_annual_GCFR_gather$Year %<>% as.factor()
boxplot(
  ppt ~ Year,
  data = chirps_annual_GCFR_gather,
  xaxt = "n"
)                                                       # Fascinating plot!!!
axis(1, at = 1:36, labels = as.character(1981:2016), las = 3)

chirps_annual_GCFR[] %>%                # Getting the Years as a proper numeric var
  as.data.frame() ->
  chirps_annual_GCFR_df
names(chirps_annual_GCFR_df) <- as.character(1981:2016)
chirps_annual_GCFR_gather_yrlabs <-
  chirps_annual_GCFR_df %>%
  gather()
names(chirps_annual_GCFR_gather_yrlabs) <- c("Year", "ppt")
summary(chirps_annual_GCFR_gather_yrlabs)
class(chirps_annual_GCFR_gather_yrlabs$Year) <- "numeric"
m1 <- lm(
  ppt ~ Year,
  data = chirps_annual_GCFR_gather_yrlabs
)
summary(m1)
visreg::visreg(m1, points.par = c(pch = ""), fill.par = c(col =  "black"))


# Bioclimatic variables (e.g. TWQ) (GCFR) -----------------------------------------------

chirps_monthly_GCFR_1981 <-
  chirps_monthly_GCFR %>%
  names() %>%
  startsWith("chirps.v2.0.1981.") %>%
  which() %>%
  raster::subset(chirps_monthly_GCFR, subset = .)

# Or can use `dismo`! Automatically makes all 19 BioClim vars
# (but needs tmin and max rastes in order to run at all! Fuck you Hijman's!)
# JJ"<

#dismo::biovars(chirps_monthly_GCFR_1981)

chirps_monthly_GCFR_1981_tib <- 
  chirps_monthly_GCFR[] %>%
  as_tibble() %>%
  dplyr::select(starts_with("chirps.v2.0.1981."))
  # must say `dplyr::` bc `raster::select()` hides `dplyr::select()`.

chirps_monthly_GCFR_1981_tib %>%
  map(mean, na.rm = T) %>%
  as_tibble() %>%
  gather() %>%
  arrange(desc(value))

dismo::biovars(chirps_monthly_GCFR_1981)

tib <- chirps_monthly_GCFR_1981[] %>% as_tibble()
#%>% select("chirps.v2.0.1981.01")
tib %>% select(chirps.v2.0.1981.01)

chirps_monthly_GCFR[] %>%
  tibble() %>%
  select(starts_with("chirps.v2.0.1981."))


# Correlograms --------------------------------------------------------------------------

# (Got this code from /somewhere/ online... find it!!!)

# Define function to draw random samples from a multivariate normal
# distribution
rmvn <- function(n, mu = 0, V = matrix(1)) {
  p <- length(mu)
  if (any(is.na(match(dim(V), p)))) {
    stop("Dimension problem!")
  }
  D <- chol(V)
  t(
    matrix(rnorm(n * p), ncol = p) %*%
      D + rep(mu, rep(n, p))
  )
}

# Set up a square lattice region
simgrid <- expand.grid(1:50, 1:50)
n <- nrow(simgrid)

# Set up distance matrix
distance <- as.matrix(dist(simgrid))
# Generate random variable
phi <- 0.05
X <- rmvn(1, rep(0, n), exp(-phi * distance))

# Visualize results
Xraster <- rasterFromXYZ(cbind(simgrid[, 1:2] - 0.5, X))
par(mfrow = c(1, 2))
plot(
  1:100,
  exp(-phi * 1:100),
  type = "l",
  xlab = "Distance",
  ylab = "Correlation"
)
plot(Xraster)

fit1 <- correlog(
  x = chirps_annual_GCFR$chirps.v2.0.2009,
  y = spdata$y,
  z = spdata$resid,
  increment = 5,
  resamp = 100,
  quiet = T
)
plot(fit1)


# Autocorrelation (GCFR) ----------------------------------------------------------------

plot(MoranLocal(chirps_annual_GCFR$chirps.v2.0.1981))
plot(chirps_annual_GCFR$chirps.v2.0.1981)

chirps_annual_GCFR$chirps.v2.0.1981 %T>%
  plot(main = "chirps.v2.0.1981") %>%
  MoranLocal() %>%
  plot(main = "Local Moran's I")

par(mfrow = c(1, 2))
MAP_GCFR %T>%
  plot(main = "MAP_GCFR") %>%
  MoranLocal() %>%
  plot(main = "Local Moran's I")
par(op)


# Calculations: Roughness of MAP_GCFR ---------------------------------------------------

roughMAP_GCFR <- terrain(MAP_GCFR, "roughness") # Calc. roughness
plot(roughMAP_GCFR)

roughMAP_GCFR_narm <- 
  MAP_GCFR %>%
  na.exclude() %>%
  terrain("roughness")
plot(roughMAP_GCFR_narm) # seems to be the same as with the NAs... so won't bother


# Comparing the effect of resolution on abs vs rough (MAP_GCFR) -------------------------

# Write a function that will automatically plot a raster's roughness ~ abs,
# model it linearly, and summarise.

plot_abs_vs_rough <- function(x) {
  
  #if (!is.raster(x)) {
  #  stop("x must be a raster")
  #}
  
  r <- terrain(x, "roughness")
  plot(r[] ~ x[], pch = 1, col = "grey")
  m <- lm(r[] ~ x[])
  print(summary(m))
  
  new <- data.frame(`x[]` = seq(0.025, 2000, by = 0.025))
  
  CI <- predict(m, newdata = new, interval = "confidence")
  lines(x[], CI[, 2], col = "green")
  lines(x[], CI[, 3], col = "green")
  
  PI <- predict(m, newdata = new, interval = "prediction")
  lines(x[], PI[, 2], col = "blue")
  lines(x[], PI[, 3], col = "blue")
  
  print("CI")
  print(summary(CI))
  print("PI")
  print(summary(PI))
  
  abline(m, col = "red", lwd = 2)
  
}
plot_abs_vs_rough(MAP_GCFR)

#m2 <- lm(roughMAP_GCFR[] ~ MAP_GCFR[])
#summary(m2)
#new <- data.frame(`MAP_GCFR[]` = seq(0.025, 2000, by = 0.025))
#CI <- predict(m2, newdata = new, interval = "confidence")
#lines(MAP_GCFR[], CI[, 2], col = "green")
#lines(MAP_GCFR[], CI[, 3], col = "green")
#PI <- predict(m2, newdata = new, interval = "prediction")
#lines(MAP_GCFR[], PI[, 2], col = "blue")
#lines(MAP_GCFR[], PI[, 3], col = "blue")
#summary(CI)
#summary(PI)
#abline(m2, col = "red", lwd = 2)

MAP_GCFR_0.05 <- MAP_GCFR
plot_abs_vs_rough(MAP_GCFR_0.05) # works!

# To resample a raster w/ custom params

custom_resample <- function(x, res, co_ord = std_CRS, method = "bilinear") {
  # `std_CRS` and "bilinear" are defaults now! yay!
  #if (!is.raster(x)) {
  #  stop("x must be a raster")
  #}
  y <-
    raster(resolution = res, crs = co_ord) %>%
    crop(x)
  y %<>%
    resample(x = x, y = ., method = method)
  y
}

# To resample a raster w/ custom params, specifically multiple res's,
# & store it all in a list.

custom_resample_loop <- function(x, res, co_ord = std_CRS, method = "bilinear") {
  #if (!is.raster(x)) {
  #  stop("x must be a raster")
  #}
  custom_resample_list <- vector("list", length = length(res))
  for (i in 1:length(res)) {
    y <-                                                      
      raster(resolution = res[i], crs = co_ord) %>%
      crop(x)
    y %<>%
      resample(x = x, y = ., method = method)
    custom_resample_list[[i]] <- y
  }
  custom_resample_list
  #names(custom_resample_list) <- as.character(res)
}

MAP_GCFR_0.1 <-
  custom_resample(
    MAP_GCFR_0.05,
    res = 0.1
  )

plot_abs_vs_rough(MAP_GCFR_0.1)


# 1. For resolutions from 0.05 to 1.00 degrees ------------------------------------------

ress <- seq(0.05, 1.00, by = 0.05)    # Creates a num. vector of the resolutions I want
resampled_MAP_GCFR <-                 # to resample to, and then pipe it into
  ress %>%                            # `custome_resample_loop()` on a given raster
  custom_resample_loop(               # (in this case `MAP_GCFR_0.05`)
    MAP_GCFR_0.05,                    # (which /is/ `MAP_GCFR`)
    res = .,                          # PS ignore the "too many args." error... it's
  )                                   # b.c. `ress` is a vector.

names(resampled_MAP_GCFR) <- as.character(ress)

# Plot out the rasters that have been resampled from the list

plot_raster_list <- function(x) {
  if (!is.list(x)) {
    stop("x must be a list")
  }
  for (i in 1:length(x)) { 
    plot(
      x[[i]],
      main = names(x)[i]
    )
  }
}

plot_raster_list(resampled_MAP_GCFR)

# Plots out those rasters' vals only (~ Index)

plot_raster_vals_list <- function(x) {
  if (!is.list(x)) {
    stop("x must be a list")
  }
  for (i in 1:length(x)) {
    plot(
      x[[i]][],
      main = names(x)[i]
    )
  }
}

plot_raster_vals_list(resampled_MAP_GCFR)

# Storing those rasters' vals only in a new list, and using `reshape2::melt()`
# to convert that list to a "gathered" data.frame

as_df_raster_vals_list <- function(x, n) {
  # for a list `x` and a vector of labels `n`
  # `n` would either be `ress` or `facts` for me
  if (!is.list(x)) {
    stop("x must be a list")
  }
  x_vals <- vector("list", length = length(x))
  for (i in 1:length(x)) {
    x_vals[[i]] <- unlist(x[[i]][])
  }
  names(x_vals) <- as.character(n)
  x_vals %<>% reshape2::melt()
  names(x_vals) <- c("ppt", "res")
  x_vals$res %<>% as.numeric()
  x_vals
}

resampled_MAP_GCFR_vals <- as_df_raster_vals_list(resampled_MAP_GCFR, n = ress)

# Plotting that! (i.e. the ppt vals ~ the res of the raster)

plot(ppt ~ res, data = resampled_MAP_GCFR_vals)


# 2. Plotting using `plot_abs_vs_rough()` -----------------------------------------------

# I.e. how does roughMAP dep. on MAP @ diff res's?

quartz()
par(mfrow = c(3, 3))
for (i in 1:length(ress)) {
  plot_abs_vs_rough(
    resampled_MAP_GCFR[[i]]
    #, main = names(range_of_ress)[i]
  )
}
par(op)
dev.new()

# JJ"<


# 2017-04-25 16:01 --- Thoughts ---------------------------------------------------------
# So, that's all very well and good. I finally figured out and got round to plotting
# roughMAP ~ MAP + res. Yay!
# This was a good first step and may yield very interesting results once I interprate
# them. (And do some resampling at SMALLER/FINER res's too!!)
# But... I actually need, from Tony's POV, to see the effect of grid cell (i.e. the
# avg of the cell in a grid cell)
# 
# JJ"<
# 
# (But good job today, Ruan! You worked!! It is possible!! EVEN essay stuff!!)

# (See the `raster` pkg .pdf document. It's very helpful!!)


# 2017-04-27 13:11 --- [...] ------------------------------------------------------------

# Cell AND grid-cell stuff!

# I.e. using `raster::aggregate()` to pull the mean vals into a new raster of a coarser
# resolution.

MAP_GCFR_0.1_avg <- aggregate(MAP_GCFR_0.05, fact = 2, fun = mean)
# Note, `fact = 2` means we double the size of the pixels, i.e. 0.05 deg^2 --> 0.1 deg^2

plot(MAP_GCFR_0.05) # the orignial, full res

MAP_GCFR_0.1_bil <- MAP_GCFR_0.1 # renaming for clarity

# Compare `MAP_GCFR_0.1` from my custom bilinear resampling loop
# with the mean-aggregated version ()

par(mfrow = c(1, 2))
plot(MAP_GCFR_0.1_bil)
plot(MAP_GCFR_0.1_avg)
par(op)

# Very, very similar... but NOT the same... --> consult Tony!

# In the mean time, let's run this mean-aggregation style approach across
# a range of resolutions again.

# Here is, basically, a fn that does exactly the same stuff as `custom_resample_loop()`,
# but with the `raster::aggregate()` fn instead of `resample()` :^)

custom_aggregate_loop <- function(x, fact, co_ord = std_CRS, method = mean) {
  #if (!is.raster(x)) {
  #  stop("x must be a raster")
  #}
  custom_aggregate_list <- vector("list", length = length(fact))
  for (i in 1:length(fact)) {
    y <- aggregate(x = x, fact = facts[i], fun = method)
    custom_aggregate_list[[i]] <- y
  }
  names(custom_aggregate_list) <- as.character(fact)
  custom_aggregate_list
}

# So, because `aggregate()` uses factors > 1 for making coarser rasters, and not absolute
# resolutions like `resample()`. Let me look at the ratio of my res's from above to the
# orginial resolution:

for (i in 1:length(ress)) {
  print(ress[i] / ress[1])
  }

# This shows me that, in fact, when your original res is 0.05 deg and you add
# increments of 0.05 deg linearly is the same as a geometric progression of base 0.05 :)

facts <- 2:21

# (Have to use factors > 1)
# (Maybe try fact ∈ (1, 2) as well?) 
# (Maybe also try a smaller range of facts? Something like 1:10? (i.e. 0.05 to HDS)
#   And with smaller increments between 1 & 2?)

# Applying `custome_aggregrate_loop()` to MAP_GCFR_0.1:

aggregated_MAP_GCFR <- custom_aggregate_loop(MAP_GCFR_0.1, fact = facts)

# Now let's use those wonderful functions I wrote above that free my rasters from the
# schackles of their list prison!

aggregated_MAP_GCFR %T>%
  plot_raster_list() %T>%
  plot_raster_vals_list() %T>%
  as_df_raster_vals_list(n = facts)

# Now let's use that old favourite, `plot_abs_vs_rough()`, to visualise! And model!

quartz()
par(mfrow = c(3, 3))
for (i in 1:length(facts)) {
  plot_abs_vs_rough(
    aggregated_MAP_GCFR[[i]]
    #, main = facts[i]              # Still can't get a nice solution for the mains?
  )
}
par(op)
dev.new()

# Now, what I want to do is have a miniature versions of `plot_abs_vs_rough()`, just w/
# the models:

mini_abs_vs_rough <- function(x) {  # This can be used on any 1 raster :)
  r <- terrain(x, "roughness")      # Ignore the definition-error
  lm(r[] ~ x[]) %>%
    summary()
}

# And let's expand this mini-fn to work a LIST of rasters, and save to a data.frame
# (much the same goal as `as_df_raster_vals_list()`, but very different code) (mostly
# b.c. of all the `broom::` I use. Very, very helpful pkg here!!!)

as_df_mini_abs_vs_rough <- function(x, fact) {
  if (!is.list(x)) {
    stop("x must be a list")
  }
  df <- data.frame()
  for (i in 1:length(fact)) {                 # For every factor we aggregated to...
    df_tidy <-                                # (= `length(x)` as well!)
      x[[i]] %>%
      mini_abs_vs_rough() %>%                 # ... apply the mini-fn ...
      broom::tidy() %>%                       # ... use `tidy()` to get the ests. & Ps...
      slice(2) %>%
      dplyr::select(estimate, p.value)
    df_glance <-
      x[[i]] %>%
      mini_abs_vs_rough() %>%
      broom::glance() %>%                     # ... and use 'glance()' to get the adj.R2.
      dplyr::select(adj.r.squared)
    df_i <- cbind(df_tidy, df_glance, fact = fact[i])
    df %<>% rbind(df_i)                       # Bind it all together, w/ a label for the
  }                                           # factor, and then `rbind()` each row (=
                                              # each)
  df # Output :^)
}

# Let's apply /that/ to our list of aggregated rasters:

fact_df <- as_df_mini_abs_vs_rough(aggregated_MAP_GCFR, fact = facts)

summary(fact_df)

# Sick!!!!

par(mfrow = c(1, 3))
plot(fact ~ ., data = fact_df) # Plotted x & y swapped, sorry. (but actually ok?)
par(op)

plot(p.value ~ estimate, data = fact_df)

ggplot(aes(x = fact, y = estimate), data = fact_df) +
  geom_point(aes(size = p.value, col = adj.r.squared))

# Look at those plots!!

# 2017-04-27 16:05 --- Tentative interpretations:
#     (* est. = slope of roughMAP ~ MAP)
#     1. est ~ grid cell size
#         - When cells between fact 1 & 10 (i.e. up to HDS), cell size seems inconseq.
#         - But beyond that, crazy shit --> slope increases ~ cell size!
#     2. P_est ~ grid cell size
#         - Extremely significant P-values below fact 15! Good news!
#         - Only at ca. fact 20 do the relationships fizz away!
#         - And /those/ cells (e.g fact 20) are HUGE.
#     3. adj.R^2 ~ grid cell size
#         - higher with smaller grid cell sizes
#         - not too surprising

# 2017-04-27 16:13 --- Future ideas:
#     * AIC() of the abs_vs_rough models?
#     * do all those different fact configurations
#     * do `as_df_mini_abs_vs_rough()` for the resample and ress versions


# Junk ----------------------------------------------------------------------------------

chirps_annual_GCFR[] %>%
  as.data.frame() %>%
  dplyr::select(chirps.v2.0.1981) %>%
  na.exclude() %>%
  hist(breaks = 1000)

chirps_annual_GCFR[] %>% # A histogram for ALL points, not just a sample of point!!
  na.exclude() %>%       # Obvi very slow, so do later
  map(histogram)

# ...

#chirps_annual_GCFR %>% cellStats(stat = "mean")

# ...

chirps_annual_GCFR[x, y] %>%
  as.data.frame() %>%
  na.exclude() %>%
  map(median) %>%
  unlist() %>% #%T>%
  plot() #%T>%
#lm(. ~ )

chirps_annual_GCFR[x, y] %>%
  as.data.frame() %>%
  na.exclude() %>%
  unlist() %>%
  #head() %>%
  plot()

# ...

#res(x) <- c(0.125, 0.125)

# ...

for (i in 1:length(ress)) {
  print(range(range_of_ress_vals_only[[i]], na.rm = T))
}

#lapply(range_of_ress_vals_only, `[[`, 2) %>% 
#  data.frame() %>% 
#  add_rownames("key") %>% 
#  gather(x, value, -key) %>% 
#  dplyr::select(-x)

# ...

MAP_vs_roughMAP_GCFR_df <- tibble(
  MAP = MAP_GCFR[],
  slopeMAP = terrain(MAP_GCFR, "slope")[],
  aspectMAP = terrain(MAP_GCFR, "aspect")[],
  TPIMAP = terrain(MAP_GCFR, "TPI")[],
  TRIMAP = terrain(MAP_GCFR, "TRI")[],
  roughMAP = terrain(MAP_GCFR, "roughness")[]
)

hMAP_models <- tibble(
  Response = names(MAP_vs_hMAP_GCFR_df),
  Model = MAP_vs_hMAP_GCFR_df %>% 
      names() %>% 
      paste(., " ~ MAP") %>% 
      map(as.formula) %>% 
      map(lm, data = MAP_vs_hMAP_GCFR_df)
)

summary(hMAP_models)

# ...

x <- data.frame(NULL)
y <- data.frame(NULL)
z <- data.frame(NULL)
a <- data.frame(NULL)
b <- data.frame(NULL)
c <- data.frame(NULL)
tibble(list(x, y, z, a, b ,c))

coeff <- vector("list", 6)
class(coeff)
coeff <- tibble(coeff)
class(coeff[1, 1])
x <- vector("list", 6)
for (i in 1:6) {
  x[i] <-
    hMAP_models$Model[[i]] %>%
    summary() %$%
    coefficients %>%
    as.data.frame() %>%
    as_tibble()
}
coeff[1, 1]

x <- tibble(coeff)

# ...

layerStats(chirps_annual_GCFR, stat = "pearson", na.rm = T)


# </> -----------------------------------------------------------------------------------