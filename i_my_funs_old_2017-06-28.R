# My defined functions for my Hons thesis analyses

# Hons thesis
# Ruan van Mazijk

# created:      2017-05-07
# last edited:  2017-06-21


# Setup ---------------------------------------------------------------------------------

#.libPaths("/Users/ruanvanmazijk/Documents/R/R_pkgs")

if (!require(pacman)) {
  install.packages("pacman", dep = T)
} else {
  library(pacman)
}
library(pacman)
p_load(
  magrittr, tidyverse, beepr,   reshape2,  here,
  quantreg, broom,     ggplot2, gridExtra, visreg,
  raster,   rasterVis, sp,      dismo,     rgdal,  spatstat
)

# All relative to the project wd:
datwd <- paste0(getwd(), "/Data/")
giswd <- "/Volumes/RUAN'S HDD/GIS/"
modwd <- paste0(giswd, "MOD11C3/order_501145540_GeoTiffs/")
reswd <- paste0(getwd(), "/Results/")

op <- par()

std_CRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# plot_fun_raster() et al. --------------------------------------------------------------

# Takes a raster `x` and plots a raster that is a calculation/fn of it: `f(x)`.
# `f` can be a char vector length > 1, in which case loop through it to plot all of them.
plot_fun_raster_loop <- function(x, f) {
  #if (!is.raster(x)) {
  #  stop("x must be a raster")
  #}
  for (i in 1:length(f)) {                          
    x_eval <- eval(parse(text = paste0(f[i], "(x)")))
    plot(                                           
      x_eval,
      zlim = c(min(x_eval[]), max(x_eval[])),
      main = paste0(f[i], " ", substitute(x))
    )
  }
}

# Same as `plot_fun_raster_loop`, but no loop capability (i.e., `f` must have length = 1).
plot_fun_raster_char <- function(x, f) {
  #if (!is.raster(x)) {
  #  stop("x must be a raster")
  #}
  x_eval <- eval(parse(text = paste0(f, "(x)")))
  plot(                                         
    x_eval,
    zlim = c(min(x_eval[]), max(x_eval[])),
    main = paste0(f, " ", substitute(x))        
  )
}

# Same as `plot_fun_raster_loop`, but no loop capability (i.e., `f` must have length = 1).
# And this version doesn't rely on char parsing, but instead directly coding `f(x)`.
plot_fun_raster <- function(x, f) {
  #if (!is.raster(x)) {
  #  stop("x must be a raster")
  #}
  if (!is.function(f)) {
    stop("f must be a function")
  }
  f_x <- f(x)                                       
  plot(                                             
    x,                                              
    zlim = c(min(f_x[]), max(f_x[])),
    main = paste0(substitute(f), " ", substitute(x))
  )
}


# Own funs for rasters ------------------------------------------------------------------

# Convenience fns for calcs on rasters (for some raster `x`)

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

calc_median <- function(x) {
  calc(x, fun = median)
}

diff_mean_median <- function(x) {
  mean(x) - calc_median(x)
}

diff_median_mean <- function(x) {
  calc_median(x) - mean(x)
}


# plot_abs_vs_rough() et al. ------------------------------------------------------------

# Takes some raster `x`, calculates the "roughness" raster of it `r`, and plots/models
# the relationship between `r` and `x` values, with confidence and prediction intervals,
# and returns summary tables for these.
plot_abs_vs_rough <- function(x) {
  
  #if (!is.raster(x)) {
  #  stop("x must be a raster")
  #}
  
  r <- terrain(x, "roughness")
  plot(
    r[] ~ x[],
    xlab = substitute(x),
    ylab = paste0("h(", substitute(x), ")"),
    pch = 1,
    col = "grey")
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
  
  out_list <- list(m, CI, PI)
  out_list %<>% c(., out_list %>% map(summary))
  names(out_list) <- c("model", "CI", "PI", "model_summary", "CI_summary", "PI_summary")
  return(out_list)
  
}

# Mini-version of `plot_abs_vs_rough`, w/o CIs, PIs, or plots.
mini_abs_vs_rough <- function(x) {  # This can be used on any 1 raster :)
  r <- terrain(x, "roughness")      # Ignore the definition-error
  lm(r[] ~ x[]) %>%
    summary()
}

# Mini-version of `plot_abs_vs_rough`, w/o CIs, PIs, or plots,
# and storing the `broom` summary of the model as a dataframe.
as_df_mini_abs_vs_rough <- function(x, fact) {
  if (!is.list(x)) {
    stop("x must be a list")
  }
  df <- data.frame()
  for (i in 1:length(fact)) {                 # For every factor we aggregated to...
    df_tidy <- x[[i]] %>%                     # (= `length(x)` as well!).
      mini_abs_vs_rough() %>%                 # ... apply the mini-fn ...
      broom::tidy() %>%                       # ... use `tidy()` to get the ests. & Ps...
      slice(2) %>%
      dplyr::select(estimate, p.value)
    df_glance <- x[[i]] %>%
      mini_abs_vs_rough() %>%
      broom::glance() %>%                     # ... and use `glance()` to get the adj.R2.
      dplyr::select(adj.r.squared)
    df_i <- cbind(df_tidy, df_glance, fact = fact[i])
    df %<>% rbind(df_i)                       # Bind it all together, w/ a label for the
  }                                           # factor, and then `rbind()` each row (=
                                              # each)
  df # Output :^)
}


# Custom resampling & aggregation -------------------------------------------------------

# Convenience fn to resample a raster to a diff resolution (bilinear is the default 
# method).
custom_resample <- function(x, res, co_ord = std_CRS, method = "bilinear") {
  # `std_CRS` and "bilinear" are defaults now! yay!
  #if (!is.raster(x)) {
  #  stop("x must be a raster")
  #}
  y <- raster(resolution = res, crs = co_ord) %>%
    crop(x)
  y %<>% resample(x = x, y = ., method = method)
  y
}

# Same as `custom_resample`, but with loop for vector length > 1 of resolutions desired.
# Returns all resampled rasters in a list.
custom_resample_loop <- function(x, res, co_ord = std_CRS, method = "bilinear") {
  #if (!is.raster(x)) {
  #  stop("x must be a raster")
  #}
  custom_resample_list <- vector("list", length = length(res))
  for (i in 1:length(res)) {
    y <- raster(resolution = res[i], crs = co_ord) %>%
      crop(x)
    y %<>% resample(x = x, y = ., method = method)
    custom_resample_list[[i]] <- y
  }
  custom_resample_list
  #names(custom_resample_list) <- as.character(res)
}

# Same idea as `custom_resample_loop`, but with `aggregate()` instead, so as to only create
# aligned, integer multiples of the original raster.
# Returns all resampled rasters in a list.
custom_aggregate_loop <- function(x, facts, co_ord = std_CRS, method = mean) {
  #if (!is.raster(x)) {
  #  stop("x must be a raster")
  #}
  custom_aggregate_list <- vector("list", length = length(facts))
  for (i in 1:length(facts)) {
    y <- aggregate(x = x, fact = facts[i], fun = method)
    custom_aggregate_list[[i]] <- y
  }
  names(custom_aggregate_list) <- as.character(facts)
  custom_aggregate_list
}


# Misc. plots ---------------------------------------------------------------------------

# Convenience fn for histograms when using `map()`.
hist_loop <- function(x) {                          
  hist(x, breaks = 200, main = paste0(substitute(x)))
}                          

# Convenience fn for desnity kernel when using `map()`.
dens_loop <- function(x) {                          
  density(x, main = paste0(substitute(x)))          
}


# Plotting from and querying lists ------------------------------------------------------

# Literally just plots the objects in a list, with main = the name in the list.
plot_from_list <- function(x) {
  if (!is.list(x)) {
    stop("x must be a list")
  }
  for (i in 1:length(x)) {          
    plot(x[[i]], main = names(x[i]))
  }                                 
}

# Same as `plot_from_list`, but instead of 1 plot per item, it plots the first one, and
# then overlays the rest (e.g. lots of line graphs)!
plot_from_list_overlay <- function(x) {
  if (!is.list(x)) {
    stop("x must be a list")
  }
  plot(x[[1]], ylim = c(0, 0.007))                  
  for (i in 2:length(x)) {                          
    #lines.default(x[[i]], col = heat.colors(34)[i])
    lines.default(x[[i]])
  }
}

# Basically `plot_from_list`, but tailored to rasters.
plot_raster_list <- function(x) {
  if (!is.list(x)) {
    stop("x must be a list")
  }
  for (i in 1:length(x)) { 
    plot(x[[i]], main = names(x)[i])
  }
}

# Same as `plot_raster_list`, bt with an argument for plotting a polygon too!
plot_raster_list2 <- function(r, b) {
  if (!is.list(r)) {
    stop("x must be a list")
  }
  for (i in 1:length(r)) {
    plot(r[[i]], main = names(r)[i])      # plot raster[[i]] from list
    plot(b, add = T)                      # plot border
  }
}

# Plots the VALUES instead of the map of a raster.... but otherwise basically the
# same as `plot_raster_list`
plot_raster_vals_list <- function(x) {
  if (!is.list(x)) {
    stop("x must be a list")
  }
  for (i in 1:length(x)) {
    plot(x[[i]][], main = names(x)[i])
  }
}

# Stores the values of rasters (in a list `x`) in a dataframe, with some index `n`.
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
  names(x_vals) <- c("val", "res") # or "fact"
  x_vals$res %<>% as.numeric()
  x_vals
}

# Makes a dataframe out of the values of pts in each item in a list `x`, such that the
# list `x` is returned with `melt()`-ed dataframes in each slot, aot the original objs.
make_df_of_list_pts <- function(x, val) {
  if (!is.list(x)) {
    stop("x must be a list")
  }
  for (i in 1:length(x)) {
    names(x[[i]]) <- 1:length(x[[i]])
    x[[i]] %<>%
      reshape2::melt(id.var = names(x)[i]) %>%
      cbind(
        pt_ID = rownames(.),
        .,
        fact = names(x)[i]
      )
    names(x[[i]])[2] <- val
  }
  x
}


# Comparing models of h() vs factor -----------------------------------------------------

# Convenience fn for comparing 2 lms `a` and `b`,
# using `print()`, `summary()`, and `visreg()`

cf_1_2 <- function(a, b) {

  print(substitute(a))
  print(summary(a))
  print(paste0("###########################"))
  print(substitute(b))
  print(summary(b))
  
  visreg(a)
  title(main = paste0(substitute(a), ":    ", a$call[2]))
  visreg(b)
  title(main = paste0(substitute(b), ":    ", b$call[2]))

}


# Quantile regression plots -------------------------------------------------------------

# `quant_plot` plots the 95%, 75%, 50% (= mediaj), 25%, and 5% quantile regression fits
# for some data `x` vs `y`.
# The fn also has nice convenient label arguments :).
# `quant_combo_plot` below is exactly the same, but ALSO fits and plots the
# lm/mean regression fit.

quant_plot <- function(x, y, dat,
                       x_lab = "x", y_lab = "y",
                       main_lab = "", sub_lab = "") {
  
  #Plot the data pts
  plot(
    y ~ jitter(x, 2.5), # jittered bc when I used it on the `fact` data, looks better!
    data = dat,
    ylab = y_lab,
    xlab = x_lab,
    main = main_lab,
    sub = sub_lab,
    pch = ".",
    col = "grey"
  )
  
  # Generatre a sequence of `x` to predict the quantiles from
  x_seq <- dat %$%
    seq(
      min(x, na.rm = T), max(x, na.rm = T),
      (max(x, na.rm = T) - min(x, na.rm = T)) / 1000
    )
  
  # 95% quantile fit
  mq95 <- rq(
    log(y) ~ log(x) + I(log(x) ^ 2),
    tau = 0.95,
    data = dat
  )
  #mq95ll <- rq(log(y) ~ log(x) + I(log(x) ^ 2), tau = 0.95, data = dat)
  #min(AIC(mq95), AIC(mq95ll))
  # Plot that fit
  y95 <- mq95 %>%
    predict(list(x = x_seq))
  lines(
    x = x_seq, y = exp(y95),
    lty = 2, lwd = 1, col = "red"
  )
  #summary(mq95)
  
  # And repeat for 75, 50, 25, & 5%:
  
  mq75 <- rq(
    log(y) ~ log(x) + I(log(x) ^ 2),
    tau = 0.75,
    data = dat
  )
  y75 <- mq75 %>%
    predict(list(x = x_seq))
  lines(
    x = x_seq, y = exp(y75),
    lty = 1, lwd = 1, col = "red"
  )
  
  mq50 <- rq(
    log(y) ~ log(x) + I(log(x) ^ 2),
    tau = 0.50,
    data = dat
  )
  y50 <- mq50 %>%
    predict(list(x = x_seq))
  lines(
    x = x_seq, y = exp(y50),
    lty = 1, lwd = 2, col = "red"
  )
  #summary(extents_quant0.50_quadratic)
  
  #rho <- function(u, tau = 0.5) {
  #  u * (tau - (u < 0))
  #}
  #(R1 <- 1 - extents_quant0.50_quadratic$rho / extents_quant0.50_quadratic0$rho)
  #summary(lm(QR_y ~ QR_x + I(QR_x ^ 2)))
  #summary(lm(QR_y ~ QR_x))
  
  mq25 <- rq(
    log(y) ~ log(x) + I(log(x) ^ 2), 
    tau = 0.25, 
    data = dat
  )
  y25 <- mq25 %>%
    predict(list(x = x_seq))
  lines(
    x = x_seq, y = exp(y25),
    lty = 1, lwd = 1, col = "red"
  )
  
  mq05 <- rq(
    log(y) ~ log(x) + I(log(x) ^ 2),
    tau = 0.05,
    data = dat
  )
  y05 <- mq05 %>%
    predict(list(x = x_seq))
  lines(
    x = x_seq, y = exp(y05),
    lty = 2, lwd = 1, col = "red"
  )
  #summary(extents_quant0.05_quadratic)
  
}

quant_combo_plot <- function(x, y, dat,
                             x_lab = "x", y_lab = "y",
                             main_lab = "", sub_lab = "") {
  
  plot(
    y ~ jitter(x, 2.5),
    data = dat,
    ylab = y_lab,
    xlab = x_lab,
    main = main_lab,
    sub = sub_lab,
    pch = ".",
    col = "grey"
  )
  
  x_seq <- dat %$%
    seq(
      min(x, na.rm = T), max(x, na.rm = T),
      (max(x, na.rm = T) - min(x, na.rm = T)) / 1000
    )
  
  m <- lm(
    log(y) ~ log(x) + I(log(x) ^ 2),
    data = dat
  )
  fit <- m %>%
    stats::predict(., newdata = data.frame(x = x_seq), interval = "confidence")
  lines(
    x = x_seq, y = exp(fit[, 1]),
    lty = 1, lwd = 2, col = "black"
  )
  lines(
    x = x_seq, y = exp(fit[, 2]), 
    lty = 2, lwd = 1, col = "black"
  )
  lines(
    x = x_seq, y = exp(fit[, 3]), 
    lty = 2, lwd = 1, col = "black"
  )
  
  mq95 <- rq(
    log(y) ~ log(x) + I(log(x) ^ 2),
    tau = 0.95,
    data = dat
  )
  #mq95ll <- rq(log(y) ~ log(x) + I(log(x) ^ 2), tau = 0.95, data = dat)
  #min(AIC(mq95), AIC(mq95ll))
  y95 <- mq95 %>%
    stats::predict(., list(x = x_seq))
  lines(
    x = x_seq, y = exp(y95),
    lty = 2, lwd = 1, col = "red"
  )
  #summary(mq95)
  
  mq75 <- rq(
    log(y) ~ log(x) + I(log(x) ^ 2),
    tau = 0.75,
    data = dat
  )
  y75 <- mq75 %>%
    stats::predict(., list(x = x_seq))
  lines(
    x = x_seq, y = exp(y75),
    lty = 1, lwd = 1, col = "red"
  )
  
  mq50 <- rq(
    log(y) ~ log(x) + I(log(x) ^ 2),
    tau = 0.50,
    data = dat
  )
  y50 <- mq50 %>%
    stats::predict(., list(x = x_seq))
  lines(
    x = x_seq, y = exp(y50),
    lty = 1, lwd = 2, col = "red"
  )
  #summary(extents_quant0.50_quadratic)
  
  #rho <- function(u, tau = 0.5) {
  #  u * (tau - (u < 0))
  #}
  #(R1 <- 1 - extents_quant0.50_quadratic$rho / extents_quant0.50_quadratic0$rho)
  #summary(lm(QR_y ~ QR_x + I(QR_x ^ 2)))
  #summary(lm(QR_y ~ QR_x))
  
  mq25 <- rq(
    log(y) ~ log(x) + I(log(x) ^ 2), 
    tau = 0.25, 
    data = dat
  )
  y25 <- mq25 %>%
    stats::predict(., list(x = x_seq))
  lines(
    x = x_seq, y = exp(y25),
    lty = 1, lwd = 1, col = "red"
  )
  
  mq05 <- rq(
    log(y) ~ log(x) + I(log(x) ^ 2),
    tau = 0.05,
    data = dat
  )
  y05 <- mq05 %>%
    stats::predict(., list(x = x_seq))
  lines(
    x = x_seq, y = exp(y05),
    lty = 2, lwd = 1, col = "red"
  )
  #summary(extents_quant0.05_quadratic)

}


# Using bash-QGIS-GDAL commands in R ----------------------------------------------------

# 2017-05-22 08:48 --- This is necessary, because of a LONG-ASS reason... which I shall
# document here: `bash_gdal_algs_documentation.txt`

# Now, let's apply the `gdal_info` algorithm to `MOD11B3.A2000032.h19v11.006.
# 2015168203953.hdf`, so that we can see what we're dealing with, in `bash`!
bash_gdalinfo <- function(
  x, # the `.hdf` file name; <chr>
  x_dir = getwd(), # the dir of the `.hdf`` is in; <chr>; default is `getwd()`
  gdal_dir = "/Library/Frameworks/GDAL.framework/Versions/1.11/Programs/" # QGIS GDAL
) {
  system(
    paste0(
      gdal_dir, "gdalinfo ",
      "\"", x_dir, x, "\""
    )
  )
}

# ... And for just one band.
bash_gdalinfo_one <- function(
  x, prefix = "HDF4_EOS:EOS_GRID:",
  band, # e.g. :MODIS_Grid_Month_6km_LST:Emis_20
  x_dir = getwd(),
  gdal_dir = "/Library/Frameworks/GDAL.framework/Versions/1.11/Programs/"
) {
  system(
    paste0(
      gdal_dir, "gdalinfo ",
      prefix, "\"", x_dir, x, "\"", band
    )
  )
}

# Now, let's apply `gdal_translate` to all bands of `MOD11B3.A2000032.h19v11.006.
# 2015168203953.hdf` in `bash`, using the gdal library executables that QGIS uses.
bash_gdaltranslate_all <- function(
  x, out, # the desired output file/dir; <chr>; typically a `.tif`
  x_dir = getwd(), out_dir = getwd(),
  gdal_dir = "/Library/Frameworks/GDAL.framework/Versions/1.11/Programs/"
) {
  system(
    paste0(
      gdal_dir, "gdal_translate -sds ",
      "\"", x_dir,   x,   "\"", " ",
      "\"", out_dir, out, "\""
    )
  )
}

# Now, let's apply `gdal_translate` to just one band of `MOD11B3.A2000032.h19v11.006.
# 2015168203953.hdf`.
bash_gdaltranslate_one <- function(
  x, prefix = "HDF4_EOS:EOS_GRID:", band, out,
  x_dir = getwd(), out_dir = getwd(), 
  gdal_dir = "/Library/Frameworks/GDAL.framework/Versions/1.11/Programs/"
) {
  system(
    paste0(
      gdal_dir, "gdal_translate ",
      prefix, "\"", x_dir,   x,   "\"", band, " ",
              "\"", out_dir, out, "\""
    )
  )
}

# This version of `bash_gdaltranslate_one()` can handle a list of HDF4s, converting
# the specified band to a GeoTiff.
bash_gdaltranslate_loop <- function(
  x, prefix = "HDF4_EOS:EOS_GRID:", band, out_format = ".tif",
  x_dir = getwd(), out_dir = getwd(), 
  gdal_dir = "/Library/Frameworks/GDAL.framework/Versions/1.11/Programs/"
) {
  if (!is.atomic(x) | !is.character(x)) {
    warning("x must be an atomic (i.e. 1-D) character vector!")
    warning("e.g. `order$name` (NOT the data-frame `order`)")
  }
  for (i in 1:length(x)) {
    bash_gdaltranslate_one(
      x = x[i], band = band,
      out = paste0(
        x[i] %>% substr(0, nchar(.) - 4),
        out_format
      ),
      x_dir = x_dir, out_dir = out_dir
    )
  }
}
# 2017-05-23 17:31 <nevermind> --- make progress bar work smoothly?
# the fn currently overloading console output with gdal outputs


# <DEPREC> project()-crop()-mask()-mean()-ing monthly MODIS rasters ---------------------
# This approach to dealing with the MODIS data yielded the unfortunate result of mean-ing
# the rasters as Feb + Jan + Jan + Jan + ...,
#                Mar + Feb + Feb + Feb + ...,
# etc.
# Scrap! Deprecated!!

#proj_crop_mask_mean_deprec <- function(month = c("Jan", "Feb", "Mar",
#                                                 "Apr", "May", "Jun",
#                                                 "Jul", "Aug", "Sep",
#                                                 "Oct", "Nov", "Dec"),
#                                       years, # <chr>
#                                       crs = std_CRS,
#                                       border = c(GCFR_border_buffered, GCFR_border),
#                                       dir = modwd,
#                                       prefix = "MOD11C3.A",
#                                       suffix = ".*.tif") {
##out_filename # <chr> e.g. `paste0(reswd, "MODIS_Jan_v2_GCFR_0.05_buffered.grd")`
#  
#  f <- vector("list", length = length(years))
#  for (i in 1:length(years)) {
#    f[[i]] <- list.files(
#      path = dir,
#      pattern = paste0(prefix, years[i], suffix)
#    )[
#      switch(
#        month,
#        Jan = 1,  Feb = 2,  Mar = 3,
#        Apr = 4,  May = 5,  Jun = 6,
#        Jul = 7,  Aug = 8,  Sep = 9,
#        Oct = 10, Nov = 11, Dec = 12
#      )
#    ]
#  }
#  #return(f)
#  
#  pb <- txtProgressBar(min = 0, max = length(years), style = 3)
#  raster_stack <- stack()
#  for (i in 1:length(years)) {
#    
#    x <- paste0(dir, f[[i]]) %>% raster()
#    setTxtProgressBar(pb, i - 0.8)
#    cat("\n\r", years[i], ": Raster made")
#    
#    x %<>% projectRaster(crs = crs)
#    setTxtProgressBar(pb, i - 0.6)
#    flush.console()
#    cat("\n\r", years[i], ": Reprojected to", substitute(crs))
#    
#    x %<>% mask(mask = border)
#    setTxtProgressBar(pb, i - 0.4)
#    flush.console()
#    cat("\n\r", years[i], ": Masked to", substitute(border))
#    
#    x %<>% crop(y = border)
#    setTxtProgressBar(pb, i - 0.2)
#    flush.console()
#    cat("\n\r", years[i], ": Cropped to extent of", substitute(border))
#
#    raster_stack %<>% stack(x)
#    setTxtProgressBar(pb, i - 0.0)
#    flush.console()
#    cat("\n\r", years[i], ": Added to raster stack")
#  
#  }
#  
#  raster_stack %<>% mean()
#  cat("\n\r", "Mean of stack calculated")
#  return(raster_stack)
#  
#  #if (!is.null(out_filename)) {
#  #  writeRaster(
#  #    raster_stack,
#  #    filename = out_filename,
#  #    overwrite = T
#  #  )
#  #}
#  # 2017-05-30 13:44 --- can't get this bit to execute after the return statement?
#  
#}


# MODIS file_query & proj-crop-mask-mean (monthly) function suite -----------------------
# A series of hierarchical functions that call on e/o w/ the following logic:
# (also see my A4 drawings of this logic)
#   
#   modfile_query(
#     for months {
#       modfile_query_slave(
#         for years {
#           ...
#         }
#       ) # e/ output is a list of files for e/ month
#     }
#   ) # 1x output is a df (from melt()) of e/ list labelled by month
#
#   proj_crop_mask_mean(
#     for months {
#       if !exists() {
#         proj_crop_mask_mean_slave(
#           for years {
#             ...
#           }
#         ) # e/ output is a raster of a month's mean LST
#       } else {
#         import raster list
#       }
#     }
#   ) # 1x output is list of rasters (for e/ month)
#
#   proj_crop_mask_mean_write(
#     proj_crop_mask_mean(
#       for months {
#         if !exists() {
#           proj_crop_mask_mean_slave(
#             for years {
#               ...
#             }
#           )
#         } else {
#           import raster_list
#         }
#       }
#     )
#     then write.raster( for months { ... } )
#   )


# Note, because of the AWFUL situation (2017-06-20 22:40) where 2017 is missing the months
# that haven't happened yet, I am forced (down the line by the raster functions below)
# to only query one month @ a time.
# That's safer too, seeing as I keep losing perfectly good executions on Feb
# again again and again because of problems with May...
# SO! That's the "recommended usage": only one month!!!


# Calls on `modfile_query_slave()` in a loop through the months given (only one, please):
# <a query fn>
modfile_query <- function(month = c(       "Feb", "Mar",
                                    "Apr", "May", "Jun",
                                    "Jul", "Aug", "Sep",
                                    "Oct", "Nov", "Dec"),
                          years        = 2000:2017,
                          slave_dir    = modwd,
                          slave_prefix = "MOD11C3.A",
                          slave_suffix = ".*.tif") {

  list_of_files <- vector("list", length = length(month))
  
  for (i in 1:length(month)) {
    list_of_files[[i]] <- modfile_query_slave( # passes all these to slave function below
      month  = month[i], 
      years  = years,
      dir    = slave_dir,
      prefix = slave_prefix,
      suffix = slave_suffix
    )
  }
  
  df_of_files        <- reshape2::melt(list_of_files)
  names(df_of_files) <- c("file_name", "year", "month")
  df_of_files$year   <- as.factor(df_of_files$year)
  for (i in 1:length(df_of_files$month)) {
    df_of_files$month[i] <- switch( # Only one month in fn input!
        month,                      # So that the month label column makes sense!
        Jan =  1, Feb =  2, Mar = 3,
        Apr =  4, May =  5, Jun = 6,
        Jul =  7, Aug =  8, Sep = 9,
        Oct = 10, Nov = 11, Dec = 12
      ) 
  }
  df_of_files$month  <- as.factor(df_of_files$month)
  # So that the raster functions don't shit themselves:
  df_of_files        <- na.exclude(df_of_files)
  
  df_of_files

}


# Calls on `base::list.files()` in a loop through the years provided:
# <a query fn>
modfile_query_slave <- function(month, years,
                                dir    = modwd,
                                prefix = "MOD11C3.A",
                                suffix = ".*.tif") {

  if (month == "Jan") { # Just in case!!!!!!!! (mostly deprecated though)
    stop("Jan has already been done!")
  }

  slave_list <- vector("list", length = length(years))
  for (i in 1:length(years)) {
      
    if (years[i] == 2000) { # To solve the Y2KJan problem....
      
      month_file_index <-switch(
        month,
        Jan =  0, Feb =  1, Mar = 2,
        Apr =  3, May =  4, Jun = 5,
        Jul =  6, Aug =  7, Sep = 9,
        Oct =  9, Nov = 10, Dec = 11
      )
      list_files <- list.files(
        path   = dir,
        pattern = paste0(prefix, years[i], suffix)
      )
      slave_list[[i]] <- list_files[month_file_index]
    
    } else {

      month_file_index <- switch(
        month,
        Jan =  1, Feb =  2, Mar = 3,
        Apr =  4, May =  5, Jun = 6,
        Jul =  7, Aug =  8, Sep = 9,
        Oct = 10, Nov = 11, Dec = 12
      )
      list_files <- list.files(
        path    = dir,
        pattern = paste0(prefix, years[i], suffix)
      )
      slave_list[[i]] <- list_files[month_file_index]
    
    }

  }
  
  names(slave_list) <- paste0("Year_", years)
  slave_list

}


# Calls on `proj_crop_mask_mean()` in a loop through months provided in the output of
# `modfile_query()` (only one, please)
# AND simultaneously writes those resulting rasters to disc:
# <a cmd fn>
proj_crop_mask_mean_write <- function(df_of_files, # the df produced by modfile_query()
                                      slave_crs    = std_CRS,
                                      slave_border = GCFR_border_buffered, #GCFR_border),
                                      slave_dir    = modwd,
                                      slave_prefix = "MOD11C3.A",
                                      slave_suffix = ".*.tif") {

  list_of_rasters <- proj_crop_mask_mean( # passes all these to slave functions below
    df_of_files  = df_of_files,
    slave_crs    = slave_crs,
    slave_border = slave_border,
    slave_dir    = slave_dir, 
    slave_prefix = slave_prefix,
    slave_suffix = slave_suffix
  )

  for (i in 1:length(levels(df_of_files$month))) {

    raster::writeRaster(
      list_of_rasters[[i]],
      filename = paste0(
        reswd, "MODIS_",
        levels(df_of_files$month)[i], "_GCFR_0.05_buffered.grd"
      ),
      overwrite = T
    )

  }

  list_of_rasters

}


# Calls on `proj_crop_mask_slave()` in a loop through months provided in the output of
# `modfile_query()` (only one, please):
# <a cmd fn>
proj_crop_mask_mean <- function(df_of_files, # the df produced by modfile_query()
                                slave_crs,
                                slave_border,
                                slave_dir,
                                slave_prefix,
                                slave_suffix) {

  pb_month <- txtProgressBar(min = 0, max = length(levels(df_of_files$month)), style = 3)
  list_of_rasters <- vector("list", length = length(levels(df_of_files$month)))

  for (i in 1:length(levels(df_of_files$month))) {

    list_of_rasters[[i]] <- proj_crop_mask_mean_slave( # passes to slave fns below
      df_of_files = df_of_files,
      slave_month = levels(df_of_files$month)[i],
      crs         = slave_crs,
      border      = slave_border,
      dir         = slave_dir, 
      prefix      = slave_prefix,
      suffix      = slave_suffix
    )
    
    setTxtProgressBar(pb_month, i)
    
  }

  names(list_of_rasters) <- levels(df_of_files$month)
  list_of_rasters

}


# Calls on a sequence of `raster()`, `projectRaster()`, `mask()`, & `crop()` in a loop
# through the years in the output of `modfile_query()` for a given month:
# <a cmd fn>
proj_crop_mask_mean_slave <- function(df_of_files, # the df produced by modfile_query()
                                      slave_month,
                                      crs,
                                      border,
                                      dir,
                                      prefix,
                                      suffix) {

  pb_year <- txtProgressBar(min = 0, max = length(levels(df_of_files$year)), style = 3)
  # Initialise an empty stack to fill w/ each yr's raster in the loop below:
  raster_stack <- stack()

  for (i in 1:length(levels(df_of_files$year))) {

    x <- df_of_files %>%
      filter(
        { year  == levels(df_of_files$year)[i] } & # only use the current yr
        { month == slave_month }                   # & the month given by the slave-owner fns
      ) %>%
      dplyr::select(file_name) %>%
      as_vector() %>%
      as.character.factor()
    
    x <- paste0(dir, x)
    x <- raster(x) # read-in as raster
    setTxtProgressBar(pb_year, i - 0.8)
    cat("\n\r", levels(df_of_files$year)[i], ": Raster made")

    x %<>% raster::projectRaster(crs = crs) # reproject to std_CRS
    setTxtProgressBar(pb_year, i - 0.6)
    flush.console()
    cat("\n\r", levels(df_of_files$year)[i], ": Reprojected to", substitute(crs))
    
    x %<>% raster::mask(mask = border) # mask...
    setTxtProgressBar(pb_year, i - 0.6)
    flush.console()
    cat("\n\r", levels(df_of_files$year)[i], ": Masked to", substitute(border))
    
    x %<>% raster::crop(y = border) # ... & crop
    setTxtProgressBar(pb_year, i - 0.4)
    flush.console()
    cat("\n\r", levels(df_of_files$year)[i], ": Cropped to extent of", substitute(border))
    
    raster_stack %<>% stack(x) # add it to the stack
    setTxtProgressBar(pb_year, i - 0.2)
    flush.console()
    cat("\n\r", levels(df_of_files$year)[i], ": Added to raster stack")
    
  }

  # Smoosh that stack down into one mean raster
  raster_out <- mean(raster_stack)
  setTxtProgressBar(pb_year, i)
  cat("\n\r", "Mean of stack calculated")
  
  raster_out

}


# Shuffling the pixels around in a raster -----------------------------------------------

# Rearranges the non-NA pixels in a raster by row & column
shuffle_raster <- function(x) {

  if (is.list(x)) { # to make it raster-list compatible (see else below for if not list)

    shuffled_x <- vector("list", length = length(x))

    for (i in 1:length(x)) {
  
      r <- as.matrix(x[[i]])
      r <- r[sample(nrow(r)), sample(ncol(r))]
      
      shuffled_x_i <- x[[i]]
      shuffled_x_i[!is.na(shuffled_x_i[])] <- r[!is.na(r)] 
      
      shuffled_x[[i]] <- shuffled_x_i
    
    }

  } else {

    r <- as.matrix(x)                                # make a raw grid of the raster vals
    r <- r[sample(nrow(r)), sample(ncol(r))]         # shuffle it col & row wise
    
    shuffled_x <- x                                  # make an identical raster to the input
    shuffled_x[!is.na(shuffled_x[])] <- r[!is.na(r)] # fill it with the shuffled vals
  
  }

  shuffled_x
}

# Applies `shuffle_raster()`, agg() and rough() to a raster however many times you want,
# saving each list of rasters into a list (s.t. `out` = a list of lists)
# (Creates HASR type; see A4 sketch notes)
many_HASRs <- function(x, how_many = 1, facts) {
  pb <- txtProgressBar(min = 0, max = how_many, style = 3)
  out <- vector("list", length = how_many)
  for (i in 1:how_many) {
    out[[i]] <- x %>%
      shuffle_raster() %>%
      custom_aggregate_loop(facts = facts) %>%
      c(x, .) %>%
      map(terrain, "roughness")
    names(out[[i]])[1] <- "1"
    setTxtProgressBar(pb, i)
  }
  names(out) <- 1:how_many
  out
}


many_SRs <- function(x, how_many = 1) {
  pb <- txtProgressBar(min = 0, max = how_many, style = 3)
  out <- vector("list", length = how_many)
  for (i in 1:how_many) {
    out[[i]] <- shuffle_raster(x)
    setTxtProgressBar(pb, i)
  }
  names(out) <- 1:how_many
  out
}

many_HSARs <- function(x, how_many = 1, facts) {
  pb <- txtProgressBar(min = 0, max = how_many, style = 3)
  out <- vector("list", length = how_many)
  for (i in 1:how_many) {
    out[[i]] <- x %>%
      custom_aggregate_loop(facts = facts) %>%
      c(x, .) %>%
      map(shuffle_raster) %>%
      map(terrain, "roughness")
    names(out[[i]])[1] <- "1"
    setTxtProgressBar(pb, i)
  }
  names(out) <- 1:how_many
  out
}

many_SARs <- function(x, how_many = 1, facts) {
  pb <- txtProgressBar(min = 0, max = how_many, style = 3)
  out <- vector("list", length = how_many)
  for (i in 1:how_many) {
    out[[i]] <- x %>%
      custom_aggregate_loop(facts = facts) %>%
      c(x, .) %>%
      map(shuffle_raster)
    names(out[[i]])[1] <- "1"
    setTxtProgressBar(pb, i)
  }
  names(out) <- 1:how_many
  out
}


# </> -----------------------------------------------------------------------------------

