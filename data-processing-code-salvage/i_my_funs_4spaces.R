# My defined functions for my Hons thesis analyses

# Hons thesis
# Ruan van Mazijk

# created:      2017-05-07
# last edited:  2017-07-27


# Setup ------------------------------------------------------------------------

#.libPaths("/Users/ruanvanmazijk/Documents/R/R_pkgs")

if (!require(pacman)) install.packages("pacman", dependencies = TRUE)
library(pacman)
p_load(# Data-manipulation & coding
       magrittr, tidyverse, beepr, reshape2, here, glue, stringr, lubridate,
       # Stats
       quantreg, broom, ggplot2, gridExtra, visreg, ggfortify,
       # GIS
       raster, rasterVis, sp, dismo, rgdal, spatstat,
       # Taxonomy
       taxize)

# All relative to the project wd:
# TODO: remake with `here()`
datwd <- paste0(getwd(), "/Data/")
giswd <- "/Volumes/RUAN'S HDD/GIS/"
modwd <- paste0(giswd, "MOD11C3/order_501145540_GeoTiffs/")
reswd <- paste0(getwd(), "/Results/")

op <- par()

std_CRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# TODO: remove roxygen?

# plot_fun_raster() et al. -----------------------------------------------------

is_raster <- function(x) class(x) == "RasterLayer"

#' plot_fun_raster_loop
#' Takes a raster `x` and plots a raster that is a
#' calculation/fn of it: `f(x)`. `f` can be a char vector length > 1,
#' in which case loop through it to plot all of them.
#' @param x A raster
#' @param f A character vector of function names
#' @return Plots rasters of each function of \code{x}, as listed in \code{f}.
#' @example plot_fun_raster(x = MAP_GCFR, f = c("sqrt", "log", "foo"))
plot_fun_raster_loop <- function(x, f) {
    if (not(is.raster(x))) stop("x must be a raster")
    for (i in seq_along(f)) {
        x_eval <- eval(parse(text = paste0(f[i], "(x)")))
        plot(x_eval,
             zlim = c(min(x_eval[]), max(x_eval[])),
             main = paste0(f[i], " ", substitute(x)))
    }
}

#' plot_fun_raster_char
#' Same as `plot_fun_raster_loop`,
#' but no loop capability (i.e., `f` must have length = 1).
#' @param x A raster
#' @param f A character function name
#' @return Plots a raster of the function \code{f} of \code{x}.
#' @example plot_fun_raster(x = MAP_GCFR, f = "sqrt")
plot_fun_raster_char <- function(x, f) {
    if (not(is_raster(x))) stop("x must be a raster")
    x_eval <- eval(parse(text = paste0(f, "(x)")))
    plot(x_eval,
         zlim = c(min(x_eval[]), max(x_eval[])),
         main = paste0(f, " ", substitute(x)))
}

#' plot_fun_raster
#' Same as `plot_fun_raster_loop`,
#' but no loop capability (i.e., `f` must have length = 1).
#' And this version doesn't rely on char parsing,
#' but instead directly coding `f(x)`.
#' @param x A raster
#' @param f A vector of function calls
#' @return Plots a raster of each function \code{f} of \code{x}.
#' @example plot_fun_raster(x = MAP_GCFR, f = sqrt)
plot_fun_raster <- function(x, f) {
    if (not(is_raster(x))) stop("x must be a raster")
    if (not(is.function(f))) stop("f must be a function")
    f_x <- f(x)
    plot(f_x,
         zlim = c(min(f_x[]), max(f_x[])),
         main = paste0(substitute(f), " ", substitute(x)))
}


# Own funs for rasters ---------------------------------------------------------

# Convenience fns for calcs on rasters (for some raster `x`)

range_ss <- function(x) max(range(x)) - min(range(x))
calc_sd <- function(x) calc(x, fun = sd)
calc_var <- function(x) calc(x, fun = var)
calc_median <- function(x) calc(x, fun = median)
diff_mean_median <- function(x) mean(x) - calc_median(x)
diff_median_mean <- function(x) calc_median(x) - mean(x)


# plot_abs_vs_rough() et al. ---------------------------------------------------

#' plot_abs_vs_rough
#'
#' Takes some raster `x`, calculates the "roughness" raster of it `r`,
#' and plots/models the relationship between `r` and `x` values,
#' with confidence and prediction intervals,
#' and returns summary tables for these.
#'
#' @param x A raster
#'
#' @return Plots the modelled relationship (a simple \code{lm()})
#'         between roughness values (as \code{raster::terrain(., "roughness")})
#'         and absolute values of a raster \code{x},
#'         with confidence & prediction intervals.
#'
#' @return A summary table for the modelled relationship
#'         between roughness and absolute values
plot_abs_vs_rough <- function(x) {

    if (not(is_raster(x))) stop("x must be a raster")

    r <- terrain(x, "roughness")
    plot(r[] ~ x[],
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
    out_list %<>% c(., map(out_list, summary))
    names(out_list) <- c("model", "CI", "PI",
                         "model_summary", "CI_summary", "PI_summary")
    return(out_list)

}

#' mini_abs_vs_rough
#' Mini-version of `plot_abs_vs_rough`, w/o CIs, PIs, or plots.
#' @param x A raster
#' @return A summary of a linear model of roughness values
#'         (as \code{raster::terrain(., "roughness")})
#'         as a function of absolute values of a raster \code{x}.
mini_abs_vs_rough <- function(x) {  # This can be used on any 1 raster :)
    r <- terrain(x, "roughness")    # Ignore the definition-error
    lm(r[] ~ x[]) %>% summary()
}

#' as_df_mini_abs_vs_rough
#' Mini-version of `plot_abs_vs_rough`, w/o CIs, PIs, or plots,
#' and storing the `broom` summary of the model as a dataframe.
#' @param x A list of rasters
#' @param fact A numeric vector, corresponding to the factors of aggregation of
#'             the rasters in \code{x}
#' @return A dataframe that summarises the models of roughness vs fact
as_df_mini_abs_vs_rough <- function(x, fact) {
    if (not(is.list(x))) stop("x must be a list")
    df <- data.frame()
    for (i in 1:length(fact)) {      # For every factor we aggregated to...
        df_tidy <- x[[i]] %>%        # (= `length(x)` as well!).
            mini_abs_vs_rough() %>%  # ... apply the mini-fn ...
            broom::tidy() %>%        # ... use `tidy()` to get the ests. & Ps...
            slice(2) %>%
            dplyr::select(estimate, p.value)
        df_glance <- x[[i]] %>%
            mini_abs_vs_rough() %>%
            broom::glance() %>%      # ... and use `glance()` to get the adj.R2.
            dplyr::select(adj.r.squared)
        df_i <- cbind(df_tidy, df_glance, fact = fact[i])
        df %<>% rbind(df_i)          # Bind it all together, w/ a label for the
    }                                # factor, and then `rbind()` each row (=
    return(df)                       # each)
}


# Custom resampling & aggregation ----------------------------------------------

#' custom_resample
#' Convenience fn to resample a raster to a diff resolution
#' (bilinear is the default method)
#' @param x A raster
#' @param res A number, specifying the desired resolution
#' @param co_ord A CRS object's string (\code{std_CRS} is the default)
#' @param method A string, specifing the resampling method (\code{bilinear} default)
#' @return A raster
custom_resample <- function(x, res, co_ord = std_CRS, method = "bilinear") {
    # `std_CRS` and "bilinear" are defaults now! yay!
    if (not(is_raster(x))) stop("x must be a raster")
    raster(resolution = res, crs = co_ord) %>%
        crop(x) %>%
        resample(x = x, y = ., method = method)
}

#' custom_resample_loop
#' Same as `custom_resample`, but with loop for vector length > 1
#' of resolutions desired.
#' @param x A raster
#' @param res A numeric vector, specifying the desired resolution(s)
#' @param co_ord A CRS object's string (\code{std_CRS} is the default)
#' @param method A string, specifing the resampling method
#'               (\code{"bilinear"} default)
#' @return A list of the resampled rasters
custom_resample_loop <- function(x, res,
                                 co_ord = std_CRS, method = "bilinear") {
    if (not(is_raster(x))) stop("x must be a raster")
    custom_resample_list <- vector("list", length = length(res))
    for (i in seq_along(res)) {
        custom_resample_list[[i]] <- custom_resample(x = x,
                                                     res = res[i],
                                                     co_ord = co_ord,
                                                     method = method)
    }
    return(custom_resample_list)
}

#' custom_aggregate_loop
#' Same idea as `custom_resample_loop`, but with `aggregate()` instead,
#' so as to only create aligned, integer multiples of the original raster.
#' @param x A raster
#' @param facts A numeric vector, specifying the desired multiples
#'              of the \code{x}'s resolution
#' @param co_ord A CRS object's string (\code{std_CRS} is the default)
#' @param method A function name, specifing the resampling method
#'               (\code{mean} default)
#' @return A list of the aggregated rasters
custom_aggregate_loop <- function(x, facts, co_ord = std_CRS, method = mean) {
    if (not(is_raster(x))) stop("x must be a raster")
    custom_aggregate_list <- vector("list", length = length(facts))
    for (i in seq_along(facts)) {
        custom_aggregate_list[[i]] <- raster::aggregate(x = x,
                                                        fact = facts[i],
                                                        fun = method)
    }
    names(custom_aggregate_list) <- as.character(facts)
    return(custom_aggregate_list)
}


# Misc. plots ------------------------------------------------------------------

# Convenience fn for histograms when using `map()`.
hist_loop <- function(x) hist(x, breaks = 200, main = paste0(substitute(x)))

# Convenience fn for desnity kernel when using `map()`.
dens_loop <- function(x) density(x, main = paste0(substitute(x)))


# Plotting from and querying lists ---------------------------------------------

# Literally just plots the objects in a list, with main = the name in the list.
plot_from_list <- function(x) {
    if (not(is.list(x))) stop("x must be a list")
    for (i in seq_along(x)) plot(x[[i]], main = names(x[i]))
}

# Same as `plot_from_list`, but instead of 1 plot per item,
# it plots the first one, and then overlays the rest
# (e.g. lots of line graphs)!
plot_from_list_overlay <- function(x) {
    if (not(is.list(x))) stop("x must be a list")
    plot(x[[1]], ylim = c(0, 0.007))
    for (i in 2:length(x)) lines.default(x[[i]])
}

# Basically `plot_from_list`, but tailored to rasters.
plot_raster_list <- function(x) {
    if (not(is.list(x))) stop("x must be a list")
    for (i in seq_along(x)) plot(x[[i]], main = names(x)[i])
}

# Same as `plot_raster_list`, bt with an argument for plotting a polygon too!
plot_raster_list2 <- function(r, b) {
    for (i in seq_along(r)) {
        plot(r[[i]], main = names(r)[i])  # plot raster[[i]] from list
        plot(b, add = T)  # plot border
    }
}

# Plots the VALUES instead of the map of a raster....
# but otherwise basically the same as `plot_raster_list`
plot_raster_vals_list <- function(x) {
    if (not(is.list(x))) stop("x must be a list")
    for (i in seq_along(x)) plot(x[[i]][], main = names(x)[i])
}

# Stores the values of rasters (in a list `x`) in a dataframe,
# with some index `n`.
as_df_raster_vals_list <- function(x, n) {
    # for a list `x` and a vector of labels `n`
    # `n` would either be `ress` or `facts` for me
    if (not(is.list(x))) stop("x must be a list")
    x_vals <- vector("list", length = length(x))
    for (i in seq_along(x)) {
        x_vals[[i]] <- unlist(x[[i]][])
    }
    names(x_vals) <- as.character(n)
    x_vals %<>% reshape2::melt()
    names(x_vals) <- c("val", "res")  # or "fact"
    x_vals$res %<>% as.numeric()
    return(x_vals)
}

# Makes a dataframe out of the values of pts in each item in a list `x`,
# such that the list `x` is returned with `melt()`-ed dataframes in each slot,
# aot the original objs.
make_df_of_list_pts <- function(x, val) {
    if (not(is.list(x))) stop("x must be a list")
    for (i in seq_along(x)) {
        names(x[[i]]) <- seq_along(x[[i]])
        x[[i]] %<>%
            reshape2::melt(id.var = names(x)[i]) %>%
            cbind(pt_ID = rownames(.),
                  .,
                  fact = names(x)[i])
        names(x[[i]])[2] <- val
    }
    return(x)
}


# Comparing models of h() vs factor --------------------------------------------

# Convenience fn for comparing 2 lms `a` and `b`,
# using `print()`, `summary()`, and `visreg()`

cf_1_2 <- function(a, b) {

    print(substitute(a))
    print(summary(a))
    print(paste0("###########################"))
    print(substitute(b))
    print(summary(b))

    visreg(a)
    title(main = paste0(substitute(a), ":        ", a$call[2]))
    visreg(b)
    title(main = paste0(substitute(b), ":        ", b$call[2]))

    return(list(summary_a = summary(a),
                summary_b = summary(b)))

}


# Quantile regression plots ----------------------------------------------------

# `quant_plot` plots the 95%, 75%, 50% (= mediaj), 25%,
# and 5% quantile regression fits for some data `x` vs `y`.
# The fn also has nice convenient label arguments :).

# `quant_combo_plot` below is exactly the same, but ALSO fits and plots the
# lm/mean regression fit.


#' quant_plot
#'
#' Plots the 95%, 75%, 50% (= median), 25%, and 5% quantile regression fits
#' for some data-set of `x` vs `y`.
#' The fn also has nice convenient label arguments.
#'
#' @param x A numeric vector, as a column in a data frame
#'          \code{dat} with \code{y}
#' @param y A numeric vector, as a column in a data frame
#'          \code{dat} with \code{x}
#' @param dat A data-frame containing \code{x} & \code{y}
#' @param x_lab A string, describing the x-axis label for the plot
#' @param y_lab A string, describing the y-axis label for the plot
#' @param main_lab A string, describing the plot title
#' @param sub_lab    A string, describing the plot subtitle
#'
#' @return A plot
quant_plot <- function(x, y, dat,
                       x_lab = "x", y_lab = "y",
                       main_lab = "", sub_lab = "") {

    #Plot the data pts
    plot(y ~ jitter(x, 2.5),
         # (jittered bc when I used it on the `fact` data, looks better!)
         data = dat,
         ylab = y_lab,
         xlab = x_lab,
         main = main_lab,
         sub  = sub_lab,
         pch  = ".",
         col  = "grey")
    # Generatre a sequence of `x` to predict the quantiles from
    x_seq <- dat %$% seq(from = min(x, na.rm = T),
                         to = max(x, na.rm = T),
                         by = (max(x, na.rm = T) - min(x, na.rm = T)) / 1000)
    # 95% quantile fit
    mq95 <- rq(formula = log(y) ~ log(x) + I(log(x) ^ 2),
               tau = 0.95,
               data = dat)
    # Plot that fit
    y95 <- predict(mq95, list(x = x_seq))
    lines(x = x_seq, y = exp(y95),
          lty = 2, lwd = 1, col = "red")

    # And repeat for 75, 50, 25, & 5%:

    mq75 <- rq(formula = log(y) ~ log(x) + I(log(x) ^ 2),
               tau = 0.75,
               data = dat)
    y75 <- predict(mq75, list(x = x_seq))
    lines(x = x_seq, y = exp(y75),
          lty = 1, lwd = 1, col = "red")

    mq50 <- rq(formula = log(y) ~ log(x) + I(log(x) ^ 2),
               tau = 0.50,
               data = dat)
    y50 <- predict(mq50, list(x = x_seq))
    lines(x = x_seq, y = exp(y50),
          lty = 1, lwd = 2, col = "red")

    mq25 <- rq(
        log(y) ~ log(x) + I(log(x) ^ 2),
        tau = 0.25,
        data = dat
    )
    y25 <- predict(mq25, list(x = x_seq))
    lines(
        x = x_seq, y = exp(y25),
        lty = 1, lwd = 1, col = "red"
    )

    mq05 <- rq(
        log(y) ~ log(x) + I(log(x) ^ 2),
        tau = 0.05,
        data = dat
    )
    y05 <- predict(mq05, list(x = x_seq))
    lines(
        x = x_seq, y = exp(y05),
        lty = 2, lwd = 1, col = "red"
    )

}

#' quant_combo_plot
#'
#' Plots the 95%, 75%, 50% (= median), 25%, and 5% quantile regression fits,
#' and the mean regression fit, for some data-set of `x` vs `y`.
#' The fn also has nice convenient label arguments.
#'
#' @param x A numeric vector, as a column in a data frame \code{dat} with \code{y}
#' @param y A numeric vector, as a column in a data frame \code{dat} with \code{x}
#' @param dat A data-frame containing \code{x} & \code{y}
#' @param x_lab A string, describing the x-axis label for the plot
#' @param y_lab A string, describing the y-axis label for the plot
#' @param main_lab A string, describing the plot title
#' @param sub_lab  A string, describing the plot subtitle
#'
#' @return A plot
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


#' quant_combo_plotNEW
#'
#' Plots the 95%, 75%, 50% (= median), 25%, and 5% quantile regression fits,
#' and the mean regression fit, for some data-set of `x` vs `y`.
#' The fn also has nice convenient label arguments.
#'
#' @param x A numeric vector, as a column in a
#'          data frame \code{dat} with \code{y}
#' @param y A numeric vector, as a column in a
#'          data frame \code{dat} with \code{x}
#' @param dat A data-frame containing \code{x} & \code{y}
#' @param x_lab A string, describing the x-axis label for the plot
#' @param y_lab A string, describing the y-axis label for the plot
#' @param main_lab A string, describing the plot title
#' @param sub_lab A string, describing the plot subtitle
#'
#' @return A plot
quant_combo_plotNEW <- function(x, y, dat,
                             x_lab = "x", y_lab = "y",
                             main_lab = "", sub_lab = "") {

    # FIXME!

    quant_fit <- function(x = x, y = y, dat = dat, x_seq = x_seq,
                          tau = c(0.95, 0.75, 0.50, 0.25, 0.05)) {
        for (i in seq_along(tau)) {
            mq_tau <- rq(
                log(y) ~ log(x) + I(log(x) ^ 2),
                tau = tau[i],
                data = dat
            )
            y_tau <- stats::predict(mq_tau, list(x = x_seq))
            lines(
                x = x_seq, y = exp(y_tau),
                # Make 95%q & 5%q dashed (others solid),
                # And thick if median (others thin):
                lty = ifelse(tau[i] %in% c(0.95, 0.05), yes = 2, no = 1),
                lwd = ifelse(tau[i] == 0.50, yes = 2, no = 1),
                col = "red"
            )
        }
    }

    lmmod_fit <- function(x = x, y = y, dat = dat, x_seq = x_seq) {
        m <- lm(log(y) ~ log(x) + I(log(x) ^ 2), data = dat)
        fit <- stats::predict(m, list(x = x_seq), interval = "confidence")
        for (i in 1:3) {lines(
            x = x_seq, y = exp(fit[, i]),
            # Make mean solid, thick line,
            # And 95%CIs dashes, thin lines:
            lty = ifelse(i == 1, yes = 1, no = 2),
            lwd = ifelse(i == 1, yes = 2, no = 1),
            col = "black"
        )}
    }

    plot(
        y ~ jitter(x, 2.5),
        data = dat,
        ylab = y_lab,
        xlab = x_lab,
        main = main_lab,
        sub  = sub_lab,
        pch  = ".",
        col  = "grey"
    )

    x_seq <- dat %$% seq(
        min(x, na.rm = T), max(x, na.rm = T),
        (max(x, na.rm = T) - min(x, na.rm = T)) / 1000
    )

    lmmod_fit()
    quant_fit()

}


# Using bash-QGIS-GDAL commands in R -------------------------------------------

# 2017-05-22 08:48 --- This is necessary, because of a LONG-ASS reason...
# which I shall document here: `bash_gdal_algs_documentation.txt`

#' bash_gdalinfo
#' Applies the `gdal_info` algorithm to a `.HDF` file, using `bash`
#' @param x A string, specifying the the `.HDF` file name
#' @param x_dir A string, the directory \code{x} is in
#' @param gdal_dir A string, the directory the `gdal_info` is in
#'                 (QGIS GDAL on my MacBook is the default)
#' @return The `gdal_info` output describing \code{x}
bash_gdalinfo <- function(x, x_dir = getwd(),
                          gdal_dir = paste0("/Library/Frameworks/",
                                            "GDAL.framework",
                                            "/Versions/1.11/Programs/")) {
    system(paste0(
        gdal_dir, "gdalinfo ",
        "\"", x_dir, x, "\""
    ))
}

#' bash_gdalinfo_one
#' Applies the `gdal_info` algorithm to a band in a `.HDF` file, using `bash`
#' @param x A string, specifying the the `.HDF` file name
#' @param prefix A string, e.g. "HDF4_EOS:EOS_GRID:", needed for the
#'               `gdal-info` algorithm to parse properly
#' @param band A string, e.g. ":MODIS_Grid_Month_6km_LST:Emis_20",
#'             specifying the band
#' @param x_dir A string, the directory \code{x} is in
#' @param gdal_dir A string, the directory the `gdal_info` is in
#'                 (QGIS GDAL on my MacBook is the default)
#' @return The `gdal_info` output describing the specified band \code{x}
bash_gdalinfo_one <- function(x, prefix = "HDF4_EOS:EOS_GRID:",
                              band, x_dir = getwd(),
                              gdal_dir = paste0("/Library/Frameworks/",
                                                "GDAL.framework",
                                                "/Versions/1.11/Programs/")) {
    system(paste0(
        gdal_dir, "gdalinfo ",
        prefix, "\"", x_dir, x, "\"", band
    ))
}

#' bash_gdaltranslate_all
#' Applies `gdal_translate` to all bands of some `.HDF` file in `bash`,
#' using the gdal library executables that QGIS uses,
#' in order to change the file format
#' @param x A string, specifying the the `.HDF` file name
#' @param out A string, specifying the desired output file---typically a `.tif`
#' @param x_dir A string, the directory \code{x} is in
#' @param out_dir A string, the directory \code{out} is to be put in
#' @param gdal_dir A string, the directory the `gdal_info` is in
#'                 (QGIS GDAL on my MacBook is the default)
#' @return A converted `.HDF` to, e.g., a `.tif`, in \code{out_dir}
bash_gdaltranslate_all <- function(x, out, x_dir = getwd(), out_dir = getwd(),
                          gdal_dir = paste0("/Library/Frameworks/",
                                            "GDAL.framework",
                                            "/Versions/1.11/Programs/")) {
    system(paste0(
        gdal_dir, "gdal_translate -sds ",
        "\"", x_dir,  x, "\"", " ",
        "\"", out_dir, out, "\""
    ))
}

#' bash_gdaltranslate_one
#' Applies `gdal_translate` to one band of some `.HDF` file in `bash`,
#' using the gdal library executables that QGIS uses,
#' in order to change the file format
#' @param x A string, specifying the the `.HDF` file name
#' @param prefix A string, e.g. "HDF4_EOS:EOS_GRID:", needed for the
#'               `gdal-info` algorithm to parse properly
#' @param band A string, e.g. ":MODIS_Grid_Month_6km_LST:Emis_20",
#'             specifying the band
#' @param out A string, specifying the desired output file---typically a `.tif`
#' @param x_dir A string, the directory \code{x} is in
#' @param out_dir A string, the directory \code{out} is to be put in
#' @param gdal_dir A string, the directory the `gdal_info` is in
#'                                 (QGIS GDAL on my MacBook is the default)
#' @return A converted `.HDF` band to, e.g., a `.tif`, in \code{out_dir}
bash_gdaltranslate_one <- function(x, prefix = "HDF4_EOS:EOS_GRID:",
                          band, out, x_dir = getwd(), out_dir = getwd(),
                          gdal_dir = paste0("/Library/Frameworks/",
                                            "GDAL.framework",
                                            "/Versions/1.11/Programs/")) {
    system(paste0(
        gdal_dir, "gdal_translate ",
        prefix, "\"", x_dir, x, "\"", band, " ",
        "\"", out_dir, out, "\""
    ))
}

#' bash_gdaltranslate_loop
#' Applies `gdal_translate` to one band of some each `.HDF` file in a list,
#' in `bash`, by calling on `bash_gdaltranslate_one()` i
#' n a loop through that list, using the gdal library executables that
#' QGIS uses, in order to change the file format
#' @param x A character vector, specifying the the `.HDF` file names
#' @param prefix A string, e.g. "HDF4_EOS:EOS_GRID:", needed for the
#'               `gdal-info` algorithm to parse properly
#' @param band A string, e.g. ":MODIS_Grid_Month_6km_LST:Emis_20",
#'             specifying the band
#' @param out_format A string, specifying the desired outputs'
#'                   file formats---default `.tif`
#' @param x_dir A string, the directory \code{x} is in
#' @param out_dir A string, the directory \code{out} is to be put in
#' @param gdal_dir A string, the directory the `gdal_info` is in
#'                 (QGIS GDAL on my MacBook is the default)
#' @return A converted batch of `.HDF` bands to, e.g., a `.tif`,
#'         in \code{out_dir}
bash_gdaltranslate_loop <- function(x, prefix = "HDF4_EOS:EOS_GRID:",
                           band, out_format = ".tif",
                           x_dir = getwd(), out_dir = getwd(),
                           gdal_dir = paste0("/Library/Frameworks/",
                                             "GDAL.framework",
                                             "/Versions/1.11/Programs/")) {
    if (not(is.atomic(x)) | not(is.character(x))) {
        warning("x must be an atomic (i.e. 1-D) character vector!")
        warning("e.g. `order$name` (NOT the data-frame `order`)")
    }
    for (i in seq_along(x)) {bash_gdaltranslate_one(
        x = x[i], band = band,
        out = paste0(
            x[i] %>% substr(0, nchar(.) - 4),
            out_format
        ),
        x_dir = x_dir, out_dir = out_dir
    )}
}
# 2017-05-23 17:31 <nevermind> --- make progress bar work smoothly?
# the fn currently overloading console output with gdal outputs


# <DEPREC> project()-crop()-mask()-mean()-ing monthly MODIS rasters ---------------------
# This approach to dealing with the MODIS data yielded the unfortunate result of mean-ing
# the rasters as Feb + Jan + Jan + Jan + ...,
#                                Mar + Feb + Feb + Feb + ...,
# etc.
# Scrap! Deprecated!!

#proj_crop_mask_mean_deprec <- function(month = c("Jan", "Feb", "Mar",
#                                                                                                 "Apr", "May", "Jun",
#                                                                                                 "Jul", "Aug", "Sep",
#                                                                                                 "Oct", "Nov", "Dec"),
#                                                                             years, # <chr>
#                                                                             crs = std_CRS,
#                                                                             border = c(GCFR_border_buffered, GCFR_border),
#                                                                             dir = modwd,
#                                                                             prefix = "MOD11C3.A",
#                                                                             suffix = ".*.tif") {
##out_filename # <chr> e.g. `paste0(reswd, "MODIS_Jan_v2_GCFR_0.05_buffered.grd")`
#
#    f <- vector("list", length = length(years))
#    for (i in 1:length(years)) {
#        f[[i]] <- list.files(
#            path = dir,
#            pattern = paste0(prefix, years[i], suffix)
#        )[
#            switch(
#                month,
#                Jan = 1,    Feb = 2,    Mar = 3,
#                Apr = 4,    May = 5,    Jun = 6,
#                Jul = 7,    Aug = 8,    Sep = 9,
#                Oct = 10, Nov = 11, Dec = 12
#            )
#        ]
#    }
#    #return(f)
#
#    pb <- txtProgressBar(min = 0, max = length(years), style = 3)
#    raster_stack <- stack()
#    for (i in 1:length(years)) {
#
#        x <- paste0(dir, f[[i]]) %>% raster()
#        setTxtProgressBar(pb, i - 0.8)
#        cat("\n\r", years[i], ": Raster made")
#
#        x %<>% projectRaster(crs = crs)
#        setTxtProgressBar(pb, i - 0.6)
#        flush.console()
#        cat("\n\r", years[i], ": Reprojected to", substitute(crs))
#
#        x %<>% mask(mask = border)
#        setTxtProgressBar(pb, i - 0.4)
#        flush.console()
#        cat("\n\r", years[i], ": Masked to", substitute(border))
#
#        x %<>% crop(y = border)
#        setTxtProgressBar(pb, i - 0.2)
#        flush.console()
#        cat("\n\r", years[i], ": Cropped to extent of", substitute(border))
#
#        raster_stack %<>% stack(x)
#        setTxtProgressBar(pb, i - 0.0)
#        flush.console()
#        cat("\n\r", years[i], ": Added to raster stack")
#
#    }
#
#    raster_stack %<>% mean()
#    cat("\n\r", "Mean of stack calculated")
#    return(raster_stack)
#
#    #if (!is.null(out_filename)) {
#    #    writeRaster(
#    #        raster_stack,
#    #        filename = out_filename,
#    #        overwrite = T
#    #    )
#    #}
#    # 2017-05-30 13:44 --- can't get this bit to execute after the return statement?
#
#}


# MODIS file_query & proj-crop-mask-mean (monthly) function suite -----------------------
#################
# DO NOT TOUCH!!#
#################

# A series of hierarchical functions that call on e/o w/ the following logic:
# (also see my A4 drawings of this logic)
#
#     modfile_query(
#         for months {
#             modfile_query_slave(
#                 for years {
#                     ...
#                 }
#             ) # e/ output is a list of files for e/ month
#         }
#     ) # 1x output is a df (from melt()) of e/ list labelled by month
#
#     proj_crop_mask_mean(
#         for months {
#             if !exists() {
#                 proj_crop_mask_mean_slave(
#                     for years {
#                         ...
#                     }
#                 ) # e/ output is a raster of a month's mean LST
#             } else {
#                 import raster list
#             }
#         }
#     ) # 1x output is list of rasters (for e/ month)
#
#     proj_crop_mask_mean_write(
#         proj_crop_mask_mean(
#             for months {
#                 if !exists() {
#                     proj_crop_mask_mean_slave(
#                         for years {
#                             ...
#                         }
#                     )
#                 } else {
#                     import raster_list
#                 }
#             }
#         )
#         then write.raster( for months { ... } )
#     )


# Note, because of the AWFUL situation (2017-06-20 22:40) where 2017 is missing the months
# that haven't happened yet, I am forced (down the line by the raster functions below)
# to only query one month @ a time.
# That's safer too, seeing as I keep losing perfectly good executions on Feb
# again again and again because of problems with May...
# SO! That's the "recommended usage": only one month!!!


#' modfile_query
#'
#' A query fn that calls on `modfile_query_slave()` in a loop through the months given
#' (but only provide one, please! (bad design, I know)), in order to query a directory
#' for files of a certain pattern (used by default for MODIS `.tif`s here)
#'
#' @param month A string of the ONE month desired,
#'                            following the naming system in the default set provided
#'                            in the fn def below
#' @param years A numeric vector of the years desired
#' @param slave_dir A string, passed to `modfile_query_slave()`,
#'                                    specifying the directory of the (MODIS) `.tif`
#'                                    files to be searched for
#' @param slave_prefix A string, passed to `modfile_query_slave()`,
#'                                         to descrbe the first bit of the file-name to search for
#'                                         (in this case the default is "MOD11C3.A")
#' @param slave_suffix A string, passed to `modfile_query_slave()`,
#'                                         describing the file extension to search for
#'                                         (in this case the default is the regex ".*.tif")
#'
#' @return A data-frame containing the directory addresses of the
#'                 (MODIS) files as queried for, arranged by month (even though
#'                 there is only one)
modfile_query <- function(month = c("Jan", "Feb", "Mar",
                                                                        "Apr", "May", "Jun",
                                                                        "Jul", "Aug", "Sep",
                                                                        "Oct", "Nov", "Dec"),
                                                    years                = 2000:2017,
                                                    slave_dir        = modwd,
                                                    slave_prefix = "MOD11C3.A",
                                                    slave_suffix = ".*.tif") {

    list_of_files <- vector("list", length = length(month))

    for (i in 1:length(month)) {
        list_of_files[[i]] <- modfile_query_slave( # passes all these to slave function below
            month    = month[i],
            years    = years,
            dir        = slave_dir,
            prefix = slave_prefix,
            suffix = slave_suffix
        )
    }

    df_of_files                <- reshape2::melt(list_of_files)
    names(df_of_files) <- c("file_name", "year", "month")
    df_of_files$year     <- as.factor(df_of_files$year)
    for (i in 1:length(df_of_files$month)) {
        df_of_files$month[i] <- switch( # Only one month in fn input!
            month,                                            # So that the month label column makes sense!
            Jan =    1, Feb =    2, Mar = 3,
            Apr =    4, May =    5, Jun = 6,
            Jul =    7, Aug =    8, Sep = 9,
            Oct = 10, Nov = 11, Dec = 12
        )
    }
    df_of_files$month    <- as.factor(df_of_files$month)
    # So that the raster functions don't shit themselves:
    df_of_files                <- na.exclude(df_of_files)

    df_of_files

}


#' modfile_query_slave
#'
#' A query fn that calls on `base::list.files()` in a loop through the years provided
#'
#' @param month A string, as in 'modfile_query()`
#' @param years A number, as in 'modfile_query()`
#' @param dir A string, as passed to this fn by 'modfile_query()`
#' @param prefix A string, as passed to this fn by 'modfile_query()`
#' @param suffix A string, as passed to this fn by 'modfile_query()`
#'
#' @return A lsit containing the directory addresses of the
#'                 (MODIS) files as queried for
modfile_query_slave <- function(month, years,
                                                                dir        = modwd,
                                                                prefix = "MOD11C3.A",
                                                                suffix = ".*.tif") {

    #if (month == "Jan") stop("Jan has already been done!")
    # Just in case!!!!!!!! (mostly deprecated though) (for GCFR)

    slave_list <- vector("list", length = length(years))
    for (i in 1:length(years)) {

        if (years[i] == 2000) { # To solve the Y2KJan problem....

            month_file_index <- switch(
                month,
                Jan =    0, Feb =    1, Mar = 2,
                Apr =    3, May =    4, Jun = 5,
                Jul =    6, Aug =    7, Sep = 9,
                Oct =    9, Nov = 10, Dec = 11
            )
            list_files <- list.files(
                path     = dir,
                pattern = paste0(prefix, years[i], suffix)
            )
            slave_list[[i]] <- list_files[month_file_index]

        } else {

            month_file_index <- switch(
                month,
                Jan =    1, Feb =    2, Mar = 3,
                Apr =    4, May =    5, Jun = 6,
                Jul =    7, Aug =    8, Sep = 9,
                Oct = 10, Nov = 11, Dec = 12
            )
            list_files <- list.files(
                path        = dir,
                pattern = paste0(prefix, years[i], suffix)
            )
            slave_list[[i]] <- list_files[month_file_index]

        }

    }

    names(slave_list) <- paste0("Year_", years)
    slave_list

}


#' proj_crop_mask_mean_write
#'
#' A command function that calls on `proj_crop_mask_mean()` (which calls in turn on
#' `proj_crop_mask_mean_slave()`) in a loop through months provided in the output of
#' `modfile_query()` (only one, please!), to reproject rasters to a different
#' map projection, and crop and mask them to a border's confines,
#' AND simultaneously writes those resulting rasters to disc
#'
#' @param df_of_files A dataframe, produced by \code{modfile_query()},
#'                                        containing the file-names of the rasters you want to
#'                                        reproject, crop, & mask
#' @param region_name A string, either "GCFR" or "SWAFR"
#' @param slave_crs A CRS object's string (\code{std_CRS} is the default),
#'                                    to be passed to \code{proj_crop_mask_mean_slave()}
#' @param slave_border A spatial polygons dataframe, describing the border
#'                                         within which the rasters must be cropped & masked
#' @param slave_dir A string, the directory that the file-names in \code{df_of_files}
#'                                    are in, to be passed to \code{proj_crop_mask_mean_slave()}
#' @param slave_prefix A string, passed to `modfile_query_slave()`,
#'                                         to descrbe the first bit of the file-name to search for
#'                                         (in this case the default is "MOD11C3.A"),
#'                                         to be passed to \code{proj_crop_mask_mean_slave()}
#' @param slave_suffix A string, passed to `modfile_query_slave()`,
#'                                         describing the file extension to search for
#'                                         (in this case the default is the regex ".*.tif"),
#'                                         to be passed to \code{proj_crop_mask_mean_slave()}
#'
#' @return A list object, containing the reprojected, cropped, masked rasters
#' @return Also write the rasters in that list to disc
proj_crop_mask_mean_write <- function(df_of_files,
                                                                            region_name    = c("GCFR", "SWAFR"),
                                                                            slave_crs        = std_CRS,
                                                                            slave_border = GCFR_border_buffered,
                                                                            slave_dir        = modwd,
                                                                            slave_prefix = "MOD11C3.A",
                                                                            slave_suffix = ".*.tif") {

    list_of_rasters <- proj_crop_mask_mean( # passes all these to slave functions below
        df_of_files    = df_of_files,
        slave_crs        = slave_crs,
        slave_border = slave_border,
        slave_dir        = slave_dir,
        slave_prefix = slave_prefix,
        slave_suffix = slave_suffix
    )

    for (i in 1:length(levels(df_of_files$month))) {
        list_of_rasters[[i]] %>%
            raster::writeRaster(
                filename = paste0(
                    reswd, "MODIS_",
                    levels(df_of_files$month)[i], "_", region_name, "_0.05_buffered.grd"
                ),
                overwrite = T
            )
    }

    list_of_rasters

}


#' proj_crop_mask_mean
#'
#' A command function that calls on `proj_crop_mask_slave()` in a loop through months
#' provided in the output of `modfile_query()` (only one, please) to reproject rasters
#' to a different map projection, and crop and mask them to a border's confines.
#'
#' @param df_of_files A dataframe, produced by \code{modfile_query()},
#'                                        containing the file-names of the rasters you want to
#'                                        reproject, crop, & mask
#' @param slave_crs A CRS object's string (\code{std_CRS} is the default),
#'                                    to be passed to \code{proj_crop_mask_mean_slave()}
#' @param slave_border A spatial polygons dataframe, describing the border
#'                                         within which the rasters must be cropped & masked,
#'                                         to be passed to \code{proj_crop_mask_mean_slave()}
#' @param slave_dir A string, the directory that the file-names in \code{df_of_files}
#'                                    are in, to be passed to \code{proj_crop_mask_mean_slave()}
#' @param slave_prefix A string, passed to `modfile_query_slave()`,
#'                                         to descrbe the first bit of the file-name to search for
#'                                         (in this case the default is "MOD11C3.A"),
#'                                         to be passed to \code{proj_crop_mask_mean_slave()}
#' @param slave_suffix A string, passed to `modfile_query_slave()`,
#'                                         describing the file extension to search for
#'                                         (in this case the default is the regex ".*.tif"),
#'                                         to be passed to \code{proj_crop_mask_mean_slave()}
#'
#' @return A list object, containing the reprojected, cropped, masked rasters
proj_crop_mask_mean <- function(df_of_files,
                                                                slave_crs, slave_border,
                                                                slave_dir, slave_prefix, slave_suffix) {

    pb_month <- txtProgressBar(min = 0, max = length(levels(df_of_files$month)), style = 3)
    list_of_rasters <- vector("list", length = length(levels(df_of_files$month)))

    for (i in 1:length(levels(df_of_files$month))) {

        list_of_rasters[[i]] <- proj_crop_mask_mean_slave( # passes to slave fns below
            df_of_files = df_of_files,
            slave_month = levels(df_of_files$month)[i],
            crs                 = slave_crs,
            border            = slave_border,
            dir                 = slave_dir,
            prefix            = slave_prefix,
            suffix            = slave_suffix
        )

        setTxtProgressBar(pb_month, i)

    }

    names(list_of_rasters) <- levels(df_of_files$month)
    list_of_rasters

}


#' proj_crop_mask_mean_slave
#'
#' A command function that calls on a sequence of `raster()`, `projectRaster()`, `mask()`,
#' & `crop()` in a loop through the years in the output of `modfile_query()`,
#' for a given month
#'
#' @param df_of_files A dataframe, produced by \code{modfile_query()},
#'                                        containing the file-names of the rasters you want to
#'                                        reproject, crop, & mask
#' @param slave_month A string, the month to filter \code{df_of_files} to
#' @param crs A CRS object's string (\code{std_CRS} is the default)
#' @param border A spatial polygons dataframe, describing the border
#'                             within which the rasters must be cropped & masked,
#'                             to be passed to \code{raster::mask()} & \code{raster::crop()}
#' @param dir A string, the directory that the file-names in \code{df_of_files} are in
#' @param prefix A string, passed to `modfile_query_slave()`,
#'                             to descrbe the first bit of the file-name to search for
#'                             (in this case the default is "MOD11C3.A")
#' @param suffix A string, passed to `modfile_query_slave()`,
#'                             describing the file extension to search for
#'                             (in this case the default is the regex ".*.tif")
#'
#' @return A raster, reprojected, masked, & cropped
proj_crop_mask_mean_slave <- function(df_of_files, slave_month,
                                                                            crs, border,
                                                                            dir, prefix, suffix) {

    pb_year <- txtProgressBar(min = 0, max = length(levels(df_of_files$year)), style = 3)
    # Initialise an empty stack to fill w/ each yr's raster in the loop below:
    raster_stack <- stack()

    for (i in 1:length(levels(df_of_files$year))) {

        x <- df_of_files %>%
            filter(
                (year    == levels(df_of_files$year)[i]) && # only use the current yr
                    (month == slave_month)                                        # & the month given by the slave-owner fns
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


# Shuffling the pixels around in a raster --------------------------------------

#' shuffle_raster
#' Rearranges the non-NA pixels in a raster by row & column
#' @param x A raster
#' @return A raster, "shuffled"
shuffle_raster <- function(x) {
    if (is.list(x)) {
        # to make it raster-list compatible (see else below for if not list)
        shuffled_x <- vector("list", length = length(x))
        for (i in seq_along(x)) {
            r <- as.matrix(x[[i]])
            r <- r[sample(nrow(r)), sample(ncol(r))]
            shuffled_x_i <- x[[i]]
            shuffled_x_i[!is.na(shuffled_x_i[])] <- r[!is.na(r)]
            shuffled_x[[i]] <- shuffled_x_i
        }
    } else {
        r <- as.matrix(x)  # make a raw grid of the raster vals
        r <- r[sample(nrow(r)), sample(ncol(r))]  # shuffle it col & row wise
        shuffled_x <- x  # make an identical raster to the input
        shuffled_x[!is.na(shuffled_x[])] <- r[!is.na(r)]
        # (fills it with the shuffled vals)
    }
    return(shuffled_x)
}

#' many_HASRs
#' Applies `shuffle_raster()`, then agg(),
#' and then rough() to a raster however many times you want,
#' saving each list of rasters into a list (s.t. `out` = a list of lists)
#' (Creates HASR type rasters; see Hons thesis A4 sketch notes)
#' @param x A raster
#' @param how_many A number
#' @param facts A numeric vector, the factors of aggregation
#' @return A list of lists of rasters
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
    return(out)
}

#' many_SRs
#' Applies `shuffle_raster()` to a raster however many times you want,
#' saving each raster into a list
#' @param x A raster
#' @param how_many A number
#' @return A list of rasters
many_SRs <- function(x, how_many = 1) {
    pb <- txtProgressBar(min = 0, max = how_many, style = 3)
    out <- vector("list", length = how_many)
    for (i in 1:how_many) {
        out[[i]] <- shuffle_raster(x)
        setTxtProgressBar(pb, i)
    }
    names(out) <- 1:how_many
    return(out)
}

#' many_HSARs
#' Applies agg(), then `shuffle_raster()`,
#' and then rough() to a raster however many times you want,
#' saving each list of rasters into a list (s.t. `out` = a list of lists)
#' @param x A raster
#' @param how_many A number
#' @param facts A numeric vector, the factors of aggregation
#' @return A list of lists of rasters
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
    return(out)
}

#' many_SARs
#' Applies agg() and then `shuffle_raster()` to a raster
#' however many times you want, saving each list of rasters into a list
#' (s.t. `out` = a list of lists)
#' @param x A raster
#' @param how_many A number
#' @param facts A numeric vector, the factors of aggregation
#' @return A list of lists of rasters
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
    return(out)
}

# TODO: "BRT is black box ss bc of ONLY partial dependence plots, aot each interaction term separately?" is what i thought... that's where my confusion came in (though I admit, as Elith says, and you say, the PDP are great!!!!) plus see this: https://arxiv.org/pdf/1309.6392.pdf

# TODO: un-set.seed() before running do_prelim_anal()


# Functions for plotting PCA rough vs fact stuff -------------------------------

#' plot_PC_loadings
#'
#' Plots the rotation values of a set of pseudo-continuous variables from a PCA,
#' and fits a quadratic curve using `lm()` for the rotation ~ variable,
#' for both PC1 & PC2
#'
#' @param x A list of class "prcomp" (i.e. A PCA model output from
#'          \code{prcomp()})
#' @param facts A numeric vector, the factors of aggregation
#'              (which for my purposes is the variable in the PCA),
#'              usually 1:15
#'
#' @return Plots of rotation ~ variable for PC1 & PC2, side-by-side
plot_PC_loadings <- function(x, facts) {
    if ((class(x) != "prcomp") || (not(is.list(x)))) {
        stop("x must be a list of class prcomp")
    }
    op <- par()
    par(mfrow = c(1, 2))
    for (i in 1:2) {
        plot(
            x$rotation[, i],
            ylim = c(-max(abs(x$rotation[, i])), max(abs(x$rotation[, i]))),
            main = glue::glue("PC{i} eigenvectors (= variable loadings)"),
            ylab = "Rotation",
            xlab = "Aggregation factor",
            xaxt = "n"
        )
        axis(1, labels = c(1, facts), at = c(1, facts))
        abline(h = 0, lty = 2)
        fit <- lm(x$rotation[, i] ~ c(1, facts) + I(c(1, facts) ^ 2))
        newdat <- data.frame(fact = seq(
            1, (length(facts) + 1),
            length.out = (length(facts) + 1)
        ))
        newdat$fit <- predict(fit, newdat, interval = "confidence")[, 1]
        newdat$lwr <- predict(fit, newdat, interval = "confidence")[, 2]
        newdat$upr <- predict(fit, newdat, interval = "confidence")[, 3]
        with(newdat, lines(x = fact, y = upr))
        with(newdat, lines(x = fact, y = fit))
        with(newdat, lines(x = fact, y = lwr))
    }
    par(op)
}


#' plot_PC_geospace
#'
#' 2017-06-30 17:57 TODO --- write descr.
#' (Designed to handle a PCA of GCFR & SWAFR together)
#'
#' @param x A dataframe, see @example for form
#' @param pc1 A numeric vector, namely \code{foo$x[, 1]}
#'            where foo is the \code{prcomp()} object
#' @param pc2 A numeric vector, namely \code{foo$x[, 2]}
#'            where foo is the \code{prcomp()} object
#' @param region_a A string, non-optional
#' @param region_b A string, optional
#' @param border_a A string, non-optional
#' @param border_b A string, optional
#'
#' @return 2017-06-30 17:57 TODO --- write descr.
#'
#' @example
#' > # x should look like this (colnames NB, dimensions are just for e.g.):
#' > x
#' # A tibble: 1,781 x 19
#' #    lon     lat pt_ID region_name      `1`      `2`      `3`      ...
#' #  <dbl>   <dbl> <int>      <fctr>    <dbl>    <dbl>    <dbl>      ...
#' # 17.225 -29.575   713        GCFR 31.36417 32.53892 53.99228      ...
#' # 17.225 -29.475   105        GCFR 44.10604 48.44220 57.58888      ...
#' #    ...     ...   ...         ...      ...      ...      ...      ...
plot_PC_geospace <- function(x, pc1, pc2,
                             region_a, region_b = NULL,
                             border_a, border_b = NULL,
                             pt_size = 2) {

    arrange_PC_geospace_child <- function(x_child, pc1, pc2, border, region,
                                          scale_limits_pc1, scale_limits_pc2,
                                          pt_size = 2) {
        p <- ggplot(data = x_child) +
            geom_polygon(
                data = border, aes(x = long, y = lat, group = group),
                colour = "black", fill = NA
            ) +
            theme_classic() +
            theme(legend.position = "top")
        p_pc1 <- p +
            geom_point(data = x_child, aes(x = lon, y = lat, col = pc1), size = pt_size) +
            scale_color_distiller(
                name = "PC1",
                palette = "Spectral", limits = scale_limits_pc1
            )
        p_pc2 <- p +
            geom_point(data = x_child, aes(x = lon, y = lat, col = pc2), size = pt_size) +
            scale_color_distiller(
                name = "PC2",
                palette = "Spectral", limits = scale_limits_pc2
            )
        return(arrangeGrob(
            p_pc1, p_pc2,
            widths = c(15, 15), heights = 10
        ))

        # FIXME: tidy!

        #p <- ggplot(data = x_child, aes(x = lon, y = lat)) +
        #    geom_polygon(
        #        data = border, aes(x = long, y = lat, group = group),
        #        colour = "black", fill = NA
        #    ) +
        #    theme_classic() +
        #    theme(legend.position = "top")
        #p_pc1 <- p +
        #    geom_point(data = x_child, aes(col = pc1), size = pt_size) +
        #    scale_color_distiller(
        #        name = "PC1",
        #        palette = "Spectral", limits = scale_limits_pc1
        #    )
        #p_pc2 <- p +
        #    geom_point(data = x_child, aes(col = pc2), size = pt_size) +
        #    scale_color_distiller(
        #        name = "PC2",
        #        palette = "Spectral", limits = scale_limits_pc2
        #    )
        #return(arrangeGrob(
        #    p_pc1, p_pc2,
        #    widths = c(15, 15), heights = 10
        #))
    }

    if (not(is.null(region_b)) && not(is.null(border_b))) {
        x_a <- x %>%
            cbind(pc1 = pc1, pc2 = pc2) %>%
            filter(region_name == region_a)
        x_b <- x %>%
            cbind(pc1 = pc1, pc2 = pc2) %>%
            filter(region_name == region_b)
        p_a <- arrange_PC_geospace_child(
            x_child = x_a,
            pc1 = pc1, pc2 = pc2,
            region = region_a, border = border_a,
            scale_limits_pc1 = range(pc1),
            scale_limits_pc2 = range(pc2)
        )
        p_b <- arrange_PC_geospace_child(
            x_child = x_b,
            pc1 = pc1, pc2 = pc2,
            region = region_b, border = border_b,
            scale_limits_pc1 = range(pc1),
            scale_limits_pc2 = range(pc2)
        )
        plot(arrangeGrob(
            p_a, p_b,
            widths = 15, heights = c(10, 10)
        ))
    } else if (is.null(region_b) && is.null(border_b)) {
        x %<>% cbind(pc1 = pc1, pc2 = pc2)
        plot(arrange_PC_geospace_child(
            x_child = x,
            pc1 = pc1, pc2 = pc2,
            region = region_a, border = border_a,
            scale_limits_pc1 = range(pc1),
            scale_limits_pc2 = range(pc2),
            pt_size = 2
        ))
    } else {
        stop("You either need 2 regions w/ 2 borders, or 1 region w/ 1 border!")
    }

}


do_PCAandkmeans <- function(region_a_dir,
                            region_a_name,
                            border_a,
                            region_b_dir = NULL,
                            region_b_name = NULL,
                            border_b = NULL,
                            var_name,
                            do_kmeans = FALSE) {

    import_region_dfs <- function(region_dir, region_name, var_name) {
        read.csv(paste0(region_dir))[, -1] %>%
        cbind(region_name = rep(region_name, times = 1000)) %>%
        as_tibble() %>%
        spread_(
            key_col = "fact",
            value_col = paste0("rough_agg_", var_name)
        ) %>%
        na.omit()
    }

    if (not(is.null(region_b_dir)) && not(is.null(region_b_name))) {
        all_regions <- rbind(
            import_region_dfs(
                region_dir = region_a_dir, region_name = region_a_name,
                var_name = var_name
            ),
            import_region_dfs(
                region_dir = region_b_dir, region_name = region_b_name,
                var_name = var_name
            )
        )
    } else if (is.null(region_b_dir) && is.null(region_b_name)) {
        all_regions <- import_region_dfs(
            region_dir = region_a_dir, region_name = region_a_name,
            var_name = var_name
        )
    }

    all_regions_pca <- prcomp(all_regions[, -c(1:4)])

    plot(all_regions_pca)

    plot(
        all_regions_pca$x,
        col = all_regions$region_name,
        ylim = c(
            -max(abs(all_regions_pca$x[, 2])),
             max(abs(all_regions_pca$x[, 2]))
        ),
        xlim = c(
            -max(abs(all_regions_pca$x[, 1])),
             max(abs(all_regions_pca$x[, 1]))
        )
    )
    abline(h = 0, lty = 2)
    abline(v = 0, lty = 2)

    plot_PC_loadings(all_regions_pca, 2:15)

    plot_PC_geospace(
        all_regions,
        region_a = region_a_name, region_b = region_b_name,
        border_a = border_a,            border_b = border_b,
        pc1 = all_regions_pca$x[, 1],
        pc2 = all_regions_pca$x[, 2]
    )

    can_do_kmeans <- {
        not(is.null(region_b_dir)) &&
        not(is.null(region_b_name)) &&
        do_kmeans
    }
    if (can_do_kmeans) {
        fit <- kmeans(all_regions[, -c(1:4)], centers = 2)
        cluster::clusplot(
            all_regions[, -c(1:4)],
            fit$cluster,
            color = TRUE,
            shade = TRUE,
            labels = 2,
            lines = 0
        )
    }

}


do_env_vars_PCA <- function(vars = "list",
                            var_names = c("elev", "MAP", "MA_LST"),
                            border) {

    do_PCA <- function(vars) {
        # vars is a stack
        vars_pca <- vars %>%
            as.matrix() %>%
            na.omit() %>%
            prcomp()
        plot(vars_pca)
        plot(autoplot(    # Thanks, `ggfortify::`!
            vars_pca,
            scale = 0, col = "grey",
            loadings = TRUE, loadings.label = TRUE, loadings.label.size = 5
        ))
        return(vars_pca)
    }

    vars <- c(vars)
    names(vars) <- var_names
    vars %<>%
        map(mask, border) %>%
        map(crop, border) %$%
        stack(elev, MAP, MA_LST)
    names(vars) <- var_names

    rough_vars <- stack()
    for (i in 1:nlayers(vars)) {
        x <- terrain(vars[[i]], "roughness")
        rough_vars %<>% stack(x)
    }
    names(rough_vars) <- paste0("rough_", var_names)

    vars_pca       <- do_PCA(vars)
    rough_vars_pca <- do_PCA(rough_vars)
    combo_pca      <- do_PCA(stack(vars, rough_vars))

    out <- list(vars_pca, rough_vars_pca, combo_pca)
    names(out) <- c("vars_pca", "rough_vars_pca", "combo_pca")
    return(out)

}


# GBIF schlep ------------------------------------------------------------------

genus_only <- function(x) str_extract(x, "^[a-zA-Z]*")
binom_only <- function(x) str_extract(x, "^[a-zA-Z]* [a-zA-Z]*")

make_wkt <- function(x = "SpatialPolygonsDataframe") {
    # Converts a matrix of XY-coords from within a polygon
    # into a "Well Known Text" (WKT) output, for use in querying GBIF
    x <- x@polygons[[1]]@Polygons[[1]]@coords  # dig in the S4 stuff
    wkt_expr <- ""
    for (i in 1:nrow(x)) {
        # glue() together all the x's and y's recursively
        # ("+" = " ", "%2C" = ",")
        wkt_expr <- glue("{wkt_expr}{x[i, 1]}+{x[i, 2]}%2C")
    }
    wkt_expr %<>%
        as.character() %>%
        substr(., 0, nchar(.) - 3) # rm the trailing "%2C"
    return(wkt_expr)
}

tidy_gbif <- function(x = "data.frame") {
    # Takes a GBIF derived dataframe and tidies it up for me, like so:
    x %>%
        dplyr::filter(
            phylum == "Tracheophyta",  # Just in case
            taxonrank %in% c("SPECIES", "SUBSPECIES", "FORM", "VARIETY")
            # (so that all infrasp. ranks can be merged into their spp.)
        ) %>%
        dplyr::select(
            family, genus, species, infraspecificepithet,
            scientificname, taxonrank,
            # Note: use `species` mostly, as does not contain any infrasp. info!
            # TODO: think about `species` vs `scientificname`
            decimallatitude, decimallongitude,
            coordinateuncertaintyinmeters, coordinateprecision
        ) %>%
        dplyr::arrange(species)
}

disjunct <- function(x, y) {
    union <- union(x, y)
    intersect <- intersect(x, y)
    disjunc <- setdiff(union, intersect)
    membership <- vector(length = length(disjunc))
    for (i in seq_along(disjunc)) {
        membership[i] <-
            if (disjunc[i] %in% x)      as.character(substitute(x))
            else if (disjunc[i] %in% y) as.character(substitute(y))
            else                        NA
    }
    return(tibble(disjunc, membership))
}

summarise_spp_sets <- function(flora_GBIF_tidy) {
    flora_GBIF_tidy %$%
        list(
            species                   = species,
            infraspecificepithet      = infraspecificepithet,
            scientificname            = scientificname,
            # Add these:
            scientificname_species    = binom_only(scientificname),
            species_subsp             = paste(species, infraspecificepithet) %>%
                                        str_replace_all(" NA", "")
        ) %>%
        map(unique) %>%
        map(sort) %$%
        list(
            # Same as before:
            species                   = species,
            infraspecificepithet      = infraspecificepithet,
            scientificname            = scientificname,
            scientificname_species    = scientificname_species,
            species_subsp             = species_subsp,
            # And add these:
            intersect_species_sciname = intersect(species,
                                                  scientificname_species),
            union_species_sciname     = union(species,
                                              scientificname_species),
            disjunct_species_sciname  = disjunct(species,
                                                 scientificname_species)
        ) %$%
        list(
            # Same as before:
            species                   = species,
            infraspecificepithet      = infraspecificepithet,
            scientificname            = scientificname,
            scientificname_species    = scientificname_species,
            species_subsp             = species_subsp,
            intersect_species_sciname = intersect_species_sciname,
            union_species_sciname     = union_species_sciname,
            disjunct_species_sciname  = disjunct_species_sciname,
            # And add these:
            disj_spp_sciname_genera   = disjunct_species_sciname$disjunc %>%
                                        genus_only() %>%
                                        unique() %>%
                                        sort()
        )
}

estimate_hr <- function(end, start, flora, how_many) {
    # takes `end` and `start` markers (to time how long a function run takes)
    # and presents the difference * no. of runs intended in future,
    # in terms of hours :)
    # (used in test_tnrs() and test_gnr() below)
    duration <- end - start
    duration %>%
        as.period() %>%
        as.numeric() %>%
        divide_by(how_many) %>%
        multiply_by(length(as.character(unique(flora$species)))) %>%
        divide_by(60) %>%
        divide_by(60) %>%
        as.numeric()
}

# TODO: write doc for fns below

test_tnrs <- function(how_many = 1, flora) {
    start <- Sys.time()
    tnrs_test = tnrs(
        query = as.character(unique(flora$species))[1:how_many],
        source = "iPlant_TNRS"
    )
    end <- Sys.time()
    est_GCFR_runtime_hr <- estimate_hr(end, start, flora, how_many)
    est_SWAFR_runtime_hr <- estimate_hr(end, start, flora, how_many)
    return(list(
        df = tnrs_test,
        est_GCFR_runtime_hr = est_GCFR_runtime_hr,
        est_SWAFR_runtime_hr = est_SWAFR_runtime_hr
    ))
}

test_gnr <- function(how_many = 1,
                     flora,
                     data_source_ids = 12) {  # (12 = EOL)
    start <- Sys.time()
    gnr_test = gnr_resolve(
        names = as.character(unique(flora$species))[1:how_many],
        data_source_ids = data_source_ids
    )
    end <- Sys.time()
    est_GCFR_runtime_hr <- estimate_hr(end, start, flora, how_many)
    est_SWAFR_runtime_hr <- estimate_hr(end, start, flora, how_many)
    return(list(
        df = gnr_test,
        est_GCFR_runtime_hr = est_GCFR_runtime_hr,
        est_SWAFR_runtime_hr = est_SWAFR_runtime_hr
    ))
}

do_tnrs <- function(spp_vect,
                    unique_index_range = NULL) {
    spp_vect_unique <- spp_vect %>%
        unique() %>%
        as.character()
    if (not(is.null(unique_index_range))) {
        spp_vect_unique <- spp_vect_unique[unique_index_range]
    }
    out_tnrs <- as_tibble(tnrs(
        query = spp_vect_unique,
        source = "iPlant_TNRS"
    ))
    return(out_tnrs)
}

do_gnr <- function(spp_vect,
                   unique_index_range = NULL,
                   data_source_ids = 12) {  # (12 = EOL)
    spp_vect_unique <- spp_vect %>%
        unique() %>%
        as.character()
    if (not(is.null(unique_index_range))) {
        spp_vect_unique <- spp_vect_unique[unique_index_range]
    }
    out_gnr <- as_tibble(gnr_resolve(
        names = spp_vect_unique,
        data_source_ids = data_source_ids
    ))
    return(out_gnr)
}

is_consistent <- function(tnrs_df,
                          gnr_df,
                          index = 1:nrow(tnrs_df)) {

    # Preamble -----------------------------------------------------------------

    # If A = B, B = C, and C = D, then one needn't test that A = C or A = D,
    # as it is logically consequential!
    # And because A = D (tnrs_df$submittedname == gnr_df$user_supplied_name)
    # is a given for my work, I don't even need to test for B = C!
    # So ALL I need to check is that the submitted name is matched in both
    # TNRS and GNR


    # Check if names are matched by TRNS ---------------------------------------

    tnrs_cond <- tnrs_df %$% {submittedname[index] == matchedname[index]}


    # Check if names are matched by GNR ----------------------------------------

    # Organise the gnr_df
    gnr_df_indexed <- gnr_df %>%
        filter(user_supplied_name %in% tnrs_df$submittedname[index]) %>%
        transmute(
            user_supplied_name = user_supplied_name,
            matched_name = matched_name,
            binom_only_matched_name = binom_only(matched_name), # rms authority
            data_source_title = data_source_title,
            score = score
        ) %>%
        as_tibble()

    # Test it against the submitted names
    gnr_cond <- vector(length = length(tnrs_df$submittedname[index]))
    for (i in seq_along(tnrs_df$submittedname[index])) {
        submitted_name_i_is_in_gnr <- tnrs_df$submittedname[index][i] %in%
            gnr_df_indexed$binom_only_matched_name
        if (submitted_name_i_is_in_gnr) {
            gnr_cond[i] <- TRUE
        } else {
            gnr_cond[i] <- FALSE
        }
    }


    # Combine the results of these 2 tests -------------------------------------
    consistent <- tnrs_cond & gnr_cond
    conds <- tibble(
        tnrs_cond  = tnrs_cond,
        gnr_cond   = gnr_cond,
        consistent = consistent
    )


    # Fetch the names that GNR recommends --------------------------------------
    # (whether gnr_cond is true or not)
    # (in order to diagnose TRUE/FALSE validity in the outputs by hand :))
    # (used by print_gnr_matches() below!)

    gnr_matched_names <- vector(
        "list",
        length = length(tnrs_df$submittedname[index])
    )
    for (i in seq_along(tnrs_df$submittedname[index])) {
        gnr_matched_names[[i]] <- gnr_df_indexed %>%
            filter(user_supplied_name == tnrs_df$submittedname[index][i]) %>%
            dplyr::select(binom_only_matched_name) %>%
            as_vector()
    }

    name_summary <- tibble(
        submitted_name    = tnrs_df$submittedname[index],
        tnrs_matched_name = tnrs_df$matchedname[index],
        gnr_matched_names  = gnr_matched_names
    )


    # Return -------------------------------------------------------------------

    return(list(
        gnr_df = gnr_df_indexed,
        conds = as_tibble(cbind(conds, name_summary))
    ))

}

flag_inconsistent_names <- function(flora, name_range = 1:100) {
    # A meta-function function to do a lot at once and flag stuff!
    tnrs_actual <- do_tnrs(flora$species, name_range)
    gnr_actual <- do_gnr(flora$species, name_range)
    consistency_check <- is_consistent(tnrs_actual, gnr_actual)
    flagged <- consistency_check$conds %>%
        filter(not(consistent))
    return(list(
        tnrs_actual              = tnrs_actual,
        gnr_actual               = gnr_actual,
        consistency_check_gnr_df = consistency_check$gnr_df,
        consistency_check_conds  = consistency_check$conds,
        flagged                  = flagged
    ))
}

print_gnr_matches <- function(x) {
    # Utilises the gnr_matched_names list column in the outputs of
    # flag_inconsistent_names() (i.e. is_consistent()).
    print(glue("[GNR match?]: [list of matched names]"))
    for (i in 1:nrow(x)) {
        names <- ""
        for (j in 1:length(x$gnr_matched_names[[i]])) {
            names <- glue("{names}{x$gnr_matched_names[[i]][j]}, ")
        }
        print(glue("{x$gnr_cond[i]}: {names}"))
    }
}

rbind_flagged_spp <- function(x) {
    # Takes a vector of flag_inconsistent_names() output tibbles
    # and rbind()s them all together :)
    rbind_x <- tibble()
    for (i in seq_along(x)) {
        rbind_x <- x[i]$flagged %>%
            rbind(rbind_x, .) %>%
            as_tibble()
    }
    return(rbind_x)
}


# Richness & rasters -----------------------------------------------------------

make_flora <- function(region_border = GCFR_border,
                       region_raster = agg_MAP_GCFR$`5`,
                       how_many_spp = 25,
                       how_many_occ = 100) {

    # Creates a specified amount of random occurence points, for each "species"
    # for the amount of species specified by running `populate_a_species()`
    # in a loop of that length.
    # All within the confines of a SpatialPolygonsDataframe provided.

    populate_a_species <- function(region_border = region_border,
                                   how_many_occ = how_many_occ,
                                   species_name = "sp1") {
        # Creates a specified amount of random occurence points,
        # thus representing a single "species", within the confines of a
        # SpatialPolygonsDataframe provided.
        a_species_occs <- spsample(
            region_border,
            how_many_occ,
            type = "random"
        )
        id_df <- data.frame(
            ID = seq_along(a_species_occs),
            species = species_name
        )
        a_species_occs %<>%
            SpatialPointsDataFrame(id_df) %>%
            na.omit() %>%
            spTransform(crs(region_border))
        return(a_species_occs)
    }

    # Run `populate_a_species()` in a loop.
    flora <- vector("list", length = how_many_spp)
    for (i in 1:how_many_spp) {
        flora[[i]] <- populate_a_species(
            region_border,
            how_many_occ,
            # Name sp column identically within a species
            species_name = paste0("sp", i)
        )
        # Name list's slots accordingly
        names(flora)[i] <- glue("sp{i}")
    }

    # Add all 1:how_many species' SpatialPointsDataframe together
    # (i.e. bind them into 1 object (e.g. a + b)).
    temp <- flora[[1]]
    for (i in 2:how_many_spp) {
        temp <- temp + flora[[i]]  # TODO: same as `temp %<>% add(flora[[i]])`?
    }
    flora <- temp

    return(flora)

}

get_cells <- function(flora_occs = "SpatialPointsDataframe",
                      region_raster = "RasterLayer") {
    # Get the cell-ID of the cell that a point falls in on the given raster,
    # so that this can be used as an INDEX in `calc_richness()`.
    cells <- region_raster %>%
        extract(SpatialPoints(flora_occs), cellnumbers = TRUE) %>%
        as.data.frame() %>%
        dplyr::select(cells)
    flora_occs@data %<>% cbind(cells)  # S4! f*** yeah!
    return(flora_occs)
}

calc_richness <- function(flora_occs = "SpatialPointsDataframe",
                          region_raster = "RasterLayer") {
    # Returns a raster & frequency table describing the number of unique
    # species occurences (i.e. the species richness) in cells on a raster,
    # when looking at a SpatialPointsDataframe that has cell-IDs based on
    # the same raster as that provided to this function.
    richness_raster <- region_raster
    richness_raster[] <- 0
    # (See count_occs() [DEPREC] for the logic of the frequency table below.)
    richness_by_cell <- tapply(
        X = flora_occs$species,
        INDEX = flora_occs$cells,
        FUN = function(x) length(unique(x))
    )
    richness_raster[as.numeric(names(richness_by_cell))] <- richness_by_cell
    return(list(
        freq_tab = richness_by_cell,
        raster = richness_raster
    ))
}

QDSpolydf2raster <- function(qds_shp = "SpatialPolygonsDataFrame",
                             region_border = "SpatialPolygonsDataFrame",
                             crs = std_CRS,
                             resolution = 0.25) {
    qds_shp@polygons %>%
        SpatialPolygons(proj4string = CRS(crs)) %>%
        rasterize(
            raster(resolution = 0.25, crs = crs) %>%
            crop(region_border) %>%
            mask(region_border)
        )
}

QDSpolydf2pixels <- function(qds_shp = "SpatialPolygonsDataFrame",
                             region_border = "SpatialPolygonsDataFrame",
                             crs = std_CRS,
                             resolution = 0.25) {
    qds_shp@data %>%
        dplyr::select(lon, lat) %>%
        map(as.character) %>%
        map(as.numeric) %>%
        data.frame() %>%
        SpatialPoints(proj4string = CRS(crs)) %>%
        SpatialPixelsDataFrame(
            data = data.frame(
                qdgc = qds_shp@data$qdgc,
                blank = rep(0, length(qds_shp@data$qdgc))
            ),
            proj4string = CRS(crs)
        )
}

plot_pixelgrid <- function(x = "SpatialPixelsDataframe",
                           var = "blank",
                           add = FALSE) {
    plot(
        x[var],
        col = "white",
        border = "black",
        what = "image",
        scale = FALSE,
        add = add
    )
}

calc_richness_QDS <- function(flora_occs = "SpatialPointsDataframe",
                              qds_shp = "SpatialPolygonsDataFrame") {
    # Returns a raster & frequency table describing the number of unique
    # species occurences (i.e. the species richness) in qdgc cells,
    # when looking at a SpatialPointsDataframe that has cell-IDs based on
    # the same raster as that provided to this function.
    richness_raster <- region_raster
    richness_raster[] <- 0
    # (See count_occs() [DEPREC] for the logic of the frequency table below.)
    richness_by_cell <- tapply(
        X = flora_occs$species,
        INDEX = flora_occs$cells,
        FUN = function(x) length(unique(x))
    )
    richness_raster[as.numeric(names(richness_by_cell))] <- richness_by_cell
    return(list(
        freq_tab = richness_by_cell,
        raster = richness_raster
    ))
}

make_richness_raster <- function(flora_occs, region_raster, crs) {
    # For use on GBIF occ dfs (that I have cleaned using taxize::)
    flora_sp_pts_df <-
        SpatialPointsDataFrame(
            coords = flora_occs[, c("decimallongitude", "decimallatitude")],
            data = flora_occs[, c("species")],
            proj4string = crs(crs)
        ) %>%
        get_cells(region_raster)
    raster <- calc_richness(flora_sp_pts_df, region_raster)
    return(list(
        flora_sp_pts_df = flora_sp_pts_df,
        freq_tab = raster$freq_tab,
        raster = raster$raster
    ))
}


# Junk (DEPREC) -------------------------------------------------------------------------

if (FALSE) { #<DEPREC>

    do_myPCA_old <- function(region_dir, region_name, var_name) {

        import_region_dfs <- function(region_dir, region_name, var_name) {
            region <- read.csv(paste0(region_dir))[, -1] %>%
                cbind(region_name = rep(region_name, times = 1000)) %>%
                as_tibble() %>%
                spread_(key_col = "fact", value_col = paste0("rough_agg_", var_name)) %>%
                na.omit()
            region
        }

        region <- import_region_dfs(
            region_dir = region_dir, region_name = region_name,
            var_name = var_name
        )

        region_pca <- prcomp(region[, -c(1:4)])

        plot(region_pca)

        plot(
            region_pca$x,
            col = region_pca$region_name,
            ylim = c(-max(abs(both_regions_pca$x[, 2])), max(abs(both_regions_pca$x[, 2]))),
            xlim = c(-max(abs(both_regions_pca$x[, 1])), max(abs(both_regions_pca$x[, 1])))
        )
        abline(h = 0, lty = 2)
        abline(v = 0, lty = 2)

        plot_PC_loadings(region_pca, 2:15)

    }

    do_myPCA_two_regions_old <- function(region_a_dir, region_a_name, region_b_dir, region_b_name,
                                                                             var_name) {

        import_region_dfs <- function(region_dir, region_name, var_name) {
            region <- read.csv(paste0(region_dir))[, -1] %>%
                cbind(region_name = rep(region_name, times = 1000)) %>%
                as_tibble() %>%
                spread_(key_col = "fact", value_col = paste0("rough_agg_", var_name)) %>%
                na.omit()
        }

        both_regions <- rbind(
            import_region_dfs(
                region_dir = region_a_dir, region_name = region_a_name,
                var_name = var_name
            ),
            import_region_dfs(
                region_dir = region_b_dir, region_name = region_b_name,
                var_name = var_name
            )
        )

        both_regions_pca <- prcomp(both_regions[, -c(1:4)])

        plot(both_regions_pca)

        plot(
            both_regions_pca$x,
            col = both_regions$region_name,
            ylim = c(-max(abs(both_regions_pca$x[, 2])), max(abs(both_regions_pca$x[, 2]))),
            xlim = c(-max(abs(both_regions_pca$x[, 1])), max(abs(both_regions_pca$x[, 1])))
        )
        abline(h = 0, lty = 2)
        abline(v = 0, lty = 2)

        plot_PC_loadings(both_regions_pca, 2:15)

    }

}


# </> -----------------------------------------------------------------------------------

