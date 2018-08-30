# [...] ------------------------------------------------------------------------

# "Fractals are typically not self-similar"
# https://www.youtube.com/watch?v=gB9n2gHsHN4&ab_channel=3Blue1Brown

calc_dimensionality <- function(x_poly, x_raster) {
    
    # x is a Spatial Polygons Dataframe
    
    area <- function(x) {
        # x is a raster
        e <- x %>%
            extent() %>%
            as.matrix() %>%
            as.data.frame()
        (e["x", "max"] - e["x", "min"]) * (e["y", "max"] - e["y", "min"])
    }
    
    x_large <- x_poly %>%
        as("SpatialLines") %>%
        rasterize(x_raster)
    
    x_small <- x_large %>%
        extend(., sqrt(2)*extent(.)) %>%
        aggregate(2)
    
    out <- list(
        D = log2(
            ncell(x_large[!is.na(x_large)]) /
                ncell(x_small[!is.na(x_small)])
        ),
        x_small   = x_small,
        x_large   = x_large,
        a_x_small = area(x_small),
        a_x_large = area(x_large),
        n_x_small = ncell(x_small[!is.na(x_small)]),
        n_x_large = ncell(x_large[!is.na(x_large)])
    )
    out
    
}

D_SWAFR <- calc_dimensionality(x_poly = SWAFR_border, x_raster = agg_MAP_SWAFR$`1`)
D_SWAFR

D_GCFR <- calc_dimensionality(x_poly = GCFR_border, x_raster = agg_MAP_GCFR$`1`)
D_GCFR


calc_dimensionality_multi <- function(x_poly   = NULL,
                                      x_raster = NULL,
                                      x_zoom   = NULL) {
    
    # x is a Spatial Polygons Dataframe
    
    zoom_out <- function(x) {
        # x is a raster
        out <- vector("list", length = 15)
        for (i in 2:15) {
            out[[i]] <- x %>%
                extend(., sqrt(i)*extent(.)) %>%
                aggregate(i)
        }
        out[[1]] <- x
        names(out) <- 1:15
        out
    }
    
    if (is.null(x_zoom)) {
        x_zoom <- x_poly %>%
            as("SpatialLines") %>%
            rasterize(x_raster) %>%
            zoom_out()
    }
    
    n_cell_x_zoom <- c()
    for (i in 1:15) {
        n_cell_x_zoom[i] <- ncell(x_zoom[[i]][!is.na(x_zoom[[i]])])
    }
    n_cell_x_zoom %<>% cbind(15:1)
    
    plot(log(n_cell_x_zoom[, 1]) ~ log(n_cell_x_zoom[, 2]))
    fit <- lm(log(n_cell_x_zoom[, 1]) ~ log(n_cell_x_zoom[, 2]))
    
    out <- list(
        x_zoom = x_zoom,
        n_cell_x_zoom = n_cell_x_zoom,
        fit = fit
    )
    out
    
}

D_multi_SWAFR <- calc_dimensionality_multi(
    x_poly = SWAFR_border,
    x_raster = agg_MAP_SWAFR$`1`
)

D_multi_SWAFR2 <- calc_dimensionality_multi(
    x_zoom = D_multi_SWAFR$x_zoom
)

# <DEPREC
2 ^ (D_multi_SWAFR$fit$coefficients)
# The slope shows the dimensionality of this border! (~= 1.85)
# />







# </> -----------------------------------------------------------------------------------

zoom(foo, ext = drawExtent())