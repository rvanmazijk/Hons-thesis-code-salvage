# Copied & modified from within the source code of
# [`biovars`](https://github.com/cran/dismo/blob/master/R/biovars.R) (Hijmans)
# R. van Mazijk

biovars_window <- function(x)  {
    lng <- length(x)
    x <- c(x,  x[1:3])
    m <- matrix(ncol = 3, nrow = lng)
    for (i in 1:3) {
        m[, i] <- x[i:(lng + i - 1)]
    }
    apply(m, MARGIN = 1, FUN = sum)
}


#' @param ppt RasterLayer, 12 layers, each containing monthly avg rainfalls
#' @return RasterLayer of ppt in wettest quarter
biovars_PWQ <- function(ppt = "RasterLayer") {
    # P16. Precipitation of Wettest Quarter
    ppt_matrix <- as.matrix(ppt)
    wet_matrix <- t(apply(ppt_matrix, 1, biovars_window))
    PWQ_matrix <- apply(wet_matrix, 1, max)
    # Get the skeleton of the input raster
    PWQ <- ppt[[1]]
    # Fill it with the PWQ data
    PWQ[] <- PWQ_matrix
    return(PWQ)
}

#' @param ppt RasterLayer, 12 layers, each containing monthly avg rainfalls
#' @return RasterLayer of ppt in driest quarter
biovars_PDQ <- function(ppt = "RasterLayer") {
    # P17. Precipitation of Driest Quarter
    ppt_matrix <- as.matrix(ppt)
    dry_matrix <- t(apply(ppt_matrix, 1, biovars_window))
    PDQ_matrix <- apply(dry_matrix, 1, min)
    # Get the skeleton of the input raster
    PDQ <- ppt[[1]]
    # Fill it with the PWQ data
    PDQ[] <- PDQ_matrix
    return(PDQ)
}

#' @param ppt RasterLayer, 12 layers, each containing monthly avg rainfalls
#' @return RasterLayer of ppt in driest quarter
biovars_PCV <- function(ppt = "RasterLayer") {
    # P15. Precipitation Seasonality(Coefficient of Variation)
    # the "1 +" is to avoid strange CVs for areas
    # where mean rainfaill is < 1)
    ppt_matrix <- as.matrix(ppt)
    # + 1 to avoid strange CVs where avg ppt is < 1
    PCV_matrix <- apply(ppt_matrix + 1, 1, cv)
    # Get the skeleton of the input raster
    PCV <- ppt[[1]]
    # Fill it with the PWQ data
    PCV[] <- PCV_matrix
    return(PCV)
}

#' @param ppt RasterLayer, 12 layers, each containing monthly avg temperatures
#' @return RasterLayer of tmp in warmest quarter
biovars_TWQ <- function(tmp = "RasterLayer") {
    # P10 Mean Temperature of Warmest Quarter
    tmp_matrix <- as.matrix(tmp)
    TWQ_matrix <- apply(tmp_matrix, 1, max)
    # Get the skeleton of the input raster
    TWQ <- tmp[[1]]
    # Fill it with the PWQ data
    TWQ[] <- TWQ_matrix
    return(TWQ)
}

#' @param ppt RasterLayer, 12 layers, each containing monthly avg temperatures
#' @return RasterLayer of tmp in coldest quarter
biovars_TCQ <- function(tmp = "RasterLayer") {
    # P11 Mean Temperature of Coldest Quarter
    tmp_matrix <- as.matrix(tmp)
    TCQ_matrix <- apply(tmp_matrix, 1, min)
    # Get the skeleton of the input raster
    TCQ <- tmp[[1]]
    # Fill it with the PWQ data
    TCQ[] <- TCQ_matrix
    return(TCQ)
}


# Tests

if (FALSE) {
    GCFR_PWQ_box <- biovars_PWQ(GCFR_monthly_CHIRPS_means)
    GCFR_PWQ <- biovars_PWQ(crop(GCFR_monthly_CHIRPS_means, GCFR_border))
    par(mfrow = c(1, 2))
    plot(GCFR_PWQ_box, zlim = c(0, 650))
    plot(GCFR_PWQ, zlim = c(0, 650))
    par(mfrow = c(1, 1))
}
