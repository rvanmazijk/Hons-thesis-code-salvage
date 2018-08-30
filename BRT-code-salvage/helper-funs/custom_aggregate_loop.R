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
        custom_aggregate_list[[i]] <- raster::aggregate(
            x = x,
            fact = facts[i],
            fun = method
        )
    }
    names(custom_aggregate_list) <- as.character(facts)
    return(custom_aggregate_list)
}
