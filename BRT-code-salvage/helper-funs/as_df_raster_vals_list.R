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
