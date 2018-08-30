#count_neighbouring_cells <- function(x = "Rasterlayer", state = 1) {
#    x_data <- as.matrix(x)
#    neighbours <- simecol::neighbours(
#        x = x_data,
#        state = state,
#        wdist = matrix(
#            data = c(
#                1, 1, 1,
#                1, 0, 1,
#                1, 1, 1
#            ),
#            nrow = 3,
#            ncol = 3
#        )
#    )
#    x[] <- neighbours
#    return(x)
#}
#
## Tests
#
#make_raster <- function(nrow, ncol, vals = rep(1, times = nrow * ncol)) {
#    raster(nrows = nrow, ncol = ncol, vals = vals)
#}
#
#x <- make_raster(5, 5)
#count_neighbouring_cells(x)
#x[1, 1] <- 0
#count_neighbouring_cells(x)
