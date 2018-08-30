# Copied and modified from <http://rfunctions.blogspot.co.za/2015/08/calculating-beta-diversity-on-grid.html>
# TODO: credit this person!

# Today I have a new function that calculates beta diversity (or phylogenetic beta diversity) for each cell on a spatial grid. This is why I called it "betagrid". The idea is simple: for each focal cell on a grid, the function calculates the mean beta diversity with each of its neighboring cells. These can be the eight (or fewer) adjacent cells or all cells within a specific radius you want. For example, something like this was used in the following paper:

#' @param gridshp grid as shapefile (or "SpatialPolygonsDataFrame" class). At the end of the post I present a version of the function which does not need this grid (and the xfeature and yfeature arguments).
#' @param comp community data matrix (species occurrence on each grid cell).
#' @param xfeature number of the feature within the grid shapefile corresponding to the longitude.
#' @param yfeature number of the feature within the grid shapefile corresponding to the latitude.
#' @param radius the radius that define the maximum distance to select neighbor cells.
#' @param phylotree (optional) phylogenetic tree ("phylo" class). It can also be a "phylo" class functional dendrogram.
#' @param phylobeta (optional) to calculate or not phylogenetic beta diversity (see "phylo.beta.pair" function in "betapart" package) instead of the usual beta diversity (see "beta.pair" function in "betapart" package). Default is FALSE.
#' @param index to calculate "sorensen" or "jaccard". Default is "sorensen".

#p_load(rgdal, rgeos, picante, betapart, CommEcol)

betagrid <- function(gridshp, comp,
                     xfeature, yfeature, radius,
                     phylotree = NULL, phylobeta = FALSE,
                     index = c("sorensen", "jaccard")) {
    data <- data.frame(
        gridshp[xfeature],
        gridshp[yfeature],
        comp
    )
    mean_turnover <- numeric(length(comp[, 1]))
    mean_nestedness <- numeric(length(comp[, 1]))
    mean_beta <- numeric(length(comp[, 1]))
    for (i in 1:length(shape[[2]])) {
        adj <- select.window(
            xf = data[i, 1],
            yf = data[i, 2],
            radius,
            xydata = data
        )[, -c(1, 2)]
        if (phylobeta == FALSE) {
            res <- beta.pair(adj, index.family = index)
        } else if (phylobeta == TRUE) {
            res <- phylo.beta.pair(adj, phylotree, index.family = index)
        }
        mean_turnover[i] <- mean(
            as.matrix(res[[1]])[
                2:length(as.matrix(res[[1]])[, 1]),
                1
            ],
            na.rm = TRUE
        )
        mean_nestedness[i] <- mean(
            as.matrix(res[[2]])[
                2:length(as.matrix(res[[2]])[, 1]),
                1
            ], na.rm = TRUE
        )
        mean_beta[i] <- mean(
            as.matrix(res[[3]])[
                2:length(as.matrix(res[[3]])[, 1]),
                1
            ], na.rm = TRUE
        )
    }
    return(data.frame(
        cell = row.names(comp),
        mean_turnover,
        mean_nestedness,
        mean_beta
    ))
}
