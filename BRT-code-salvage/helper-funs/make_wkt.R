# Converts a matrix of XY-coords from within a polygon
# into a "Well Known Text" (WKT) output, for use in querying GBIF

make_wkt <- function(x = "SpatialPolygonsDataframe") {
    x <- x@polygons[[1]]@Polygons[[1]]@coords
    wkt_expr <- ""
    for (i in 1:nrow(x)) {
        # Encoding: "+" = " ", "%2C" = ",")
        wkt_expr <- glue("{wkt_expr}{x[i, 1]}+{x[i, 2]}%2C")
    }
    # Remove the trailing "%2C"
    wkt_expr %<>%
        as.character() %>%
        substr(., 0, nchar(.) - 3)
    return(wkt_expr)
}
