download_soil <- function(variables = "character",
                          depths = c("sl1", "sl2", "sl3",
                                     "sl4", "sl5", "sl6", "sl7"),
                          blocks = "list of numeric atomics each of length 4",
                          destdir = "character",
                          geoserver_URL = NULL) {

    # `blocks` e.g.
    # > blocks <- list(
    # >     c(<min-lon>, <max-lon>, <min-lat>, <max-lat>),
    # >     ...
    # > )

    download_block <- function(geoserver_URL,
                               variable_code, depth, block,
                               destdir, block_id = i) {

        CoverageId <- glue("{variable_code}_M_{depth}_250m")
        Long <- glue("Long({block[1]},{block[2]})")
        Lat <- glue("Lat({block[3]},{block[4]})")

        destfile <- glue("
            {destdir}/\\
            {CoverageId}_0{block_id}.tiff
        ")
        query <- glue("
            {geoserver_URL}\\
            &CoverageId={CoverageId}\\
            &subset={Long}\\
            &subset={Lat}
        ")

        download.file(
            url = query,
            destfile = destfile
        )
        print(glue("Downloaded block 0{block_id} for {CoverageId} \n\n"))

    }

    if (is.null(geoserver_URL)) {
        geoserver_URL <- glue("
            http://85.214.241.121:8080/geoserver/ows?service=WCS\\
            &version=2.0.1&request=GetCoverage
        ")
    }

    for (variable_code in variables) {
        for (depth in depths) {
            for (i in seq_along(blocks)) {
                download_block(
                    geoserver_URL,
                    variable_code,
                    depth,
                    block = blocks[[i]],
                    destdir
                )
            }
        }
    }

}
