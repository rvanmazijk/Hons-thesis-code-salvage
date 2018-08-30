merge_soils <- function(variables = "character",
                        depths = c("sl1", "sl2", "sl3",
                                   "sl4", "sl5", "sl6", "sl7"),
                        sourcedir = "character", destdir = sourcedir,
                        do_parallel = FALSE) {

    merge_blocks <- function(depth, variable_code,
                             sourcedir, destdir = sourcedir) {

        get_blocks <- function(sourcedir, CoverageId) {
            blocks_file_names <- list.files(
                sourcedir,
                pattern = glue::glue("{CoverageId}_0*"),
                full.names = TRUE
            )
            blocks <- vector("list", length = length(blocks_file_names))
            for (i in seq_along(blocks)) {
                blocks[[i]] <- raster::raster(blocks_file_names[i])
            }
            return(blocks)
        }

        CoverageId <- glue::glue("{variable_code}_M_{depth}_250m")
        blocks <- get_blocks(sourcedir, CoverageId)

        layer <- raster::merge(
            blocks[[1]],
            blocks[[2]],
            blocks[[3]],
            blocks[[4]],
            blocks[[5]],
            blocks[[6]]
        )
        print(glue::glue("Merged all blocks for {CoverageId} \n\n"))

        raster::writeRaster(
            layer,
            overwrite = TRUE,
            filename = glue::glue("
                {destdir}/\\
                {CoverageId}.tiff
            ")
        )
        print(glue::glue("Written {CoverageId} layer to disc \n\n"))

    }

    if (do_parallel) {

        for (variable_code in variables) {
            n_cores <- parallel::detectCores() - 1
            cluster <- parallel::makeCluster(
                n_cores,
                outfile = ""  # `print()`s and `message()`s to console
            )
            parallel::parLapply(cluster,
                X = depths,
                fun = merge_blocks,
                variable_code = variable_code,
                sourcedir = sourcedir
            )
            parallel::stopCluster(cluster)
        }

    } else {

        for (variable_code in variables) {
            for (depth in depths) {
                merge_blocks(
                    depth,
                    variable_code,
                    sourcedir
                )
            }
        }

    }

}
