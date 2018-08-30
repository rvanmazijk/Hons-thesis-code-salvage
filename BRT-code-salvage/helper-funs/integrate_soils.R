integrate_soils <- function(variables = "character",
                            depths = c("sl1", "sl2", "sl3",
                                       "sl4", "sl5", "sl6", "sl7"),
                            sourcedir = "character", destdir = sourcedir,
                            do_parallel = FALSE) {

    integrate_trapezoid_whole_raster <- function(variable_code, depths,
                                                 sourcedir,
                                                 destdir = sourcedir) {

        integrate_trapezoid <- function(values, depths = c(sl1 = 0.00,
                                                           sl2 = 0.05,
                                                           sl3 = 0.15,
                                                           sl4 = 0.30,
                                                           sl5 = 0.60,
                                                           sl6 = 1.00,
                                                           sl7 = 2.00)) {
            # Defined inside here again so that `parallel::` can find it
            depth_intervals          <- depths[2:7] - depths[1:6]
            depth_interval_weights   <- depth_intervals / depths[7]
            interval_values          <- (values[1:6] + values[2:7]) / 2
            weighted_interval_values <- depth_interval_weights * interval_values
            integral                 <- sum(weighted_interval_values)
            return(integral)
        }

        variable_stack <- raster::stack()
        for (depth in depths) {
            x <- raster::raster(glue::glue("
                {sourcedir}/\\
                {variable_code}_M_{depth}_250m.tif
            "))
            variable_stack <- raster::stack(variable_stack, x)
            print(glue::glue("Added {depth} to {variable_code} stack \n\n"))
        }

        variable <- raster::calc(
            variable_stack,
            fun = integrate_trapezoid,
            progress = "text"
        )
        print(glue::glue("Integrated all depths of {variable_code} stack \n\n"))

        raster::writeRaster(
            variable,
            overwrite = TRUE,
            filename = glue::glue("
                {destdir}/\\
                {variable_code}_M_250m.tif
            ")
        )
        print(glue::glue("Written integrated {variable_code} to disc \n\n"))

    }

    if (do_parallel) {

        n_cores <- parallel::detectCores() - 1
        cluster <- parallel::makeCluster(
            n_cores,
            outfile = ""  # `print()`s and `message()`s to console
        )
        parallel::parLapply(cluster,
            X = variables,
            fun = integrate_trapezoid_whole_raster,
            depths = depths,
            sourcedir = sourcedir
        )
        parallel::stopCluster(cluster)

    } else {

        for (variable_code in variables) {
            integrate_trapezoid_whole_raster(
                variable_code,
                depths,
                sourcedir
            )
        }

    }

}
