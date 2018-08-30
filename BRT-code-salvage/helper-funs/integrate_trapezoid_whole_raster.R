integrate_trapezoid_whole_raster <- function(variable_code, depths,
                                             sourcedir, destdir = sourcedir) {

    variable_stack <- raster::stack()
    for (depth in depths) {
        x <- raster::raster(glue::glue("
            {sourcedir}/\\
            {variable_code}_M_{depth}_250m.tif
        "))
        variable_stack <- raster::stack(variable_stack, x)
        print(glue::glue("Added {depth} to {variable_code} stack \n\n"))
    }

    variable <- raster::calc(variable_stack, fun = integrate_trapezoid)
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
