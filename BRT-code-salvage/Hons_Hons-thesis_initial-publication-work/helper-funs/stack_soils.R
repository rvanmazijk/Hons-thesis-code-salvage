stack_soils <- function(regiondir,
                        region,
                        variables = c("CECSOL",
                                      "BLDFIE",
                                      "CLYPPT",
                                      "CRFVOL",
                                      "OCDENS",
                                      "PHIKCL",
                                      "SLTPPT",
                                      "SNDPPT",
                                      "AWCh1")) {
    soils <- raster::stack()
    for (variable_code in variables) {
        x <- raster::raster(glue::glue("
            {regiondir}/\\
            {region}_{variable_code}_M_250m_std_CRS_0.05.tif
        "))
        soils <- raster::stack(soils, x)
    }
    return(soils)
}
