read_roughness_and_scale_analysis_RDS <- function(region = c("GCFR",
                                                             "SWAFR",
                                                             "BOTH")) {
    roughness_and_scale_analysis_object <- vector("list", length = 16)
    saved_analyses <- list.files(
        path = here::here(
            "Results",
            "roughness_across_scales"
        ),
        pattern = region,
        full.names = TRUE
    )
    var_names <- saved_analyses %>%
        str_extract(paste0(region, "_[a-zA-Z0-9]{3,}")) %>%
        str_replace_all(paste0(region, "_"), "")
    for (i in seq_along(roughness_and_scale_analysis_object)) {
        roughness_and_scale_analysis_object[[i]] <- readRDS(saved_analyses[i])
    }
    names(roughness_and_scale_analysis_object) <- var_names
    return(roughness_and_scale_analysis_object)
}
