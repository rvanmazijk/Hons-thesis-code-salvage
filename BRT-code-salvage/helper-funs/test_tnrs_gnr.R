test_tnrs <- function(how_many = 1, flora) {
    start <- Sys.time()
    tnrs_test = tnrs(
        query = as.character(unique(flora$species))[1:how_many],
        source = "iPlant_TNRS"
    )
    end <- Sys.time()
    est_GCFR_runtime_hr <- estimate_hr(end, start, flora, how_many)
    est_SWAFR_runtime_hr <- estimate_hr(end, start, flora, how_many)
    return(list(
        df = tnrs_test,
        est_GCFR_runtime_hr = est_GCFR_runtime_hr,
        est_SWAFR_runtime_hr = est_SWAFR_runtime_hr
    ))
}

test_gnr <- function(how_many = 1,
                     flora,
                     data_source_ids = 12) {  # (12 = EOL)
    start <- Sys.time()
    gnr_test = gnr_resolve(
        names = as.character(unique(flora$species))[1:how_many],
        data_source_ids = data_source_ids
    )
    end <- Sys.time()
    est_GCFR_runtime_hr  <- estimate_hr(end, start, flora, how_many)
    est_SWAFR_runtime_hr <- estimate_hr(end, start, flora, how_many)
    return(list(
        df = gnr_test,
        est_GCFR_runtime_hr = est_GCFR_runtime_hr,
        est_SWAFR_runtime_hr = est_SWAFR_runtime_hr
    ))
}
