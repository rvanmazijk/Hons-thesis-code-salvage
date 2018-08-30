make_dict <- function(spp_set, syns) {
    syns_dict <- vector("list", length = length(spp_set))
    for (i in seq_along(syns_dict)) {
        syns_dict[[i]] <- list(
            raw_name = spp_set[[i]],
            syns = syns[[i]]
        )
    }
    return(syns_dict)
}
