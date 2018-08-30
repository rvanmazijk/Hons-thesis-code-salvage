force_positive_loadings <- function(PC1 = NULL, PC2 = NULL) {

    if (not(is.null(PC1))) {

        # Make all/most PC1-loadings positive

        if (any(PC1 < 0)) {
            if (all(PC1 < 0 )) {
                PC1 %<>% multiply_by(-1)
            } else {
                warning("One or more, but not all, PC1-loadings are negative")
            }
        }

    }

    if (not(is.null(PC2))) {

        # Make PC2 loadings start negative and end positive

        if (all(PC2[1:3] > 0)) {
            PC2 %<>% multiply_by(-1)
        }

    }

    return(list(
        PC1 = PC1,
        PC2 = PC2
    ))

}
