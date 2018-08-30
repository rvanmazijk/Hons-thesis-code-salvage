pseudo_r2 <- function(deviance_mean, mean_null) {
    1 - (deviance_mean / mean_null)
}

# Testing

if (FALSE) {

    GCFR_good_tolerances_set_1[[1]]$tol_7e_06_tc_4_lr_1e_04 %$% {
        pseudo_r2(
            cv.statistics$deviance.mean,
            self.statistics$mean.null
        )
    }

}
