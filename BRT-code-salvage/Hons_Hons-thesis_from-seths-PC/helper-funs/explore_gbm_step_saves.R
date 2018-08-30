explore_gbm_step_saves <- function(dir, scale, region) {

    saved_BRTs <- get_saved_BRTs(
        dir = glue("{dir}/{scale}/{region}/"),
        tol = "tol-5e-04"
    )

    model_summaries <-
        foreach(model = saved_BRTs) %do% {
            model %$% list(
                nt = gbm.call$n.trees,
                tolerance = gbm.call$tolerance,
                tc = gbm.call$tree.complexity,
                lr = gbm.call$learning.rate,
                pseudo_r2 = pseudo_r2(
                    cv.statistics$deviance.mean,
                    self.statistics$mean.null
                ),
                AUC = self.statistics$discrimination,
                cv_AUC = cv.statistics$discrimination.mean,
                deviance = self.statistics$mean.resid,
                cv_deviance = cv.statistics$deviance.mean
            )
        }
    for (i in seq_along(model_summaries)) {
        names(model_summaries)[i] <-
            names(saved_BRTs)[i]
    }
    model_summaries %<>%
        map(unlist) %>%
        as_tibble() %>%
        t() %>%
        as_tibble() %>%
        mutate(model_name = names(saved_BRTs))
    names(model_summaries) <- c(
        "nt",
        "tolerance",
        "tc",
        "lr",
        "pseudo_r2",
        "AUC",
        "cv_AUC",
        "deviance",
        "cv_deviance",
        "model_name"
    )

    return(list(
        saved_BRTs = saved_BRTs,
        model_summaries = model_summaries
    ))

}
