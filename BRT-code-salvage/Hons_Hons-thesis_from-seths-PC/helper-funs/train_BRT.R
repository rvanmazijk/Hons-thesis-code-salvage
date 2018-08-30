# DEPRECATED
train_BRT <- function(data, gbm_x, gbm_y,
                      tree_complexity, learning_rate){

    # * = See Elith papers + CRAN BRT/GBM guide for ecologists

    out <- dismo::gbm.step(
        data            = data,
        gbm.x           = gbm_x,
        gbm.y           = gbm_y,
        family          = "gaussian",       # Cramer & Verboom 2016
        tree.complexity = tree_complexity,
        learning.rate   = learning_rate,
        bag.fraction    = 0.7,              # *
        prev.stratify   = TRUE,             # *
        n.folds         = 10,               # *
        step.size       = 50,               # *
        silent          = FALSE,
        plot.main       = TRUE,
        plot.folds      = TRUE#,
        #max.trees = 1000,
        #tolerance = 0.0001
    )

    return(list(
        raw = out,
        interaction_depth = out$interaction.depth,
        shrinkage         = out$shrinkage,
        n_trees           = out$n.trees,
        AUC               = out$self.statistics$discrimination,
        cv_AUC            = out$cv.statistics$discrimination.mean,
        deviance          = out$self.statistics$mean.resid,
        cv_deviance       = out$cv.statistics$deviance.mean
    ))

}

train_BRT_loop <- function(data, gbm_x, gbm_y,
                           tree_complexities, learning_rates,
                           in_parallel = FALSE, n_cores){

    results <- vector(
        "list",
        length = length(tree_complexities)
    )

    if (in_parallel) {

        # TODO

    } else {

        for (i in seq_along(tree_complexities)) {

            results_for_a_given_tc <- vector(
                "list",
                length = length(learning_rates)
            )

            for (j in seq_along(learning_rates)) {

                results_for_a_given_tc_and_given_lr <- dismo::gbm.step(
                    data            = data,
                    gbm.x           = gbm_x,
                    gbm.y           = gbm_y,
                    family          = "poisson",
                    tree.complexity = tree_complexities[i],
                    learning.rate   = learning_rates[j],
                    bag.fraction    = 0.7,
                    prev.stratify   = TRUE,
                    n.folds         = 10,
                    step.size       = 50,
                    silent          = FALSE,
                    plot.main       = TRUE
                )

                results_for_a_given_tc[[j]] <-
                    results_for_a_given_tc_and_given_lr
                names(results_for_a_given_tc)[j] <- glue::glue("
                    lr_{learning_rates[j]}
                ")

            }

            results[[i]] <-
                results_for_a_given_tc
            names(results)[i] <- glue::glue("
                tc_{tree_complexities[i]}
            ")

        }

        return(results)

    }

}
