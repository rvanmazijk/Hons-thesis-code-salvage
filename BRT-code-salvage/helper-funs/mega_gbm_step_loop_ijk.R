mega_gbm_step_loop_ijk <- function(data, gbm.x, gbm.y,
                                   family = "gaussian",
                                   tolerances,
                                   tree_complexities,
                                   learning_rates,
                                   out_dir,
                                   do_tolerances_in_parallel = FALSE) {

    gbm_step_loop_jk <- function(tolerance,
                                 # ^ singular!
                                 # has to be first for `parallel::`
                                 data, gbm.x, gbm.y,
                                 family,
                                 tree_complexities,
                                 learning_rates,
                                 out_dir) {
        for (j in tree_complexities) {
            for (k in learning_rates) {
                message(glue::glue("
                                   Starting\\
                                   tolerance = {tolerance},\\
                                   tree complexity = {j},\\
                                   learning rate = {k}
                                   "))
                gbm_step_results_ijk <- dismo::gbm.step(
                    data             = data,
                    gbm.x            = gbm.x,
                    gbm.y            = gbm.y,
                    family           = family,
                    tolerance.method = "auto",
                    tolerance        = tolerance,
                    tree.complexity  = j,
                    learning.rate    = k,
                    bag.fraction     = 0.75,   # default
                    prev.stratify    = TRUE,
                    n.folds          = 10,
                    silent           = FALSE,
                    plot.main        = TRUE,
                    n.trees          = 1,      # starting nt
                    step.size        = 1,      # nt increment
                    max.trees        = 10000   # (default) (esp. for set_3)
                )
                # Note:
                # n.trees & step.size are *very* small,
                # because of the drastically small tolerance I seem to need
                # (see notebook)
                saveRDS(
                    object = gbm_step_results_ijk,
                    file = paste(sep = "/",
                                 out_dir,
                                 glue::glue("
                                gbm.step-output_\\
                                tol-{tolerance}_\\
                                tc-{j}_\\
                                lr-{k}\\
                                .rds
                            ")
                    )
                )
            }
        }
    }

    if (do_tolerances_in_parallel) {
        # Note: doing tol in parallel is largely useless now,
        # as I am (for predictor set 3) only really feeding 1 tol.

        cluster <- makeCluster(detectCores() - 1, outfile = "")
        parLapply(cluster,
                  as.list(tolerances),
                  fun = gbm_step_loop_jk,
                  data = data,
                  gbm.x = gbm.x,
                  gbm.y = gbm.y,
                  family = family,
                  tree_complexities = tree_complexities,
                  learning_rates = learning_rates,
                  out_dir = out_dir
        )
        stopCluster(cluster)

    } else {

        for (i in tolerances) {
            gbm_step_loop_jk(
                data = data,
                gbm.x = gbm.x,
                gbm.y = gbm.y,
                family = family,
                tolerance = i,
                tree_complexities = tree_complexities,
                learning_rates = learning_rates,
                out_dir = out_dir
            )
        }

    }

}
