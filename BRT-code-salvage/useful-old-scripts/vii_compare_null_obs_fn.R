# TODO
# - [x] implement writing t test & f test to disc
# - [ ] implement writing plots to disc
# - [ ] implement writing model objects to disc
# - [ ] Z-scores?
# - [ ] Test for sig diff betw null & obs rough vs fact slopes w/ 1 lm() w/ interaction term?
#     - form of this model? ll_qu? linear?

#' @param obs_dir A string, the existing dir of the outputs from \code{do_prelim_anal()}
#' @param null_dir A string, the existing dir of the outputs from \code{rand_raster_null()}
#' @param compare_dir A string, the existing dir IN WHICH TO STORE the outputs of this fn
#' @param var_name A string
#' @param region_name A string, one of "GCFR" & "SWAFR"
compare_null_obs <- function(obs_dir,
                             null_dir,
                             compare_dir,
                             var_name,
                             region_name = c("GCFR", "SWAFR")) {


  # Read in data at the 1000 random points ----------------------------------------------

  obs_dir <- paste0(obs_dir, "dfs_", var_name, "_", region_name, "/")
  obs_dir <- paste0(obs_dir, list.files(obs_dir, pattern = "extracted"))
  obs_roughness <- as_tibble(read.csv(paste0(obs_dir)))

  null_dir <- paste0(null_dir, "dfs_", var_name, "_", region_name, "/")
  null_dir <- paste0(null_dir, list.files(null_dir, pattern = "extracted"))
  null_roughness <- as_tibble(read.csv(paste0(null_dir)))

  obs_null_join <- left_join(
    obs_roughness, null_roughness,
    by = c("X", "lon", "lat", "pt_ID", "fact")
  )
  names(obs_null_join)[6:7] <- c(
    paste0("rough_agg_", var_name),
    paste0("rough_shuff_agg_", var_name)
  )
  obs_null_join %<>% gather("case", "roughness", 6:7)


  # T-test & F-test for equal var ---------------------------------------------------------
  # (of raw roughnesses between obs & null)

  obs_null_join %$% {

    t.test(
      roughness[case == paste0("rough_agg_", var_name)],
      roughness[case == paste0("rough_shuff_agg_", var_name)]
    ) %>%
      broom::tidy() %>%
      write.csv(paste0(
        compare_dir,
        "obs_null_ttest_", var_name, "_", region_name, ".csv"
      ))

    var.test(
      roughness[case == paste0("rough_agg_", var_name)],
      roughness[case == paste0("rough_shuff_agg_", var_name)]
    ) %>%
      broom::tidy() %>%
      write.csv(paste0(
        compare_dir,
        "obs_null_ftest_", var_name, "_", region_name, ".csv"
      ))

  }


  # Plotting obs & null rough vs fact -----------------------------------------------------

  # rough vs fact, for null & obs (plain, abs):
  p <- ggplot(data = obs_null_join %>% filter(fact <= 15)) + # filter() just in case!
    geom_boxplot(aes(x = as.factor(fact), y = roughness, col = case)) +
    geom_smooth(
      aes(x = fact, y = roughness, col = case),
      method = "lm"
    ) +
    xlab("fact")
  ggsave(p, filename = paste0(compare_dir, "rough_vs_fact_nullandobs_plain.pdf"))

  # rank(rough, by = all (i.e. out of 30k)) vs fact for null & obs:
  p <- obs_null_join %>%
    mutate(roughness_rank_overall = rank(roughness)) %>%
    filter(fact <= 15) %>%
    ggplot() +
    geom_boxplot(aes(x = as.factor(fact), y = roughness_rank_overall, col = case)) +
    geom_smooth(
      aes(x = fact, y = roughness_rank_overall, col = case),
      method = "lm"
    ) +
    xlab("fact")
  ggsave(p, filename = paste0(compare_dir, "rough_vs_fact_nullandobs_byall.pdf"))

  # rank(rough, by = case (i.e. out of 2k (1k x 2 cases))) vs fact, for null & obs:
  p <-
    as_tibble(transform(
      obs_null_join,
      roughness_rank_fact = ave(
        obs_null_join$roughness, obs_null_join$fact,
        FUN = function(x) rank(x, ties.method = "first")
      )
    )) %>%
    filter(fact <= 15) %>%
    ggplot() +
    geom_boxplot(aes(x = as.factor(fact), y = roughness_rank_fact, col = case)) +
    geom_smooth(
      aes(x = fact, y = roughness_rank_fact, col = case),
      method = "lm"
    ) +
    xlab("fact")
  ggsave(p, filename = paste0(compare_dir, "rough_vs_fact_nullandobs_bycase.pdf"))

  # rank(rough, by = fact (i.e. out of 15k (1k x 15 facts))) vs fact, for null & obs:
  p <-
    as_tibble(transform(
      obs_null_join,
      roughness_rank_case = ave(
        obs_null_join$roughness, obs_null_join$case,
        FUN = function(x) rank(x, ties.method = "first")
      )
    )) %>%
    filter(fact <= 15) %>%
    ggplot() +
    geom_boxplot(aes(x = as.factor(fact), y = roughness_rank_case, col = case)) +
    geom_smooth(
      aes(x = fact, y = roughness_rank_case, col = case),
      method = "lm"
    ) +
    xlab("fact")
  ggsave(p, filename = paste0(compare_dir, "rough_vs_fact_nullandobs_byfact.pdf"))


  # PCA of rough vs fact, labelled by obs & null ------------------------------------------

  null_roughness$pt_ID %<>% paste0("_shuff")

  obs_null_join_PCA <-
    obs_roughness[obs_roughness$fact <= 15, 4:6] %>%
    rbind(null_roughness[, 4:6]) %>%
    spread_(key_col = "fact", value_col = paste0("rough_agg_", var_name)) %>%
    na.omit()
  case <- vector(length = length(obs_null_join_PCA$pt_ID))
  obs_null_join_PCA <- cbind(case, obs_null_join_PCA)
  for (i in 1:length(obs_null_join_PCA$pt_ID)) {
    foo_char <- as.character(obs_null_join_PCA$pt_ID[i])
    if (stringr::str_detect(foo_char, "shuff")) {
      obs_null_join_PCA$case[i] <- "null"
    } else {
      obs_null_join_PCA$case[i] <- "obs"
    }
  }

  foo_pca <- prcomp(obs_null_join_PCA[, -c(1, 2)])

  pdf(paste0(compare_dir, "PCA.pdf"), width = 10, height = 10)
  palette(c("blue", "magenta"))
  plot(foo_pca$x[, c(1, 2)], col = as.factor(obs_null_join_PCA$case))
  abline(h = 0, lty = 2)
  abline(v = 0, lty = 2)
  palette("default")
  dev.off()


  # </> -----------------------------------------------------------------------------------


}


# </> -----------------------------------------------------------------------------------
