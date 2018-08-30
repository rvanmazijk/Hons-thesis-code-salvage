# ...
# Hons thesis
# R. van Mazijk

#'
#' Runs `do_prelim_anal()`, `do_rand_null()`, and `compare_null_obs()` on raster provided
#' @param x A raster
#' @param need_do_prelim_anal A logical
#' @param need_do_rand_null A logical
#' @param border A spatial polygons data frame
#' @param border_buffer A spatial polygons data frame
#' @param facts A numeric vector
#' @param var_name A string
#' @param region_name A string, one of "GCFR" & "SWAFR"
#' @param how_many_SARs A number
#' @param how_many_rand_pts A number
#' @param x_tester A raster, from which to sample `rand_pts`
#' @param obs_dir A string, the existing dir for the outputs of \code{do_prelim_anal()}
#' @param null_dir A string, the existing dir for the outputs of \code{rand_raster_null()}
#' @param compare_dir A string, the existing dir for the outputs of \code{compare_null_obs()}
prelim_rand_compare <- function(x = NULL,
                                need_do_prelim_anal = FALSE,
                                need_do_rand_null = FALSE,
                                border = NULL,
                                border_buffer = NULL,
                                facts = 2:15,
                                var_name = NULL,
                                region_name = c("GCFR", "SWAFR"),
                                how_many_SARs = 1,
                                how_many_rand_pts = 1000,
                                x_tester = NULL,
                                obs_dir,
                                null_dir,
                                compare_dir,
                                plot_format = c("PDF", "TIFF"),
                                save_grd = TRUE) {

  if (need_do_prelim_anal) {
    message("Performing `do_prelim_anal()` ... \n")
    do_prelim_anal(
      x                 = x,
      border            = border,
      border_buffer     = border_buffer,
      facts             = facts,
      var_name          = var_name,
      region_name       = region_name,
      reswd_temp        = obs_dir,
      x_tester          = x_tester,
      how_many_rand_pts = 1000,
      save_grd          = TRUE
    )
  }

  if (need_do_rand_null) {
    message("Performing `do_rand_null()` ... \n")
    do_rand_null(
      x                 = x,
      how_many_SARs     = 1,  # note: SAR <- raster %>% agg() %>% shuff()
      facts             = facts,
      border            = border,
      border_buffer     = border_buffer,
      var_name          = var_name,
      region_name       = region_name,
      reswd_temp        = null_dir,
      x_tester          = x_tester,
      how_many_rand_pts = 1000,
      save_grd          = TRUE
    )
  }

  message("Performing `compare_null_obs()` ... \n")
  compare_null_obs(
    obs_dir     = obs_dir,
    null_dir    = null_dir,
    compare_dir = compare_dir,
    var_name    = var_name,
    region_name = region_name
  )

  message("DONE")

}
