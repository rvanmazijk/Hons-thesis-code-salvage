# Randomised raster null analysis function

# Hons thesis
# Ruan van Mazijk

# created:      2017-06-29
# last edited:  2017-06-30

# <> ------------------------------------------------------------------------------------


#' do_rand_null
#'
#' 2017-06-30 17:29 TODO --- write descr
#' 
#' @param x A raster, to be agg()-ed and shuff()-ed by \code{many_SARs}
#' @param how_many_SARs A number
#' @param facts A numeric vector, for agg()-ing in \code{many_SARs}
#' @param border A spatial polygons dataframe
#' @param border_buffer A spatial polygons dataframe
#' @param var_name A string, characters describing the variable concerned,
#'                 for labelling columns etc.
#' @param region_name A string
#' @param how_many_rand_pts A number
#' @param x_tester A raster, from which to sample `rand_pts`
#' @param reswd_temp A string, the chosen dir for all outputs
#'
#' @return 2017-06-30 17:29 TODO --- write descr
do_rand_null <- function(x, how_many_SARs = 1,
                         border, border_buffer, facts = 2:15,
                         var_name, region_name = c("GCFR", "SWAFR"),
                         how_many_rand_pts = 1000, x_tester,
                         reswd_temp, plot_format = c("PDF", "TIFF"), save_grd = TRUE) {
  
  if (how_many_SARs != 1) {
    stop(
      "Please do only 1 random raster, the function has yet to be implemented for > 1."
    )
  }
  
  do_prelim_anal(
    agg_x             = many_SARs(x, how_many = how_many_SARs, facts = facts)[[1]],
                        # 2017-06-29 14:50 TODO --- implement for > 1?
    border            = border,
    border_buffer     = border_buffer,
    facts             = facts,
    var_name          = var_name,
    region_name       = region_name,
    reswd_temp        = reswd_temp,
    x_tester          = x_tester,
    how_many_rand_pts = how_many_rand_pts,
    save_grd          = save_grd 
  )

}
  

# </> -----------------------------------------------------------------------------------

