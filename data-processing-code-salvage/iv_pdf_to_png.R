# Converting my Hons `.pdf` figures to `.png`s
# (A temporary fix so that I can put the colour figs
# on RPubs more easily and clearly for use during meetings)

# Hons thesis
# Ruan van Mazijk

# created:      2017-05-25
# last edited:  <doesn't matter, this is a messy script lol>


# Setup ---------------------------------------------------------------------------------

source("i_my_funs.R")
source("ii_my_objs.R")

p_load(animation)


# ---------------------------------------------------------------------------------------

f <- list.files(
  path = paste0("/Users/ruanvanmazijk/Documents/R/Hons_thesis/Results/null_raster_HSAR_MAP_GCFR_buffered/models_MAP_GCFR/"),
  #path = paste0(reswd, "extracted_abs_vs_rough_agg_MAP_GCFR/"),
  pattern = ".pdf"
)
#f <- paste0(reswd, "extracted_abs_vs_rough_agg_MAP_GCFR/", f)
f <- paste0("/Users/ruanvanmazijk/Documents/R/Hons_thesis/Results/null_raster_HSAR_MAP_GCFR_buffered/models_MAP_GCFR/", f)

for (i in 1:length(f)) {
  im.convert(
    paste0(f[i]),
    output = paste0(
      f[i] %>% substr(0, nchar(f[i]) - 4),
      ".png"
    )
  )
}


# ---------------------------------------------------------------------------------------

f <- list.files(
  path = paste0(reswd, "PCA_rough_agg_mean_annual_LST_GCFR/"),
  pattern = "22.pdf"
)
f <- paste0(reswd, "PCA_rough_agg_mean_annual_LST_GCFR/", f)

for (i in 1:length(f)) {
  im.convert(
    paste0(f[i]),
    output = paste0(
      f[i] %>% substr(0, nchar(f[i]) - 4),
      ".png"
    )
  )
}


# ---------------------------------------------------------------------------------------



