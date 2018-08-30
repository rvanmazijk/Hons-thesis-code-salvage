# Making SWAFR rasters

# Hons thesis
# Ruan van Mazijk

# created:      2017-06-28
# last edited:  2017-06-29


# Setup ---------------------------------------------------------------------------------

rm(list = ls())
source("Scripts/i_my_funs.R")
source("Scripts/ii_my_objs.R")
source("Scripts/v_prelim_anal_fn.R")


# CHIRPS_annual- & MAP_SWAFR ------------------------------------------------------------

if (!"chirps_annual_SWAFR" %in% ls()) {

  year <- 1981:2016 # note, no 2017 annual---yet! duh!
  
  stacked_chirps <- stack()
  for (i in 1:36) {
    x <-
      paste0(
        giswd, "CHIRPS_v2.0/global_annual/chirps-v2.0.",
        year[i], ".tif"
      ) %>% 
      raster() %>%
      mask(SWAFR_border) %>%
      crop(SWAFR_border)
    stacked_chirps %<>% stack(x)
    print(paste0("chirps-v2.0.", year[i], " is finished."))
  }
  beep(2)
  
  writeRaster(
    stacked_chirps,
    filename = paste0(reswd, "CHIRPS_annual_SWAFR.grd"),
    bandorder = "BIL",
    overwrite = T
  )
  
}

if (!"MAP_SWAFR_0.05" %in% ls()) {
  MAP_SWAFR_0.05 <- mean(chirps_annual_SWAFR)
  writeRaster(
    MAP_SWAFR,
    filename = paste0(reswd, "MAP_SWAFR.grd"),
    bandorder = "BIL",
    overwrite = T
  )
}


# CHIRPS_annual- & MAP_SWAFR_buffered ---------------------------------------------------

if (!"SWAFR_border_buffered" %in% ls()) {
  agg_MAP_SWAFR <- custom_aggregate_loop(MAP_SWAFR_0.05, facts = 2:20)
  agg_MAP_SWAFR %<>% c(MAP_SWAFR_0.05, .)
  names(agg_MAP_SWAFR)[1] <- 1
  agg_MAP_SWAFR_borders <- vector("list", length = 20)
  for (i in 1:20) {
    temp <- agg_MAP_SWAFR[[i]] > -Inf
    agg_MAP_SWAFR_borders[[i]] <- rasterToPolygons(temp, dissolve = T)
  }
  names(agg_MAP_SWAFR_borders) <- 1:20
  SWAFR_border_buffered <- agg_MAP_SWAFR_borders$`20`
}

if (!"chirps_annual_SWAFR_buffered" %in% ls()) {
  
  year <- 1981:2016 # note, no 2017 annual---yet! duh!
  
  stacked_chirps <- stack()
  for (i in 1:36) {
    x <-
      paste0(
        giswd, "CHIRPS_v2.0/global_annual/chirps-v2.0.",
        year[i], ".tif"
      ) %>% 
      raster() %>%
      mask(SWAFR_border_buffered) %>%
      crop(SWAFR_border_buffered)
    stacked_chirps %<>% stack(x)
    print(paste0("chirps-v2.0.", year[i], " is finished."))
  }
  beep(2)
  
  writeRaster(
    stacked_chirps,
    filename = paste0(reswd, "CHIRPS_annual_SWAFR_buffered.grd"),
    bandorder = "BIL",
    overwrite = T
  )
  
}

if (!"MAP_SWAFR_0.05_buffered" %in% ls()) {
  MAP_SWAFR_0.05_buffered <- mean(chirps_annual_SWAFR_buffered)
  MAP_SWAFR_0.05_buffered[MAP_SWAFR_0.05_buffered < -1000] <- NA
  writeRaster(
    MAP_SWAFR_0.05_buffered,
    filename = paste0(reswd, "MAP_SWAFR_buffered.grd"),
    bandorder = "BIL",
    overwrite = T
  )
}


# MODIS mean annual & monthly LST SWAFR -------------------------------------------------
# All "[ √ ]" or "[ _ ]" are as of 2017-06-28 13:57

if (!("MA_LST_SWAFR_0.05_buffered"      %in% ls()) &&
    !("monthly_LST_SWAFR_0.05_buffered" %in% ls())) {
  
  modis_jan <- proj_crop_mask_mean_write(                                           # [ √ ]
    df_of_files = modfile_query(month = "Jan"),
    region_name = "SWAFR",
    slave_border = SWAFR_border_buffered
  )
                                                                        # Feb   Mar   Apr
  modis_feb <- proj_crop_mask_mean_write(                               # [ √ ] [ √ ] [ √ ]
    df_of_files = modfile_query(month = "Feb"),
    region_name = "SWAFR",
    slave_border = SWAFR_border_buffered
  )
  modis_mar <- proj_crop_mask_mean_write(
    df_of_files = modfile_query(month = "Mar"),
    region_name = "SWAFR",
    slave_border = SWAFR_border_buffered
  )
  modis_apr <- proj_crop_mask_mean_write(
    df_of_files = modfile_query(month = "Apr"),
    region_name = "SWAFR",
    slave_border = SWAFR_border_buffered
  )
  
  modis_dec <- proj_crop_mask_mean_write(                                           # [ √ ]
    df_of_files = modfile_query(
      month = "Dec",  
      years = 2000:2016 
    ),
    region_name = "SWAFR",
    slave_border = SWAFR_border_buffered
  )
  
  modis_may <- proj_crop_mask_mean_write(                                           # [ √ ]
    df_of_files = modfile_query(
      month = "May",  
      years = 2000:2016 
    ),
    region_name = "SWAFR",
    slave_border = SWAFR_border_buffered
  )      
  modis_jun <- proj_crop_mask_mean_write(                                           # [ √ ]
    df_of_files = modfile_query(
      month = "Jun",  
      years = 2000:2016 
    ),
    region_name = "SWAFR",
    slave_border = SWAFR_border_buffered
  )      
  modis_jul <- proj_crop_mask_mean_write(                                           # [ √ ]
    df_of_files = modfile_query(
      month = "Jul",  
      years = 2000:2016 
    ),
    region_name = "SWAFR",
    slave_border = SWAFR_border_buffered
  )      
  modis_aug <- proj_crop_mask_mean_write(                                           # [ √ ]
    df_of_files = modfile_query(
      month = "Aug",  
      years = 2000:2016 
    ),
    region_name = "SWAFR",
    slave_border = SWAFR_border_buffered
  )      
  modis_sep <- proj_crop_mask_mean_write(                                           # [ √ ]
    df_of_files = modfile_query(
      month = "Sep",  
      years = 2000:2016 
    ),
    region_name = "SWAFR",
    slave_border = SWAFR_border_buffered
  )      
  modis_oct <- proj_crop_mask_mean_write(                                           # [ √ ]
    df_of_files = modfile_query(
      month = "Oct",  
      years = 2000:2016 
    ),
    region_name = "SWAFR",
    slave_border = SWAFR_border_buffered
  )      
  modis_nov <- proj_crop_mask_mean_write(                                           # [ √ ]
    df_of_files = modfile_query(
      month = "Nov",  
      years = 2000:2016 
    ),
    region_name = "SWAFR",
    slave_border = SWAFR_border_buffered
  )      
  
  monthly_LST_SWAFR_0.05_buffered <- stack()
  for (i in 1:12) {
    x <- raster(paste0(reswd, "MODIS_", i, "_SWAFR_0.05_buffered.grd"))
    monthly_LST_SWAFR_0.05_buffered %<>% stack(x)
  }
  names(monthly_LST_SWAFR_0.05_buffered) <- c(
    "Jan", "Feb", "Mar",
    "Apr", "May", "Jun",
    "Jul", "Aug", "Sep",
    "Oct", "Nov", "Dec"
  )
  writeRaster(                                                                      # [ √ ]
    monthly_LST_SWAFR_0.05_buffered,
    filename = paste0(reswd, "MODIS_monthly_means_SWAFR_0.05_buffered.grd"),
    bandorder = "BIL",
    overwrite = T
  )
  
  MA_LST_SWAFR_0.05_buffered <- mean(monthly_LST_SWAFR_0.05_buffered)
  writeRaster(                                                                      # [ √ ]
    MA_LST_SWAFR_0.05_buffered,
    filename = paste0(reswd, "MODIS_annual_mean_SWAFR_0.05_buffered.grd"),
    bandorder = "BIL",
    overwrite = T
  )

}


# SRTM_SWAFR ----------------------------------------------------------------------------

if (!"SRTM_SWAFR_0.05_buffered" %in% ls()) {
  
  SRTM <- raster::getData(
    "alt",
    country = "AUS",
    path = paste0(giswd, "SRTM/")
  )[[1]]
  SRTM %<>% projectRaster(crs = std_CRS)
  proj4string(SRTM)
  plot(SRTM)

  SRTM_SWAFR_buffered <- SRTM %>%
    mask(SWAFR_border_buffered) %>%
    crop(SWAFR_border_buffered)
  
  # Bilinear resampling of 0.0083 deg SRTM raster to 0.05 deg:
  SRTM_SWAFR_0.05_buffered <- custom_resample(SRTM_SWAFR_buffered, res = 0.05)
  
  writeRaster(
    SRTM_SWAFR_0.05_buffered,
    filename = paste0(reswd, "SRTM_SWAFR_0.05_buffered.tif"),
    overwrite = T
  )
  writeRaster(
    SRTM_SWAFR_0.05_buffered,
    filename = paste0(reswd, "SRTM_SWAFR_0.05_buffered.grd"),
    overwrite = T
  )
  
}


# </> -----------------------------------------------------------------------------------

