# Working w/ MOD11C3

# Hons thesis
# Ruan van Mazijk

# created:      2017-05-24
# last edited:  2017-06-21

# Preamble/contents ---------------------------------------------------------------------

# < ... >


# Setup ---------------------------------------------------------------------------------

rm(list = ls())
source("Scripts/i_my_funs.R")
source("Scripts/ii_my_objs.R")


# <DEPREC> MOD11C3 monthly stacks: exploring the .tifs ----------------------------------

#year <- as.character(2000:2017) # the full temporal range of the MOD11C3 data
#month <- c(
## Jan,   Feb,   Mar,   Apr,   May,   Jun,   Jul,   Aug,   Sep,   Oct,   Nov,   Dec
#  "001", "032", "060", "091", "121", "152", "182", "213", "244", "274", "305", "335"
#)
#
#f_all_yr <- vector("list", length = length(year))
#for (i in 1:length(year)) {
#  f_all_yr[[i]] <- list.files(
#    path = modwd,
#    pattern = paste0("MOD11C3.A", year[i], ".*.tif")
#  )
#}
#names(f_all_yr) <- year
#f_all_yr
#f_all_yr %>% map(length)
#
## Note that 2000 is missing Jan, and 2017 only goes up to Apr
## Therefore, here is the subset of years that have all 12 months:
#start <- Sys.time()
#year_subset <- year[-c(1, 18)]
#f_yr_subset <- vector("list", length = length(year_subset))
#for (i in 1:length(year_subset)) {
#  f_yr_subset[[i]] <- list.files(
#    path = modwd,
#    pattern = paste0("MOD11C3.A", year_subset[i], ".*.tif")
#  )
#}
#names(f_yr_subset) <- year_subset
#f_yr_subset
#end <- Sys.time()
#(diff1 <- end - start)
#f_yr_subset %>% map(length)
#
## Alternatively, and more cleverly:
#start <- Sys.time()
#len_bool <- c()
#for (i in 1:length(f_all_yr)) {
#  len_bool[i] <- length(f_all_yr[[i]]) == 12
#}
#f_yr_subset <- f_all_yr[len_bool]
#end <- Sys.time()
#(diff2 <- end - start)
#as.numeric(diff1) / as.numeric(diff2) # 2-3 times faster than the previous method!
## Same result either way:
#f_yr_subset


# Note ----------------------------------------------------------------------------------

# 2017-06-08 13:41 --- Some NB things to note...
# 
# What I have done up until this point
#   (today being 2017-06-08 13:42, with the last appreciable edit of this script
#   @ time of writing this being 2017-05-31)
#   (see code below---but likely edited to be correct now!)
# isn't exactly correct. Let me explain:
# 
# What I want/need:
#   1)  avg_annual_LST
#         e.g. avg_LST_2001 = (LST_2001_Jan + LST_2001_Feb + ... + LST_2001_Dec) / 12
#              avg_LST_2002 = ''
#              ...          = ''
#              avg_LST_2016 = ''
#              -- <mean()> --
#              avg_annual_LST
#         NB: avg_annual_LST MUST == mean(
#           LST_2000_Feb + ... +
#           LST_2001_Jan + LST_2001_Feb + ... +
#           LST_2002_Jan + LST_2002_Feb + ... +
#           ...
#         )
#   2)  avg_LST for each month
#         e.g. avg_LST_Jan = (LST_2001_Jan + LST_2002_Jan + ... + LST_2016) / 16
#              avg_LST_Feb = ''
#              ...
#              avg_LST_Dec = ''
# 
# What I have made below:
#   1) avg_annual_LST -> JJ"<
#   2) avg_LST for each month ->
#       Feb:  Mar_2000, Feb_2001, Feb_2002, ... Feb_2017
#       Mar:  ''
#       ...
#       ^-- for all months that appear in 2000... bc of the STUPID situatuation where
#           Feb is `2000[1]`, not `2000[2]`, bc `2000$Jan` isn't in the data's range.
#       So, basically, the only months that are (as of 2017-06-08 16:43) error free are:
#         - Jan
#         - And that's it... only Jan...
# Solution:
#   Introduce a "dummy", blank Jan to the 2000 stack!
#   This will allow to work with 2000 more easily,
#   and using largely the same code as before,
#   and satisfy the avg_annual_LST condition above (see "NB").
#   
# Now let's do it!
# 
# Ruan
# 2017-06-08 16:49 ---


# <DEPREC> MOD11C3 monthly stacks: fiddling to get this to work -------------------------

#if (F) { # Ran [ √ ]; Correct [ √ ] (2017-06-08 16:50);
#
#modis_jan    <- raster(paste0(reswd, "MODIS_Jan_GCFR_0.05_buffered.grd"))
#modis_jan_v2 <- raster(paste0(reswd, "MODIS_Jan_v2_GCFR_0.05_buffered.grd"))
#par(mfrow = c(2, 2))
#plot(modis_jan)
#plot(modis_jan_v2)
#plot(modis_jan[])
#plot(modis_jan_v2[])
#par(op)
#
#eg <- proj_crop_mask_mean(
#  month = "Jan",
#  years = as.character(2001),
#  border = GCFR_border_buffered
#)
#writeRaster(
#  eg,
#  filename = paste0(reswd, "MODIS_Jan_v3_GCFR_0.05_buffered.grd"),
#  overwrite = F
#)
#
#modis_jan_v3 <- raster(paste0(reswd, "MODIS_Jan_v3_GCFR_0.05_buffered.grd"))
#par(mfrow = c(2, 3))
#plot(modis_jan)
#plot(modis_jan_v2)
#plot(modis_jan_v3)
#plot(modis_jan[])
#plot(modis_jan_v2[])
#plot(modis_jan_v3[])
#par(op)
#
#modis_jan <- proj_crop_mask_mean(
#  month = "Jan",
#  years = as.character(2001:2017), # because 2000 is missing Jan
#  border = GCFR_border_buffered
#)
#writeRaster(
#  modis_jan,
#  filename = paste0(reswd, "MODIS_Jan_v4_GCFR_0.05_buffered.grd"),
#  overwrite = T
#)
#
#}
#
#
# <DEPREC> MOD11C3 monthly stacks: making the monthly avg_LST .tifs ---------------------
#
#month <- c(
#  "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
#) 
#
#modis_jan <- raster(paste0(reswd, "MODIS_Jan_v4_GCFR_0.05_buffered.grd"))
#
## Now for Feb, Mar, Apr:
#month
#if (F) { # Ran [ √ ]; Re-ran [ ]; Correct [ ] (<date>);
#  
#  for (i in 2:4) {
#    print(month[i])
#    x <- proj_crop_mask_mean(
#      month = month[i],
#      years = as.character(2000:2017), # has the full temporal range
#      border = GCFR_border_buffered
#    )
#    writeRaster(
#      x,
#      filename = paste0(reswd, "MODIS_", month[i], "_GCFR_0.05_buffered.grd"),
#      overwrite = T
#    )
#  }
#
#  modis_jan <- raster(paste0(reswd, "MODIS_Jan_v4_GCFR_0.05_buffered.grd"))
#  modis_feb <- raster(paste0(reswd, "MODIS_Feb_GCFR_0.05_buffered.grd"))
#  modis_mar <- raster(paste0(reswd, "MODIS_Mar_GCFR_0.05_buffered.grd"))
#  modis_apr <- raster(paste0(reswd, "MODIS_Apr_GCFR_0.05_buffered.grd"))
#  
#  par(mfrow = c(2, 2), mar = c(0, 2, 0, 2))
#  plot(modis_jan)
#  plot(modis_feb)
#  plot(modis_mar)
#  plot(modis_apr)
#  par(op)
#
#}
#
## Then restart R & RStudio to free up disc & RAM space: [ √ ]
#
## Now for May, Jun, Jul, & Aug:
## 2017-05-31 18:16 --- Tricky, b.c. for 2000, Feb = 1... not 2...
## So have to rerun this with that in mind...
## (the "2KJan error")
#
#if (F) { # Ran [ √ ]; Re-ran [ ]; Correct [ ] (<date>);
#  for (i in 5:8) {
#    print(month[i])
#    x <- proj_crop_mask_mean(
#      month = month[i],
#      years = as.character(2000:2016), # bc 2017 May:Dec hasn't happened yet...
#      border = GCFR_border_buffered
#    )
#    writeRaster(
#      x,
#      filename = paste0(reswd, "MODIS_", month[i], "_GCFR_0.05_buffered.grd"),
#      overwrite = T
#    )
#  }
#}
#
## Now for Sep, Oct, Nov, & Dec:
#if (F) { # Ran [ √ ]; Re-ran [ ]; Correct [ ] (<date>);
#  for (i in 9:12) {
#    print(month[i])
#    x <- proj_crop_mask_mean(
#      month = month[i],
#      years = as.character(2000:2016), # bc 2017 May:Dec hasn't happened yet...
#      border = GCFR_border_buffered
#    )
#    writeRaster(
#      x,
#      filename = paste0(reswd, "MODIS_", month[i], "_GCFR_0.05_buffered.grd"),
#      overwrite = T
#    )
#  }
#}


# MOD11C3 monthly stacks: Attempt on 2017-06-20 to 21 -----------------------------------
# Start: 2017-06-20 ca. 21:00
# Jan mean_monthly_LST already made above:                                          [ √ ]
modis_jan
# (Note: `Results/MODIS_Jan_v4_GCFR_0.05_buffered` IS
#   `Results/MODIS_1_GCFR_0.05_buffered.grd`,
#   I just made the `..._1_...` one so that I con read it in in a loop easily, below.)

# Inspect Feb_file_query, just to see:
feb_fileq <- modfile_query(month = "Feb")
levels(feb_fileq$month)
# Write {Feb, Mar, May}'s mean_monthly_LSTs to disc AND to object,
# for years 2000:2017 (default):                                        [ √ ] [ √ ] [ √ ]
modis_feb <- proj_crop_mask_mean_write(df_of_files = modfile_query(month = "Feb"))
modis_mar <- proj_crop_mask_mean_write(df_of_files = modfile_query(month = "Mar"))
modis_apr <- proj_crop_mask_mean_write(df_of_files = modfile_query(month = "Apr"))

# Write {May, Jun, ... Dec}'s mean_monthly_LST to disc AND to object,
# for years 2000:2016 (b.c. 2017 May+ DNE):
modis_dec <- proj_crop_mask_mean_write(df_of_files = modfile_query(               # [ √ ]
  month = "Dec",  
  years = 2000:2016 
))  
# Most recent √-batch finished on: 2017-06-20 23:19
# Therefore:
# 2017-06-20 23:19 <...> --- run `proj_crop_mask_mean_write(modfile_query())` on May---Nov.

# 2017-06-21 14:42 --- cont. w/ May---Nov:
modis_may <- proj_crop_mask_mean_write(df_of_files = modfile_query(               # [ √ ]
  month = "May",          
  years = 2000:2016         
))          
modis_jun <- proj_crop_mask_mean_write(df_of_files = modfile_query(               # [ √ ]
  month = "Jun",        
  years = 2000:2016       
))        
modis_jul <- proj_crop_mask_mean_write(df_of_files = modfile_query(               # [ √ ]
  month = "Jul",            
  years = 2000:2016           
))            
modis_aug <- proj_crop_mask_mean_write(df_of_files = modfile_query(               # [ √ ]
  month = "Aug",            
  years = 2000:2016           
))            
modis_sep <- proj_crop_mask_mean_write(df_of_files = modfile_query(               # [ √ ]
  month = "Sep",          
  years = 2000:2016         
))          
modis_oct <- proj_crop_mask_mean_write(df_of_files = modfile_query(               # [ √ ]
  month = "Oct",          
  years = 2000:2016         
))          
modis_nov <- proj_crop_mask_mean_write(df_of_files = modfile_query(               # [ _ ]
  month = "Nov",          
  years = 2000:2016         
))          

# <all buffered>

# Now let's load all those objects back into a raster-stack,
# for use with making bioclim vars:

monthly_LST_GCFR_0.05_buffered <- stack()
for (i in 1:12) {
  x <- paste0(reswd, "MODIS_", i, "_GCFR_0.05_buffered.grd") %>%
    raster()
  monthly_LST_GCFR_0.05_buffered %<>% stack(x)
}

monthly_LST_GCFR_0.05_buffered

names(monthly_LST_GCFR_0.05_buffered) <- c(
  "Jan", "Feb", "Mar",
  "Apr", "May", "Jun",
  "Jul", "Aug", "Sep",
  "Oct", "Nov", "Dec"
)

writeRaster(
  monthly_LST_GCFR_0.05_buffered,
  filename = paste0(reswd, "MODIS_monthly_means_GCFR_0.05_buffered.grd"),
  bandorder = "BIL",
  overwrite = T
)


# MOD11C3 mean annual LST stacks: Attempt on 2017-06-21 ---------------------------------

MA_LST_GCFR_0.05_buffered <- mean(monthly_LST_GCFR_0.05_buffered)
writeRaster(
  MA_LST_GCFR_0.05_buffered,
  filename = paste0(reswd, "MODIS_annual_mean_GCFR_0.05_buffered.grd"),
  bandorder = "BIL",
  overwrite = T
)

# 2017-06-21 17:48 --- And with that, all my MODIS data has been imported. done.


# Junk ----------------------------------------------------------------------------------

#if (F) {
#
#  modis_monthly_GCFR <- vector("list", length = 12)
#  
#  for (i in 1:1) {
#  #for (i in 1:12) {
#    pb <- txtProgressBar(min = 0, max = length(year_subset), style = 3)
#    stack_month <- stack()
#    #for (j in 1:1) {
#    for (j in 1:length(year_subset)) {
#      x <- paste0(modwd, f[[j]][i]) %>%
#        raster() %>%
#        projectRaster(crs = std_CRS) %>%
#        mask(GCFR_border_buffered) %>%
#        crop(GCFR_border_buffered)
#      stack_month %<>% stack(x)
#      setTxtProgressBar(pb, j)
#      stack_month %<>% mean()
#    }
#    close(pb)
#    modis_monthly_GCFR[[i]] <- stack_month
#  }
#
#  modis_monthly_GCFR
#
#  writeRaster(
#    modis_monthly_GCFR[[1]],
#    filename = paste0(reswd, "MODIS_Jan_GCFR_0.05_buffered.tif"),
#    overwrite = T
#  )
#  
#  writeRaster(
#    modis_monthly_GCFR[[1]],
#    filename = paste0(reswd, "MODIS_Jan_GCFR_0.05_buffered.grd"),
#    overwrite = T
#  )
#  
#  MODIS_GCFR_0.05 <- paste0(reswd, "MODIS_Jan_GCFR_0.05_buffered.grd") %>%
#    raster()
#  
#  plot(MODIS_GCFR_0.05)
#
#}

#if (F) {
#  for (i in 1:1) { # 2017-05-29 17:11 --- need to run from Jan again :(
#    print(month[i])
#    pb <- txtProgressBar(min = 0, max = length(year_subset), style = 3)
#    stack_month <- stack()
#    #for (j in 1:1) {
#    for (j in 1:length(year_subset)) {
#      x <- paste0(modwd, f[[j]][i]) %>% raster()
#      setTxtProgressBar(pb, j - 0.8)
#      x %<>% projectRaster(crs = std_CRS)
#      setTxtProgressBar(pb, j - 0.6)
#      x %<>% mask(GCFR_border_buffered)
#      setTxtProgressBar(pb, j - 0.4)
#      x %<>% crop(GCFR_border_buffered)
#      setTxtProgressBar(pb, j - 0.2)
#      stack_month %<>% stack(x)
#      #stack_month %<>% mean() # 2017-05-29 17:11 --- NOOOOOOOOO!!! should be --v
#      setTxtProgressBar(pb, j)
#    }
#    stack_month %<>% mean()
#    writeRaster(
#      stack_month,
#      filename = paste0(reswd, "MODIS_", month[i], "_GCFR_0.05_buffered.grd"),
#      overwrite = F
#    )
#    close(pb)
#  }
#}

#if (T) {
#  f_jan <- vector("list", length = length(2001:2017))
#  year <- 2001:2017
#  for (i in 1:length(year)) {
#    f_jan[[i]] <- list.files(
#      path = modwd,
#      pattern = paste0("MOD11C3.A", year[i], "001.*.tif")
#    )
#  }
#  pb <- txtProgressBar(min = 0, max = length(year), style = 3)
#  modis_stack_jan <- stack()
#  for (i in 1:length(year)) {
#    x <- paste0(modwd, f_jan[[i]]) %>% raster()
#    setTxtProgressBar(pb, i - 0.8)
#    x %<>% projectRaster(crs = std_CRS)
#    setTxtProgressBar(pb, i - 0.6)
#    x %<>% mask(GCFR_border_buffered)
#    setTxtProgressBar(pb, i - 0.4)
#    x %<>% crop(GCFR_border_buffered)
#    setTxtProgressBar(pb, i - 0.2)
#    modis_stack_jan %<>% stack(x)
#    setTxtProgressBar(pb, i - 0.0)
#  }
#  modis_stack_jan %<>% mean()
#  writeRaster(
#    modis_stack_jan,
#    filename = paste0(reswd, "MODIS_Jan_v2_GCFR_0.05_buffered.grd"),
#    overwrite = F
#  )
#}


# </> -----------------------------------------------------------------------------------

