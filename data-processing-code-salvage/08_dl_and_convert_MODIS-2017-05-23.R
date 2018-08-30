# Working with MODIS in R

# Hons thesis
# Ruan van Mazijk

# created:      2017-05-16
# last edited:  2017-05-23


# Setup ---------------------------------------------------------------------------------

rm(list = ls())
source("i_my_funs.R")
source("ii_my_objs.R")
#quartz()


# Downloading monthly Terra MODIS -------------------------------------------------------
# (MOD11 L3 6km SIN Grid V006)
# (for the tiles I specified on NASA EarthData Search UI) (encompassing the GCFR)

# 2017-05-17 12:48 --- DIDN'T ACTUALLY WORK :( Only downloaded weird empty files that
# themselves contain an hypertext link to the ACTUAL files... t.f.:
# 2017-05-17 17:15 <see the todo below> --- Need to redesign/run this s.t. it actually GETS me the
# MODIS data! (or at least think of a method to get the MODIS data!!)

# <2017-05-16>
run <- F # so that this doesn't run everytime, bc it doesnt do what I though it does!!
if (run) {

  # Here's the `.R` script with the URLS in it (given to me and copy-pasted from the NASA
  # EarthDAta Search UI output)
  source(paste0(giswd, "/Test/modis_urls_monthly.R")) # the .R script with the
  
  # Now let's run a loop with the base fn `download.file()` to GET all this stuff!!:
  pb <- txtProgressBar(min = 0, max = length(urls_monthly), style = 3)
  for (i in 1:length(urls_monthly)) {
    download.file(
      url = urls_monthly[i], # the ith URL in the `urls` character vector
      destfile = paste0(
        giswd, "/Test/",
        "MODIS_Terra Land Surface Temperature_Emissivity ",
        "Monthly L3 Global 6km SIN Grid V006/",
        urls_monthly[i] %>% substr(nchar(.) - 44, nchar(.))
      ), # to be written to the filename, in that directory, 
         # where the filename comes from the url (via substrings!)!
      method = "wget", # techinical stuff *shrugs*
      quiet = T,
      #mode = "w",
      cacheOK = T,
      extra = getOption("download.file.extra")
    )
    setTxtProgressBar(pb, i)
  }
  beep(2)
  close(pb)
  
  # Check that they all downloaded:
  files <- vector(length = length(urls_monthly))
  for (i in 1:length(urls_monthly)) {
    files[i] <- paste0(
      giswd, "Test/",
      "MODIS_Terra Land Surface Temperature_Emissivity ",
      "Monthly L3 Global 6km SIN Grid V006/",
      urls_monthly[i] %>% substr(nchar(.) - 44, nchar(.))
    )
  }
  summary(files)
  head(files)
  for (i in 1:length(urls_monthly)) {
    if (file.exists(files[i])) {
      print("1")
    } else {
      print("0")
    }
  }
  # All 1s!
  # Plus the fact that there ARE 836 items in that folder in finder = yay!

}
# </2017-05-16>


# Downloading 8-Day Terra MODIS ---------------------------------------------------------
# (MOD11 L3 6km SIN Grid V006)
# (for the tiles I specified on NASA EarthData Search UI) (encompassing the GCFR)

# 2017-05-17 12:48 --- DIDN"T ACTUALL WORK :( Only downloaded weird empty files that themselves contain an hypertext link to the ACTUAL files... t.f.:
# 2017-05-17 17:15 <see the todo below> --- run this s.t. it actually gets me the MODIS data!

# <2017-05-16>
run <- F
if (run) {
  
  # there are a lot more of these, so let's use a `.csv` of the URLs:
  urls_8_day <-
    read.csv(
      paste0(giswd, "/Test/modis_urls_8-Day.csv"),
      head = F
    )
  urls_8_day <- urls_8_day[, 1]
  urls_8_day <- as.character(urls_8_day)
  summary(urls_8_day)
  head(urls_8_day)
  
  # Now let's run the loop on this:
  pb <- txtProgressBar(min = 0, max = length(urls_8_day), style = 3)
  for (i in 1:length(urls_8_day)) {
    download.file(
      url = urls_8_day[i], # the ith URL in the `urls_8_day` character vector
      destfile = paste0(
        giswd, "/Test/",
        "MODIS_Terra Land Surface Temperature_Emissivity ",
        "8-Day L3 Global 6km SIN Grid V006/",
        urls_8_day[i] %>% substr(nchar(.) - 44, nchar(.))
      ), # to be written to the filename, in that directory, 
      # where the filename comes from the url (via substrings!)!
      method = "curl", # techinical stuff *shrugs*
      quiet = T,
      mode = "w",
      cacheOK = T,
      extra = getOption("download.file.extra")
    )
    setTxtProgressBar(pb, i)
  }
  beep(2)
  close(pb)
  
  # Check that they all downloaded:
  files <- vector(length = length(urls_8_day))
  for (i in 1:length(urls_8_day)) {
    files[i] <- paste0(
      giswd, "Test/",
      "MODIS_Terra Land Surface Temperature_Emissivity ",
      "8-Day L3 Global 6km SIN Grid V006/",
      urls_8_day[i] %>% substr(nchar(.) - 44, nchar(.))
    )
  }
  summary(files)
  head(files)
  checks <- vector(length = length(urls_8_day))
  for (i in 1:length(urls_8_day)) {
    if (file.exists(files[i])) {
      checks[i] <- T
    } else {
      checks[i] <- F
    }
  }
  !any(checks)
  
  # Yay!
  
}
# </2017-05-16>


# Processing ----------------------------------------------------------------------------
# 2017-05-18 12:37 --- THIS WORKS!!
# 2017-05-18 16:56 --- (Ran on some random MODIS `.hdf`s, from my many many foreys into
# downloading MODIS data, to test it)

formats <- "gdal-config --formats" %>% # Send this char string to `bash`,
  system(intern = T) %>%               # printing the results /intern/ally in `R`,
  strsplit(split = " ") %>%
  as_vector() %>%
  sort()                               # & make an alphabetised vector of all the...
# ... file formats my /main/ GDAL installation can work with.

formats
# No HDF4 here!
# So, must use QGIS's GDAL library & `.py` & `.sh` executables instead.

# Assign the directory where there exe's are to a char obj (ease of use)
# (found this dir by fiddling around in QGIS's algorithm toolbox)
qgis_gdal <- "/Library/Frameworks/GDAL.framework/Versions/1.11/Programs/"

# And the dir where my random MODIS data lies:
datwd_temp <- paste0(datwd, "MODIS_fiddling/")

# Now, let's apply the `gdal_info` algorithm to `MOD11B3.A2000032.h19v11.006.
# 2015168203953.hdf`, so that we can see what we're dealing with, in `bash`!
# (I do this in `bash` bc the `RGIS` pkg doesn't work either---maybe it too is looking
# for the GDAL algorithms in the wrong place???)
# (and I use `bash` here in `R` bc `bash`'s scripting is junk lol! And now I'm working 
# in one nice place :D )
system(
  paste0(
    qgis_gdal, "gdalinfo ", "HDF4_EOS:EOS_GRID:",
    datwd_temp, "\"", "MOD11B3.A2000032.h19v11.006.2015168203953.hdf", "\"",
    ":MODIS_Grid_Month_6km_LST:Emis_20"
  )
)
system(
  paste0(
    qgis_gdal, "gdalinfo ",
    datwd_temp, "MOD11B3.A2000032.h19v11.006.2015168203953.hdf"
  )
)
# Which is the same as the cumbersome and long:
system("/Library/Frameworks/GDAL.framework/Versions/1.11/Programs/gdalinfo /Users/ruanvanmazijk/Documents/R/Hons_thesis/Data/MODIS_fiddling/MOD11B3.A2000032.h19v11.006.2015168203953.hdf")
# Or
system(
  paste0(datwd_temp, "/gdal_info_fiddle.txt") %>%
    readChar(., file.info(.)$size)
)
# Which is the same as the AMAZING convenience fn I wrote:
bash_gdalinfo(
  x = "MOD11B3.A2000032.h19v11.006.2015168203953.hdf",
  x_dir = datwd_temp
)
# And we can even get info for a subdataset/band:
bash_gdalinfo_one(
  x = "MOD11B3.A2000032.h19v11.006.2015168203953.hdf",
  band = ":MODIS_Grid_Month_6km_LST:Emis_20",
  x_dir = datwd_temp
)

# Now, let's apply `gdal_translate` to all bands of `MOD11B3.A2000032.h19v11.006.
# 2015168203953.hdf` in `bash`, using the gdal library executables that QGIS uses
# (i.e., ones THAT I KNOW TO WORK!!)
system(
  paste0(
    qgis_gdal, "gdal_translate ",
    "-sds ",
    datwd_temp, "MOD11B3.A2000032.h19v11.006.2015168203953.hdf ",
    datwd_temp, "myfile.tif"
  )
)
# Works!!

# Which is the same as the AMAZING convenience fn I wrote:
bash_gdaltranslate_all(
  x = "MOD11B3.A2000032.h19v11.006.2015168203953.hdf",
  out = "myfile.tif",
  x_dir = datwd_temp
)

# Now, let's apply `gdal_translate` to just one band of `MOD11B3.A2000032.h19v11.006.
# 2015168203953.hdf`,
# (namely `'HDF4_EOS:EOS_GRID:"/Users/ruanvanmazijk/Documents/R/Hons_thesis/Data/
# MODIS_fiddling/MOD11B3.A2000032.h19v11.006.2015168203953.hdf":MODIS_Grid_Month_
# 6km_LST:Emis_20'`)
# in `bash`!
system(
  paste0(
    qgis_gdal, "gdal_translate ",
    "HDF4_EOS:EOS_GRID:",
      datwd_temp, "MOD11B3.A2000032.h19v11.006.2015168203953.hdf",
    ":MODIS_Grid_Month_6km_LST:Emis_20 ",
    datwd_temp, "myfileXXX.tif"
  )
)
# Works!!

# Which is the same as the AMAZING convenience fn I wrote:
bash_gdaltranslate_one(
  x = "MOD11B3.A2000032.h19v11.006.2015168203953.hdf",
  band = ":MODIS_Grid_Month_6km_LST:Emis_20",
  out = "myfileXXX.tif",
  x_dir = datwd_temp
)

# More fiddling

temp <- paste0("~/Downloads/")
system(
  paste0(
    qgis_gdal, "gdalinfo ",
    temp, "MOD11C3.A2017032.041.2017062160424.hdf"
  )
)
system(
  paste0(
    qgis_gdal, "gdalinfo ",
    temp, "MOD11B3.A2017091.h20v12.006.2017129125338.hdf"
  )
)

system(
  paste0(
    qgis_gdal, "gdalinfo ",
    '"', "/Volumes/RUAN'S HDD/GIS/MOD11C3/501145540/",
    "MOD11C3.A2016214.006.2016286174519.hdf", '"'
  )
)

bash_gdaltranslate_one(
  x = "MOD11C3.A2016214.006.2016286174519.hdf",
  band = ":MODIS_MONTHLY_0.05DEG_CMG_LST:QC_Night",
  out = "myfileXXXxxx.tif",
  x_dir = "/Volumes/RUAN'S HDD/GIS/MOD11C3/501145540/",
  out_dir = datwd_temp
)

# /More fiddling


# Downloading MODIS (again) -------------------------------------------------------------

order <- read.csv(paste0(datwd, "NASA_ladsweb_MOD11C3_order501145540.csv"))
as_tibble(order)
order$name %<>% as.character()
order$last_modified %<>% lubridate::as_datetime()

FTP <- "ftp://ladsweb.modaps.eosdis.nasa.gov/orders/501145540/"

# Test
if (F) {

download.file(
  url = paste0(FTP, order$name[2]),
  destfile = paste0(giswd, "MOD11C3/", order$name[2]),
  method = "wget", # technical stuff *shrugs*
  quiet = T,
  #mode = "w",
  cacheOK = T,
  extra = getOption("download.file.extra")
)
# 2017-05-18 17:30 --- ^-- THIS works!!
bash_gdalinfo(
  x = paste0(order$name[2]),
  x_dir = paste0(giswd, "MOD11C3/order_501145540/")
)
bash_gdaltranslate_one(
  x = paste0(order$name[2]),
  band = ":MODIS_MONTHLY_0.05DEG_CMG_LST:QC_Night",
  out = paste0(
    order$name[2] %>% substr(0, nchar(.) - 4),
    ".tif"
  ),
  x_dir = paste0(giswd, "MOD11C3/order_501145540/"),
  out_dir = paste0(giswd, "MOD11C3/order_501145540_GeoTiffs/")
)

}
# /Test

# Running the download-loop:

as_tibble(order)

run <- F
if (run) {
  pb <- txtProgressBar(min = 0, max = length(order$name), style = 3)
  for (i in 1:length(order$name)) {
    download.file(
      url = paste0(FTP, order$name[i]),
      destfile = paste0(giswd, "MOD11C3/order_501145540/", order$name[i]),
      method = "wget", # technical stuff *shrugs*
      quiet = 2,
      #mode = "w",
      cacheOK = T,
      extra = getOption("download.file.extra")
    )
    beep(2)
    setTxtProgressBar(pb, i)
  }
  close(pb)
}

# 2017-05-22 --- Only got to run the loop for the first ...
f <- list.files(
  path = paste0(giswd, "MOD11C3/order_501145540/"),
  pattern = ".hdf"
)
length(f)
# ... 100 `.hdf`s!! Time to carry on from there:

# 2017-05-23 08:38 --- 
run <- F
if (run) {
  pb <- txtProgressBar(min = 0, max = length(order$name), style = 3)
  for (i in 99:length(order$name)) {
    download.file(
      url = paste0(FTP, order$name[i]),
      destfile = paste0(giswd, "MOD11C3/order_501145540/", order$name[i]),
      method = "wget", # technical stuff *shrugs*
      quiet = 2,
      #mode = "w",
      cacheOK = T,
      extra = getOption("download.file.extra")
    )
    beep(2)
    setTxtProgressBar(pb, i)
  }
  close(pb)
}

# 2017-05-23 15:55 --- And now for 195+ (had to re-request the files from NASA, so the
# order no. and FTP URL are now 501148261:

order2 <- read.csv(paste0(datwd, "NASA_ladsweb_MOD11C3_order501148261.csv"))
as_tibble(order2)
order2$name %<>% as.character()
order2$last_modified %<>% lubridate::as_datetime()

as_tibble(order)
as_tibble(order2)

order[,] == order2[,]
# same :)

FTP <- "ftp://ladsweb.modaps.eosdis.nasa.gov/orders/501148261/"

run <- T
if (run) {
  pb <- txtProgressBar(min = 0, max = length(order2$name), style = 3)
  for (i in 195:length(order2$name)) {
    download.file(
      url = paste0(FTP, order2$name[i]),
      destfile = paste0(giswd, "MOD11C3/order_501145540/", order2$name[i]),
      method = "wget", # technical stuff *shrugs*
      quiet = 2,
      #mode = "w",
      cacheOK = T,
      extra = getOption("download.file.extra")
    )
    beep(2)
    setTxtProgressBar(pb, i)
  }
  close(pb)
}

# 2017-05-23 16:54 --- Done! Let's check to see if we got them all:
f <- list.files(
  path = paste0(giswd, "MOD11C3/order_501145540/"),
  pattern = ".hdf"
)
length(f)
summary(order2)
# All 207! Yay!!!

# (Checked the temporal coverage as well: I think i've got it all! (for MOD11C3))


## Converting MOD11C3 from HDF4 to GeoTiff ----------------------------------------------

# Now to run `bash_gdaltranslate_loop()` to it all to make my GeoTiffs!
# (just FYI, the HDF4 files I am about to work with are:
#   - MODIS/Terra Land Surface Temperature and Emissivity Monthly L3 Global 0.05Deg CMG,
#   - from 2000-03-05 to 2017-04-30,
#   - TODO --- for day and day-night-boundary tiles only (I think?)
  
# (ignore the fact that LAADS DAAC (the website I used from NASA to /get/ these data)
# has a native post-processing option to convert HDF4s to GeoTiffs... lmao) 

bash_gdaltranslate_loop(
  x = order2$name,
  band = ":MODIS_MONTHLY_0.05DEG_CMG_LST:LST_Day_CMG", # Daytime LST (Kelvin)
  out_format = ".tif",
  x_dir = paste0(giswd, "MOD11C3/order_501145540/"),
  out_dir = paste0(giswd, "MOD11C3/order_501145540_GeoTiffs/")
)


# Junk ----------------------------------------------------------------------------------

#FTP <- "ftp://ladsweb.nascom.nasa.gov/allData/41/MOD11C3/"
#FTP <- "ftp://ladsweb.modaps.eosdis.nasa.gov/orders/501145540"
#
#p_load(RCurl)
#source("13_GetModis.R")
#GetMODIS(
#  FTP = FTP,
#  h = c(16, 17),
#  v = c(7, 7),
#  dates = c("2010.08.01", "2010.08.31"),
#  MRTLoc = "/Applications/MRT_download_Mactel/MRT_Mactel/bin",
#  projection = "UTM",
#  parameters = "-3 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
#  utm.zone = 29,
#  datum = "WGS84",
#  pixel_size = 1000
#)
#
#p_load(rts)
#FTP <- "ftp://ladsweb.nascom.nasa.gov/allData/41/MOD11C3/"
#years <- c(2015:2006)
#start <- c("01.01","07.01")
#end <- c("06.30","12.31")
#setNASAauth(
#  username = "ruanvmazijk",
#  password = "Charlottenasa1"
#)
#for (i in 1:length(years)) {
#  for (j in 1:length(start)) {
#    ModisDownload(
#      #filename = paste0(datwd_temp, "ModisDownload_fiddle.hdf"),
#      x = FTP,
#      h = c(13, 14),
#      v = c(10, 11),
#      dates = c(
#        2005
#        #paste0(years[i], ".", start[j]),
#        #paste0(years[i], ".", end[j])
#      ),
#      mosaic = T,
#      MRTpath = "/Applications/MRT_download_Mactel/MRT_Mactel/bin", #'C:/modis_MRT/bin'
#      #UL = c(state_ex@xmin, state_ex@ymax),
#      #LR = c(state_ex@xmax, state_ex@ymin), 
#      bands_subset = "1 1 1 1 1 1 1 0 0 0 1 0", # to change!
#      proj = T,
#      proj_type = "GEO",
#      proj_params = "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
#      datum = "WGS84",
#      pixel_size = 0.002259948,
#      delete = T
#    )
#  }
#}

#if (!require(gdalUtils)) {install.packages("gdalUtils")}
#library(gdalUtils)
#
#gdal_translate(
#  src_dataset = paste0(datwd, "MODIS_fiddling/test.hdf"), 
#  dst_dataset = "test.tif"
#)
#
#install.packages('ncdf4', type = "source", configure.args = c('--enable-hdf4'))
#
#nc_open(
#  filename = paste0(
#    datwd, "MODIS_fiddling/MOD11B3.A2000061.h19v12.006.2015160141751-new.hdf"
#  )
#)
#
#install.packages('rgdal', type = "source", configure.args = c('--with-hdf4'))
#
#library(raster) 
#library(rgdal)  ## built with HDF4 support 
#library(gdalUtils) 
#
#fsrc <- "ftp://ftp.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/VHP_16km/VH/VHP.G16.C07.NC.P1981035.ND#.hdf" 
#f <- file.path("/tmp", basename(fsrc)) 
#if (!file.exists(f)) download.file(fsrc, f, mode = "wb") 
#
#library(raster) 
#library(rgdal)  ## built with HDF4 support 
#library(gdalUtils) 
#
#fsrc <- "ftp://ftp.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/VHP_16km/VH/VHP.G16.C07.NC.P1981035.ND#.hdf" 
#f <- file.path(paste0(datwd, "MODIS_fiddling/test.hdf"))
#info <- gdalinfo(f)
#data <- grep("HDF4_SDS", info, value = TRUE) 
#data <- gsub("SUBDATASET_\\d+_NAME=|\\s+", "", data) 
#
#gdalinfo(
#  "/Volumes/RUAN'S HDD/GIS/Test/MODIS_Terra Land Surface Temperature_Emissivity 8-Day L3 Global 6km SIN #Grid V006/MOD11A2.A2000049.h19v11.006.2015058135050.hdf"
#  )
#
#gdalinfo("MOD17A3H.A2000001.h21v09.006.2015141183401.hdf")
#
#gdal_setInstallation(verbose=TRUE)
#
#getOption("gdalUtils_gdalPath")
#
#sds <- get_subdatasets(
#  paste0(
#    giswd, "Test/", "MODIS_Terra Land Surface Temperature_Emissivity ",
#    "Monthly L3 Global 6km SIN Grid V006/",
#    "MOD11B3.A2000032.h19v12.006.2015168203952.hdf"
#  )
#)
#
#gdal_translate(
#  src_dataset = paste0(giswd, "Test/test.hdf"),
#  dst_dataset = "test.tif"
#)
#
## from http://www.hakimabdi.com/20151023/working-with-hdf-eos-files-in-r-convert-project-and-mosaic/
#projHDF2GTiff <- function(loc, hdfs, gtiffs, lyr, fromSRS, toSRS) { 
#  if ("gdalUtils" %in% rownames(installed.packages()) == F) {
#    install.packages("gdalUtils", repos = "http://r-forge.r-project.org")
#    require(gdalUtils)
#  } # install and load the gdalUtils package. 
#  setwd(loc) # set the working directory to where the data is
#  suppressWarnings(
#    dir.create(paste(loc, "Projected", sep = "/"))
#  ) # create a directory to store projected files
#  for (i in 1:length(hdfs)) { 
#    gdal_translate(
#      hdfs[i],
#      gtiffs[i],
#      sd_index = lyr
#    ) # extract the specified HDF layer and save it as a Geotiff
#    gdalwarp(
#      gtiffs[i],
#      paste(loc, "Projected", gtiffs[i], sep = "/"),
#      s_srs = fromSRS,
#      t_srs = toSRS,
#      srcnodata = -3000,
#      dstnodata = -3000, 
#      overwrite = T
#    ) # project geotiffs
#    unlink(gtiffs[i]) # delete unprojected geotiffs to save space
#  }
#}
#projHDF2GTiff(
#  loc = "/Volumes/RUAN'S HDD/GIS/Test/",
#  hdfs = "test.hdf"
#)
#
## from http://wanwanliang.net/convert%20MODIS%20hdf%20files%20to%20geotiff%20files.html
#
#p_load(raster, MODISTools, rts, rgdal, gdalUtils)
#
#x <- list.files(
#  path = ".",
#  pattern = ".hdf$",
#  full.names = FALSE
#)
#x
#y <- list()
#for (i in 1:length(x)) {
#  names <- x[i]
#  t <- ".tif"
#  y[i] <- paste(names, t, sep = "")
#  reprojectHDF(
#    hdfName = x[i],
#    file = y[i],
#    MRTpath = "D:/MRT/bin",
#    bands_subset = "1 0 0 0 1 0 0 0 0 0 0 0",
#    resample_type = "NEAREST_NEIGHBOR",
#    proj_type = "UTM",
#    datum = 'WGS84',
#    utm_zone = "17",
#    pixel_size = 1000
#  )
#}
#
#x_loc <- paste0(giswd, "Test/")
#x <- "test.hdf"
#
#reprojectHDF(
#  hdfName = paste0(x_loc, x),
#  filename = paste0(x_loc, "test.tif"),
#  MRTpath = "/Applications/MRT_download_Mactel/MRT_Mactel/bin",
#  bands_subset = "1 0 0 0 1 0 0 0 0 0 0 0",
#  resample_type = "NEAREST_NEIGHBOR",
#  proj_type = "UTM",
#  datum = 'WGS84',
#  utm_zone = "17",
#  pixel_size = 1000
#)
#
#oldwd <- getwd()
#setwd(paste0(giswd, "Test/"))
#
#x <- list.files(
#  path = paste0(giswd, "Test/"),
#  pattern = ".hdf$",
#  full.names = FALSE
#)
#x
#y <- list()
#for (i in 1:length(x)) {
#  names <- x[i]
#  t <- ".tif"
#  y[i] <- paste(names, t, sep = "")
#  reprojectHDF(
#    hdfName = x[i],
#    file = y[i],
#    MRTpath = "/Applications/MRT_download_Mactel/MRT_Mactel/bin",
#    #bands_subset = "1 0 0 0 1 0 0 0 0 0 0 0",
#    #resample_type = "NEAREST_NEIGHBOR",
#    #proj_type = "UTM",
#    #datum = 'WGS84',
#    #utm_zone = "17",
#    pixel_size = 1000
#  )
#}
#
### Not run: 
## Choose the best installation that has both HDF4 and HDF5 drivers:
#gdal_chooseInstallation(hasDrivers=c("HDF4","HDF5"))
## Get the version of this installation:
#getOption("gdalUtils_gdalPath")[[
#  gdal_chooseInstallation(hasDrivers=c("HDF4","HDF5"))]]$version
#gdal_setInstallation("1.11.3")
#
### End(Not run)
#
#setwd(oldwd)
#
#library(gdalUtils)
## Get a list of sds names
#sds <- get_subdatasets("Data/MODIS_fiddling/MOD11B3.A2000032.h19v11.006.2015168203953.hdf")
#
#bn1 <- "MOD_Grid_BRDF:Nadir_Reflectance_Band1"
#b01 <- raster(
#  readGDAL(
#    paste0(
#      "HDF4_EOS:EOS_GRID:","02_HDF_2_mband_geotiff/hdf_in/MCD43A4.A2001033.hdf",":", bn1, sep = "")), #silent = T)
#
#hdf2mband <- function(in.dir, prm.path, out.dir, nbands) {
#  
#  INLIST  <- list.files(path = in.dir, full.names = T)
#  INNAMES <- list.files(path = in.dir, full.names = F)
#  
#  #Manufacure output names with .tif extension
#  OUTNAMES <- INNAMES
#  for (i in 1:length(OUTNAMES)) {
#    OUTNAMES[i] <- gsub(pattern = ".hdf", x = OUTNAMES[i], replacement = ".tif")
#  }
#  for (i in 1:length(INLIST)) {
#    #Call mrt to subset and reproject from sinusoidal to geographic CRS
#    system(
#      command = paste0(
#        paste0("cd ", out.dir, "&&", sep = ""),
#        #Write hdf file to text file
#        paste0("echo ", INLIST[i], " > ", out.dir, "/mosaicinput.txt","&&", sep = ""),
#        #Run mrt mosaic and write output to hdf file
#        paste0(
#          "mrtmosaic -i ", out.dir, "/mosaicinput.txt -o ",
#          out.dir, "/mosaic_tmp.hdf","&&", sep = ""
#        ),
#        #Call resample 
#        paste0(
#          "resample -p ", prm.path, " -i ", out.dir, "/mosaic_tmp.hdf -o ", 
#          out.dir, "/", OUTNAMES[i], "&&", sep = ""
#        ),
#        paste0("exit 0"),
#        sep = ""
#      ),
#      intern = T
#    )
#    print(paste("Done processing:", INNAMES[i]))
#  }
#  
#  #Clean up working files
#  file.remove(paste(out.dir, "/mosaic_tmp.hdf", sep = ""))
#  file.remove(paste(out.dir, "/mosaicinput.txt", sep = ""))
#  file.remove(paste(out.dir, "/resample.log", sep = ""))
#    
#  #Aggregate single bands to multiband rasters
#  band.paths <- list.files(path = out.dir, full.names = T)
#  
#  repeat {
#    stack.i <- stack()
#    for (i in 1:nbands) {
#      stack.i <- addLayer(stack.i, i = read.ras(x = band.paths[i]))
#    }
#    writeRaster(
#      x = stack.i,
#      filename = paste0(out.dir, "/", OUTNAMES[1], sep = ""),
#      format = "GTiff",
#      overwrite = T
#    )
#    file.remove(band.paths[1:nbands])
#    #Truncate done jobs
#    band.paths <- band.paths[-(1:nbands)]
#    OUTNAMES <- OUTNAMES[-1]
#    if (length(band.paths) == 0) {
#      break
#    }
#  }
#}
#
#hdf2mband(
#  in.dir = paste0(datwd, "test.hdf"),
#  prm.path = ".tif",
#  out.dir = datwd,
#  nbands = 1
#)
#
#install.packages("RQGIS", dep = T)
#require(RQGIS)
#find_algorithms(search_term = "Translate")
#my_env <- set_env()
#params <- get_args(alg = "gdalogr:translate", qgis_env = my_env)
#params$INPUT <- paste0(
#  datwd, "MODIS_fiddling/MOD11B3.A2000032.h19v11.006.2015168203953.hdf"
#)
#params$OUTSIZE <- 100
#params$OUTSIZE_PERC <- T
#get_options(alg = "gdalogr:translate", qgis_env = my_env)
#params$OUTPUT <- paste0(
#  datwd, "MODIS_fiddling/output_asdf.tif"
#)
## Run the QGIS API and load its output into R
#run_qgis(
#  alg = "gdalogr:translate",
#  params = params,
#  load_output = params$OUTPUT,
#  qgis_env = my_env
#)


# </> -----------------------------------------------------------------------------------

