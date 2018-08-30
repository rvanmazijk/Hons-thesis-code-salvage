#SDM analysis
#Install packages, if needed, from CRAN and load them
if(!require(pacman)){install.packages("pacman", dependencies=TRUE); library(pacman)}
p_load(virtualspecies, rgbif, rgdal, dismo, maptools, raster, rgeos, foreach, doParallel, gbm, lmodel2, TeachingDemos, devtools, dplyr)

#Set random seed to make things reproducible
set.seed(1234)

#registerDoParallel(cores=2)
cores<-detectCores(all.tests = FALSE, logical = FALSE)
cores

pdf <- "on"
#Set working directory
setwd("~/Desktop/Data")

#Designate the directory to read the PAP data from
pap_dir <- "/Users/michaelcramer/Dropbox/Dropbox_GIS_resources/Protea_Atlas/ProteaData_flat"
#Create a list of species file names for the SDM
list_shp <- list.files(path=pap_dir, pattern="pp.shp$", full.names = TRUE,recursive = FALSE, include.dirs = FALSE)

#Construct a single layer with all points for all species. 
#Note: LDIMMO_pp.shp not readable
#Load the species shapefile. For some reason readOGR cannot read these files. Somthing to do with the multipoint format in which they are stored. So this uses maptools...
temp_lon<- lapply(list_shp, function(sp_names) readShapePoints(sp_names)$LONDD)
temp_lat<- lapply(list_shp, function(sp_names) readShapePoints(sp_names)$LATDD)

#Store all lon lat and species names
occs <- NULL
for (i in 1:length(list_shp))
  {temp <- data.frame(temp_lon[[i]], temp_lat[[i]], substr(list_shp[i], 81, nchar(list_shp[i])-7))
   occs <- rbind(occs, temp)}
names(occs) <- c("lon", "lat", "CODE")
head(occs)
levels(occs$CODE)

#Write  the data.frame as a spatial object
occs_sp <- occs
coordinates(occs_sp) <- ~lon+lat
proj4string(occs_sp) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
occ_sp <- spCbind(occs_sp, occs$CODE)

#Write out the occurrence data 
writeOGR(occ_sp, dsn="Data", layer="occ_sp", driver="ESRI Shapefile", over=TRUE)

#Read in the occurrence data
occ_sp <- readOGR(dsn="Data", layer="occ_sp")
#Convert to data frame
occs <- data.frame(occ_sp)

#Get Southern African borders
SA_border <- readOGR(dsn="GIS_layers/southern_africa_border", layer="southern_africa_border")
SA_border <- spTransform(SA_border, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
plot(SA_border)
points(occ_sp)

#Read in Red data book
red_data <- read.csv("Data/Red_data_book.csv")
head(red_data)
#Extinct (EX) ??? No known individuals remaining
#Extinct in the wild (EW) ??? Known only to survive in captivity, or as a naturalized population outside its historic range
#Critically endangered (CR/CE) ??? Extremely high risk of extinction in the wild
#Endangered (EN) ??? High risk of extinction in the wild
#Vulnerable (VU) ??? High risk of endangerment in the wild
#Near threatened (NT) ??? Likely to become endangered in the near future
#Least concern (LC) ??? Lowest risk; does not qualify for a higher risk category. Widespread and abundant taxa are included in this category.
#Data deficient (DD) ??? Not enough data to make an assessment of its risk of extinction
#Not evaluated (NE) ??? Has not yet been evaluated against the criteria.

#Change all CE values to CR
red_data$Red_data_status[red_data$Red_data_status=="CE"]<-"CR"
#Convert to character and then to factor to remove the spurious CE level.
red_data$Red_data_status <- factor(as.character(red_data$Red_data_status))
head(red_data)

#Now join the two data sets
occs <- left_join(occs, red_data, by="CODE")
head(occs)

#Only keep cases in which Code is not NA
occs <- occs[complete.cases(occs$CODE_no_sub_sp),]

#Get the codes for each species, collapsing all sub-species
(cocs_all <- unique(occs$CODE_no_sub_sp))
head(occs)

#Check that at least threshold occurrences for each species and delete if not
occ<- NULL
threshold <- 100
for (i in 1:length(cocs_all)){
  spoccs <- occs[substr(occs$CODE_no_sub_sp, 1,6) == substr(cocs_all[i],1,6),]
  if (nrow(spoccs)<threshold) next else occ<-rbind(occ, spoccs)}

#Subset out all non-indigenous species
occ <- subset(occ, Alien_indig=="Indig")

#Write out a list of occ to a csv file
write.csv(occ, "Data/occ.csv")
head(occ)
#Read in the stored list ####
occ <- read.csv("occ.csv")
occ <- occ[,2:ncol(occ)]
head(occ)

#Write out a list of codes for those species for which there are more than 100 occurrences known. Note that this immediately removes species that have very small population size.
(cocs <- as.character(unique(occ$CODE_no_sub_sp)))

#Randomly select of about 10% for the training function 
(cocs_test <- sample(cocs, size= length(cocs)/10, replace=FALSE))

#Set these in stone
cocs_test <- c("SEEFFU", "SEMILL", "LSERUB", "LDLANI", "LDCNCM", "SECYGN", "FAROCH", "LDDAPH", "PRSPHL", "LDRUBR", "SEADSC", "PACENT", "LDPLAT", "DIPROT", "SEDODI", "LDSESS", "PRLAUR", "SEGREM", "PRSRFL", "LSPRCX")

#Get the environmental data
#Load the current climate conditions as rasters
#Load all the asc files in the directory with current environmental data
current_fnames <- sort(list.files(path="GIS_layers/Current", pattern=".asc$", full.names = TRUE,recursive = FALSE, include.dirs = FALSE))
cur_predictors <- stack(current_fnames)
names(cur_predictors)

#Temperature vars need to be dvivided by 10
temp_vars <- c(4,5,6, seq(from=15, to=21)) 
for (i in temp_vars){cur_predictors[[i]]<- cur_predictors[[i]]/10}

#Read in Worldclim names See http://www.worldclim.org/bioclim for details. res in minutes
bioclim_names <- c("MAT",  "Mean Diurnal Range",  "Isothermality",	"T Seasonality",	"Max T Warmest Month",	"Min T Coldest Month",	"T Annual Range",	"Mean T Wettest Quarter",	"Mean T Driest Quarter",	"Mean T Warmest Quarter",	"Mean T Coldest Quarter",	"MAP",	"P Wettest Month",	"P Driest Month",	"P Seasonality",	"P of Wettest Quarter",	"P Driest Quarter",	"P Warmest Quarter",	"P Coldest Quarter")

#Generate a list of bioclim names and their codes
bioclim_codes <- data.frame(bioclim_names, paste("bio__", seq(from = 1, to = 19, by = 1), sep=""))
names(bioclim_codes) <- c("Name", "Code")

#Generate a list of non_bioclim names and their codes
non_bioclim <- names(cur_predictors)[which(!names(cur_predictors) %in% bioclim_codes$Code)]

#Append these to the data.frame
bioclim_codes <- data.frame(c(bioclim_names, non_bioclim), c(paste("bio__", seq(from = 1, to = 19, by = 1), sep=""), non_bioclim))
names(bioclim_codes) <- c("Name", "Code")

#Rename the vars
names(cur_predictors) <- bioclim_codes$Name[match(names(cur_predictors), bioclim_codes$Code)]

#Determine which variables are collinear and produce a list of those that are NOT collinear and then only keep the ones that are not collinear
non_collinear_vars <- removeCollinearity(cur_predictors, multicollinearity.cutoff = 0.7, select.variables = TRUE, sample.points = TRUE, nb.points = 10000, plot = TRUE)

#Specify the names of the variables to be kept manually
retained_vars <- c("elevation", "aspect", "slope", "dcoast", "MAT", "Isothermality", "T.Annual.Range", "Mean.T.Warmest.Quarter",  "MAP", "P.Seasonality", "P.Driest.Quarter", "P.Warmest.Quarter",  "BDRICM_d_mean",  "CLYPPT_d_mean", "CEC_d_mean",  "PHIHOX_d_mean",  "ORCDRC_d_mean")

#Ceate a variable with the column numbers that need to be kept
#non_collinear_vars_id <- scan(text=(paste(which(names(cur_predictors) %in% non_collinear_vars), collapse=",")), what = 0L, sep=",")
non_collinear_vars_id <- scan(text=(paste(which(names(cur_predictors) %in% retained_vars), collapse=",")), what = 0L, sep=",")

#Only keep the columns needed
cur_predictors <- subset(cur_predictors, non_collinear_vars_id, drop=FALSE)

#Plot maps of the variables
if (pdf=="on"){pdf(paste("Figures/", "cur_predictors", ".pdf", sep=""), width=10, height=10)}
plot(cur_predictors)
if (pdf=="on"){dev.off()}

#Make a subset of cur_predictors with only the climate variables
climate_vars <- c("MAT", "Isothermality", "T.Annual.Range", "Mean.T.Warmest.Quarter",  "MAP", "P.Seasonality", "P.Driest.Quarter", "P.Warmest.Quarter")

#Ceate a variable with the column numbers that need to be kept
climate_vars_id <- scan(text=(paste(which(names(cur_predictors) %in% climate_vars), collapse=",")), what = 0L, sep=",")

#Only keep the columns needed
cur_predictors_clim <- subset(cur_predictors, climate_vars_id, drop=FALSE)

#Load the future climate predictions as rasters. The code below is simpler than that for current, because we just reuse some of that working
#Load all the asc file in the directory with environmental data
future_fnames <- sort(list.files(path="GIS_layers/Multi_model_mean", pattern=".asc$", full.names = TRUE,recursive = FALSE, include.dirs = FALSE))
future_predictors <- stack(future_fnames)
names(future_predictors)

#Temperature vars need to be dvivided by 10
temp_vars <- c(4,5,6, seq(from=15, to=21)) 
for (i in temp_vars){future_predictors[[i]]<- future_predictors[[i]]/10}

#Rename the vars
names(future_predictors) <- bioclim_codes$Name[match(names(future_predictors), bioclim_codes$Code)]

#Drop the vars that are highly colinear in the current climate data from this data set too
future_predictors <- subset(future_predictors, non_collinear_vars_id, drop=FALSE)

#Make a subset of future_predictors with only the climate variables
future_predictors_clim <- subset(future_predictors, climate_vars_id, drop=FALSE)


#Plot maps of the variables
if (pdf=="on"){pdf(paste("Figures/", "future_predictors", ".pdf", sep=""), width=10, height=10)}
plot(future_predictors)
if (pdf=="on"){dev.off()}

#What changed in climate variables between 2050 and current? Do the substraction
enviro_change <- future_predictors_clim - cur_predictors_clim
names(enviro_change) <- climate_vars

#Plot the actual change
if (pdf=="on"){pdf(paste("Figures/", "enviro_change", ".pdf", sep=""), width=10, height=10)}
#Increase outer margin area so that plots have enough space
par(oma=c(1,2,1,1))
plot(enviro_change, xlab="Lon", ylab="Lat", las=1, col=rev( rainbow(10, start=0,end=1)), legend=TRUE)
if (pdf=="on"){dev.off()}

#Normalise the change to a scale between -1 and 1 and plot all on same scale so you can compare between the variables.
enviro_change_norm <- ((enviro_change-min(enviro_change))/(max(enviro_change)-min(enviro_change)))*2-1
brks<- round(seq(-1, 1,length.out=10),2)
names(enviro_change_norm) <- names(enviro_change)

#Plot the normalised change
if (pdf=="on"){pdf(paste("Figures/", "enviro_change_norm", ".pdf", sep=""), width=10, height=10)}
plot(enviro_change_norm, xlab="Lon", ylab="Lat", las=1, col=rev( rainbow(10, start=0,end=1)), breaks=brks)
if (pdf=="on"){dev.off()}

####This training function is no longer needed since it has already been completed. Continue at ### below
#Create training function for gbm.step
step.train.fx=function(tree.com,learn){
  #set seed for reproducibility
  char2seed("StackOverflow", set = TRUE)
  k1 <- gbm.step(data=data_array, 
                 gbm.x = All_vars, 
                 gbm.y = 3,
                 family = "bernoulli", 
                 tree.complexity = tree.com,
                 learning.rate = learn,
                 bag.fraction = 0.7,
                 prev.stratify=TRUE,
                 n.folds=10,
                 step.size=50,
                 silent=FALSE,
                 plot.main = TRUE,
                 n.cores=cores)
  
  k.out=list(interaction.depth=k1$interaction.depth,
             shrinkage=k1$shrinkage,
             n.trees=k1$n.trees,
             AUC=k1$self.statistics$discrimination,
             cv.AUC=k1$cv.statistics$discrimination.mean,
             deviance=k1$self.statistics$mean.resid,
             cv.deviance=k1$cv.statistics$deviance.mean)  
  return(k.out)
}

#Setup to determine the optimal learning rate and tree complexity 
#define complexity and learning rate
tree.complexity<-c(1:5)
learning.rate<-c(0.01,0.005,0.001,0.0005,0.0001)
head(occs)


train.results_out <- list()
for (i in 1:20){ #length(cocs_test)
  spoccs_test <- occs[substr(occs$CODE_no_sub_sp, 1,6) == substr(cocs_test[i],1,6),]
  occ_test <- data.frame(cbind(spoccs_test[,1], spoccs_test[,2], 1))
  names(occ_test) <- c("lon", "lat", "P_A")
  bg.coords_sp <- data.frame(occs[,1][!occs$CODE_no_sub_sp == cocs_test[i]], occs[,2][!occs$CODE_no_sub_sp == cocs_test[i]], 0)
  names(bg.coords_sp) <- c("lon", "lat", "P_A")
  
  #Subsample a random selection of 5% of the the background points
  bg.coords <- bg.coords_sp[sample(nrow(bg.coords_sp), 0.05*nrow(bg.coords_sp)),]
  pa <- data.frame(rbind(occ_test, bg.coords))
  pa_sp <- as.data.frame(pa)
  coordinates(pa_sp) <- ~lon+lat
  proj4string(pa_sp) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  data_array <- data.frame(pa, extract(cur_predictors, pa_sp, fun=mean, na.rm=TRUE, weights=FALSE, cellnumbers=FALSE, small=FALSE, df=FALSE, layer=1, nl=length(current_fnames), factors=FALSE, sp=FALSE))
  head(data_array)
  
  All_vars <- c(4:ncol(data_array))
  
  #Optimise for the current data
  #setup parallel backend to use n processors
  cl<-makeCluster(cores)
  registerDoParallel(cl)
  
  #Run the actual function
  foreach(l = tree.complexity) %do% {
    foreach(j = learning.rate) %do% {
      nam=paste0("gbm_tc",l,"lr",j)
      assign(nam,step.train.fx(tree.com=l,learn=j))
    }
  }
  
  #Stop parallel
  stopCluster(cl)
  registerDoSEQ()
  
  #disable scientific notation
  options(scipen=999)
  
  #Find all item in workspace that contain "gbm_tc"
  train.all<-ls(pattern="gbm_tc")
  
  #cbind each list that contains "gbm_tc"
  train.results<-list(do.call(cbind,mget(train.all)))
  
  #Place in a data frame
  train.results<- do.call(rbind, lapply(train.results, rbind))
  train.results <- data.frame(matrix(unlist(train.results),ncol=7 , byrow=T))
  
  #Change column names
  colnames(train.results)<-c("TC","LR","n.trees", "AUC", "cv.AUC", "dev", "cv.dev")
  
  #Round 4:7
  train.results[,4:7]<-round(train.results[,4:7],digits=3)

  #Sort by cv.dev, cv.AUC, AUC
  train.results_out[[i]] <-train.results[order(train.results$cv.dev,-train.results$cv.AUC, -train.results$AUC),]
  names(train.results_out[[i]])<-cocs_test[i]
}
  
train.results_df <- train.results_out[[i]]
train.results_df$species <- names(train.results_out[[i]])[1]
colnames(train.results_df)<-c("TC","LR","n.trees", "AUC", "cv.AUC", "dev", "cv.dev", "species")
for (i in 7:10){
  train.results_temp <- data.frame(train.results_out[[i]], names(train.results_out[[i]])[1])
  colnames(train.results_temp)<-c("TC","LR","n.trees", "AUC", "cv.AUC", "dev", "cv.dev", "species")
  train.results_df <- rbind(train.results_df, train.results_temp)
}
write.csv(train.results_df, "/Users/michaelcramer/Dropbox/Bio3013F_2016/Bio3013F_2016_project/Data/train.results_df1_20.csv")

####Continue here after training function

#The original species included by Midgley
#cocs_midgley <- c("DIBUEK", "DIPARI", "DIPROT", "LDCHAM", "LDCINE", "LDCORY", "LDFOED", "LDGALP", "LDLANIN", "LDLEVI", "LDSLLR", "LDTHYM", "LSAREN", "LSHYPOC", "LSMUIR", "LSPARI", "LSRODO", "LSTOME", "PRSPHL", "SEADSC", "SEBROW", "SECAND", "SECYAN", "SEDECI", "SEDECU", "SEFUCI", "SELINE", "SETRIL")

#Additional species
#cocs_add <- c("AUPALL","BRSTEL","DIMYRT","FAGALP","FASALI","LDARGE","LDLAUR","LSCONO","MISTOK","PABRAC","PRCAFF","PRCPCT","PRCYNA","PREXIM","PRMAGN","PRNERI","PRREPE","SOSCAB","SPPROL","VEALPI")
#cocs_select <- c(cocs_midgley, cocs_add)

#This is the full set of species
cocs

#Set the tree complexity and learning rate from the optimisation
tree.com <- 5
learn <- 0.005

#Set the range for i below to encompass the species you need to run. 
for (i in 78:82){ #length(cocs)
  spoccs <- occ[occ$CODE_no_sub_sp == cocs[i],]
  occ_brt <- data.frame(cbind(spoccs[,3], spoccs[,4], 1))
  names(occ_brt) <- c("lon", "lat", "P_A")
  bg.coords_sp <- data.frame(occ[,3][!occ$CODE_no_sub_sp == cocs[i]], occ[,4][!occ$CODE_no_sub_sp == cocs[i]], 0)
  names(bg.coords_sp) <- c("lon", "lat", "P_A")
  
  #Subsample a random selection of 5% of the the background points
  bg.coords <- bg.coords_sp[sample(nrow(bg.coords_sp), 0.05*nrow(bg.coords_sp)),]
  pa <- data.frame(rbind(occ_brt, bg.coords))
  pa_sp <- as.data.frame(pa)
  coordinates(pa_sp) <- ~lon+lat
  proj4string(pa_sp) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  data_array <- data.frame(pa, extract(cur_predictors, pa_sp, fun=mean, na.rm=TRUE, weights=FALSE, cellnumbers=FALSE, small=FALSE, df=FALSE, layer=1, nl=length(current_fnames), factors=FALSE, sp=FALSE))
  head(data_array)
  
All_vars <- c(4:ncol(data_array))

#Now use the optimal results below....
SDM.results <- gbm.step(data=data_array,
                  gbm.x = All_vars,
                  gbm.y = 3,
                  plot.main = TRUE,
                  family = "bernoulli",
                  step.size = 50,
                  tree.complexity = tree.com,
                  learning.rate = learn,
                  prev.stratify = TRUE,
                  max.trees=20000,
                  n.folds = 10,
                  bag.fraction = 0.7,
                  n.cores = cores)
 
(pseudo_r2.results <- 1-(SDM.results$cv.statistics$deviance.mean/SDM.results$self.statistics$mean.null))
summary(SDM.results)

#Simplify models  
SDM.simp <- gbm.simplify(SDM.results, n.drops = "auto") # n.drops = "auto"

#Work out the optimal number of drops (copied from the gbm.simplify code)
y.max <- 1.5 * max(SDM.simp$deviance.summary[,1] + SDM.simp$deviance.summary[,2])
y.min <- 1.5 * min(SDM.simp$deviance.summary[,1] - SDM.simp$deviance.summary[,2])
min.y <- min(c(0, SDM.simp$deviance.summary[,1]))
(min.pos <- match(min.y,c(0,SDM.simp$deviance.summary[,1])) - 1) # subtract one because now zero base

#Contingency: if min.pos really is 0 set to 1....
if (min.pos==0) min.pos <- 1

SDM.simplified.results <- gbm.step(data=data_array,
                                 gbm.x =  SDM.simp$pred.list[[min.pos]], 
                                 gbm.y = 3,
                                 plot.main = TRUE,
                                 family = "bernoulli",
                                 step.size = 50,
                                 tree.complexity = tree.com,
                                 learning.rate = learn,
                                 prev.stratify = TRUE,
                                 max.trees=20000,
                                 n.folds = 10,
                                 bag.fraction = 0.7,
                                 n.cores = cores)

summary(SDM.simplified.results)

#Save the smplified model to disk
file_name <- paste("brt_models/SDM_", cocs[i], sep="")
dput(SDM.simplified.results, file = file_name, control="all")
}

#Once models have been dveloped we can use them for predicting
model_names <- sort(list.files(path="Data/brt_models", full.names = TRUE,recursive = FALSE, include.dirs = FALSE))
(species_names <- substr(model_names, 21, nchar(model_names)) )

#Set up lists to store variables.  
predicted_SDM <- stack()
pred_occ <- list()
future_SDM <- stack()
future_occ <- list()
residuals <- list()
change <- list()
max_dist <-list()

i<-10
for (i in 1:length(model_names)){
#Get the saved models 
brt_model <-dget(model_names[i], keep.source = FALSE)
#Check models
summary(brt_model)
(pseudo_r2_simp.results<- 1-(brt_model$cv.statistics$deviance.mean/brt_model$self.statistics$mean.null))

#Keep a record of which species currently on. Note this is sensitive to directory names
species_code <- substr(model_names[i], 21, nchar(model_names[i]))

#Get the occurrence data (again)
spoccs <- occs[occs$CODE_no_sub_sp == species_code,]
occ_brt <- data.frame(cbind(spoccs[,3], spoccs[,4], 1))
names(occ_brt) <- c("lon", "lat", "P_A")
head(occ_brt)
#Get background coordinates (again)
bg.coords <- data.frame(occs[,3][!occs$CODE_no_sub_sp == cocs[i]], occs[,4][!occs$CODE_no_sub_sp == cocs[i]], 0)
names(bg.coords) <- c("lon", "lat", "P_A")

#Subsample a random selection of 5% of the the background points
bg.coords <- bg.coords[sample(nrow(bg.coords), 0.05*nrow(bg.coords)),]
pa <- data.frame(rbind(occ_brt, bg.coords))
pa_sp <- as.data.frame(pa)
coordinates(pa_sp) <- ~lon+lat
proj4string(pa_sp) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
data_array <- data.frame(pa, extract(cur_predictors, pa_sp, fun=mean, na.rm=TRUE, weights=FALSE, cellnumbers=FALSE, small=FALSE, df=FALSE, layer=1, nl=length(current_fnames), factors=FALSE, sp=FALSE))
head(data_array)

#Evaluate model
brt.eval <- evaluate(p=occ_brt[,1:2], a=bg.coords[,1:2], model=brt_model, n.trees=brt_model$gbm.call$best.trees, x=cur_predictors, type="response")
plot(brt.eval, "ROC")
plot(brt.eval, "TPR")
boxplot(brt.eval)

#Threshold (cut-off) to transform model predictions (probabilities, distances, or similar values) to a binary score (presence or absence) 
#kappa: the threshold at which kappa is highest ("max kappa")
#spec_sens: the threshold at which the sum of the sensitivity (true positive rate) and specificity (true negative rate) is highest
#no_omission: the highest threshold at which there is no omission
#prevalence: modeled prevalence is closest to observed prevalence
#equal_sens_spec: equal sensitivity and specificity
#sensitivty: fixed (specified) sensitivity
(brt_threshold <- threshold(brt.eval))

#Write out the occurrence data data.frame as a spatial object
occ_brt_sp <- occ_brt
coordinates(occ_brt_sp) <- ~lon+lat
proj4string(occ_brt_sp) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#Predict to current environmental from boosted regression tree
predicted_SDM <- predict(cur_predictors, brt_model, n.trees= brt_model$gbm.call$best.trees, type="response", progress="text")

#Store raster in output file as GTiff
writeRaster(predicted_SDM, paste("Current_preds/predicted_SDM_", species_code, sep=""), format="GTiff", overwrite=TRUE) 

#Plot the predicted envelope for the occurence data
plot(predicted_SDM, xlim=c(round(min(occ_brt$lon)-1,0), round(max(occ_brt$lon)+1,0)), ylim=c(round(min(occ_brt$lat)-1,0), round(max(occ_brt$lat)+1,0)))
points(occ_brt, pch=16, cex=0.5)

#Use the thresholds to exclude all values from predictions that are below the threshold
predicted_SDM_t <- predicted_SDM
predicted_SDM_t[predicted_SDM_t < brt_threshold$sensitivity]<-NA

plot(predicted_SDM_t, xlim=c(round(min(occ_brt$lon)-1,0), round(max(occ_brt$lon)+1,0)), ylim=c(round(min(occ_brt$lat)-1,0), round(max(occ_brt$lat)+1,0)))
points(occ_brt, pch=16, cex=0.5)

#Convert to a presence only (binary) map for predicted conditions
predicted_SDM_binary <- predicted_SDM_t
predicted_SDM_binary[predicted_SDM_t>brt_threshold$sensitivity]<-1
predicted_SDM_df <- data.frame(rasterToPoints(predicted_SDM_binary))[,1:2]
names(predicted_SDM_df) <- c("lon", "lat")
predicted_SDM_sp <- predicted_SDM_df
coordinates(predicted_SDM_sp) <- ~lon+lat
proj4string(predicted_SDM_sp) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#Put a hull around the predicted points
predicted_chull <- convHull(predicted_SDM_df)

#Calculate centroid of predicted polygon
predicted_centroid <- data.frame(gCentroid(predicted_chull@polygons))

#Calculate centroid of points (median)
predicted_median_centroid <- data.frame(median(predicted_SDM_df$lon), median(predicted_SDM_df$lat))

#Calculate range size im km
#Produce a matrix of distances from each other
predicted_dist <- spDists(predicted_SDM_sp, predicted_SDM_sp, longlat=TRUE)
(max_predicted_dist <- predicted_dist[which(predicted_dist==max(predicted_dist), arr.ind=T)[1,1], which(predicted_dist==max(predicted_dist), arr.ind=T)[1,2]])

#Latitudinal and longitudinal ranges
(predicted_N_S <- max(predicted_SDM_df$lat) - min(predicted_SDM_df$lat))
(predicted_E_W <- max(predicted_SDM_df$lon) - min(predicted_SDM_df$lon))

#Predict to future environmental from boosted regression tree
future_SDM <- predict(future_predictors, brt_model, n.trees= brt_model$gbm.call$best.trees, type="response", progress="text")
names(future_predictors)

#Store raster in output file as GTiff
writeRaster(future_SDM, paste("Future_preds/future_SDM_", species_code, sep=""), format="GTiff", overwrite=TRUE) 

#Plot the predicted envelope for the future
plot(future_SDM, xlim=c(round(min(occ_brt$lon)-1,0), round(max(occ_brt$lon)+1,0)), ylim=c(round(min(occ_brt$lat)-1,0), round(max(occ_brt$lat)+1,0)))
points(occ_brt, pch=16, cex=0.5) 

#Keep only points for which predictions exceed the threshold determined for current predictions.
future_SDM_t[future_SDM_t < brt_threshold$sensitivity]<-NA

plot(future_SDM_t, xlim=c(round(min(occ_brt$lon)-1,0), round(max(occ_brt$lon)+1,0)), ylim=c(round(min(occ_brt$lat)-1,0), round(max(occ_brt$lat)+1,0)))
points(occ_brt, pch=16, cex=0.5)

#Convert to a presence only (binary) map for future conditions
future_SDM_binary<-future_SDM_t
future_SDM_binary[future_SDM_t>brt_threshold$sensitivity]<-1
future_SDM_df <- data.frame(rasterToPoints(future_SDM_binary))[,1:2]
names(future_SDM_df) <- c("lon", "lat")
future_SDM_sp <- future_SDM_df
coordinates(future_SDM_sp) <- ~lon+lat
proj4string(future_SDM_sp) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


#Put a hull around the future points
future_chull <- convHull(future_SDM_df)


#Calculate centroid of future polygon
future_centroid <- data.frame(gCentroid(future_chull@polygons))

#Calculate centroid of future points (median)
future_median_centroid <- data.frame(median(future_SDM_df$lon), median(future_SDM_df$lat))


#Calculate range size im km
#Produce a matrix of distances from each other
future_dist <- spDists(future_SDM_sp, future_SDM_sp, longlat=TRUE)
(max_future_dist <- future_dist[which(future_dist==max(future_dist), arr.ind=T)[1,1], which(future_dist==max(future_dist), arr.ind=T)[1,2]])

#Latitudinal and longitudinal ranges
(Future_N_S <- max(future_SDM_df$lat) - min(future_SDM_df$lat))
(Future_E_W <- max(future_SDM_df$lon) - min(future_SDM_df$lon))

#Use only points in the current climate predictions that exceed threshold and substract those from the future. The result is the modelled distribution change, rather than that based on current points
change_raster <- future_SDM_t - predicted_SDM_t
cellStats(change_raster, mean)       

plot(change_raster, xlim=c(round(min(occ_brt$lon)-1,0), round(max(occ_brt$lon)+1,0)), ylim=c(round(min(occ_brt$lat)-1,0), round(max(occ_brt$lat)+1,0)))
points(occ_brt, pch=16, cex=0.5)

#Plot the points
dev.off()
plot(ZA_border, xlim=c(round(min(predicted_SDM_df$lon)-0.25,0), round(max(predicted_SDM_df$lon)+0.25,0)), ylim=c(round(min(predicted_SDM_df$lat)-0.25,0), round(max(predicted_SDM_df$lat)+0.25,0)))
points(occ_brt_sp, cex=0.2, pch=21)
points(predicted_SDM_sp, cex=0.5, pch=21, col="red")
lines(predicted_chull@polygons, col="red")
points(predicted_centroid, cex=5, pch=1, col="red")
points(predicted_median_centroid, cex=5, pch=1, col="red")
points(future_SDM_sp, cex=0.5, pch=21, col="blue")
lines(future_chull@polygons, col="blue")
points(future_centroid, cex=5, pch=1, col="blue")
points(future_median_centroid, cex=5, pch=1, col="blue")

#Get the environmental predictions per occurrence point
pred_occ <- extract(predicted_SDM, occ_brt_sp, fun=mean, na.rm=TRUE, weights=FALSE, cellnumbers=FALSE, small=FALSE, df=FALSE, factors=FALSE, sp=FALSE)

#Get the environmental predictions for this changed environment per occurrence point
future_occ <- extract(future_SDM, occ_brt_sp, fun=mean, na.rm=TRUE, weights=FALSE, cellnumbers=FALSE, small=FALSE, df=FALSE, factors=FALSE, sp=FALSE)

#Write out a csv with the data for this species with current versus future pedictions
C_P <- data.frame(pred_occ, future_occ)
names(C_P) <- c("Current", "Future")
write.csv(C_P, paste("Current_future/C_P_", species_code, ".csv", sep="")) 

#Plot the current versus future
plot(pred_occ, future_occ)
abline(0, 1, col="blue")

#Use RMA to get the relationship between current and future
#Combine data into a dataframe.
model2data <- data.frame(pred_occ, future_occ)
model2 <- lmodel2(future_occ ~ pred_occ, data = model2data, range.y="relative", range.x="relative", nperm=99)
#Instead of using default plotting use abline with RMA slope and intercept
abline(model2$regression.results[4,2], model2$regression.results[4,3])

#Calculate residual per occurrence point in which a positive value means future > current and negative means future < current
residuals <- data.frame(future_occ-pred_occ)
#Combine residuals with occurrence data.frame
occ_sp_resid <- spCbind(occ_brt_sp, residuals)

names(occ_sp_resid) <- c("occ", "residuals")
spplot(occ_sp_resid, zcol=c("residuals"))

#Calculate the  change in the distribution probability.
(change_occ <- mean(residuals[,1]))
#Calculate the percentage change in the distribution relative to current.
(change <- sum(residuals, na.rm =TRUE)*100/sum(pred_occ, na.rm =TRUE))


#Calculate range size im km
#Produce a matrix of distances from each other
dist <- spDists(occ_brt_sp, occ_brt_sp, longlat=TRUE)
(max_dist <- dist[which(dist==max(dist), arr.ind=T)[1,1], which(dist==max(dist), arr.ind=T)[1,2]])

chull <- convHull(occ_brt[,1:2])
plot(occ_brt_sp)
summary(chull)

lines(chull@polygons)

help(dismo)

(max_dist_sp <- data.frame(cocs, unlist(max_dist)))
clip <- pipe("pbcopy", "w")                       
write.table(max_dist_sp, sep="\t", file=clip)                           
close(clip)

#Do predictions against 5C T increase and 50% reduction in precip
#Individual alteration of climatic variuables
#Add 5C
t_env <- stack(env$Elevation, env$Slope, env$Distance_coast, env$BDRICM_d_mean, env$bio__1+5, env$bio__2, env$bio__4, env$bio__12, env$bio__17, env$bio__19, env$BLD_d_mean, env$CEC_d_mean, env$ORCDRC_d_mean, env$PHIHOX_d_mean, env$CLYPPT_d_mean, env$SLTPPT_d_mean)
#plot(t_env)

#50% less Bio-12
p_env <- stack(env$Elevation, env$Slope, env$Distance_coast, env$BDRICM_d_mean, env$bio__1, env$bio__2, env$bio__4, env$bio__12*0.75, env$bio__17*0.75, env$bio__19*0.75, env$BLD_d_mean, env$CEC_d_mean, env$ORCDRC_d_mean, env$PHIHOX_d_mean, env$CLYPPT_d_mean, env$SLTPPT_d_mean)


#Set up lists to store variables.  
t_SDM <- stack()
t_occ <- list()
p_SDM <- stack()
p_occ <- list()
t_residuals <- list()
t_change <- list()
p_residuals <- list()
p_change <- list()

for (i in 1:length(cocs)){
#Individual alteration of climatic variuables
  
#Get the occurrence data (again)
spoccs <- occs[occs$CODE == cocs[i],]
occ <- data.frame(cbind(spoccs[,1], spoccs[,2], 1))
names(occ) <- c("lon", "lat", "P_A")

#Write out the occurrence data data.frame as a spatial object
occ_sp <- NULL
occ_sp <- occ
coordinates(occ_sp) <- ~lon+lat
proj4string(occ_sp) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
plot(occ_sp)

#Add 5C
t_SDM <- stack(t_SDM, predict(t_env, SDM.simplified.results[[i]], n.trees= SDM.simplified.results[[i]]$gbm.call$best.trees, type="response", progress="text"))
plot(t_SDM[[i]], xlim=c(round(min(occ$lon)-1,0), round(max(occ$lon)+1,0)), ylim=c(round(min(occ$lat)-1,0), round(max(occ$lat)+1,0)))
points(occ, pch=16, cex=0.5)   
#Get the environmental predictions for this changed environment per occurrence point
t_occ[[i]] <- extract(t_SDM[[i]], occ_sp, fun=mean, na.rm=TRUE, weights=FALSE, cellnumbers=FALSE, small=FALSE, df=FALSE, factors=FALSE, sp=FALSE)


#50% less Bio-19
p_SDM<- stack(p_SDM,predict(p_env, SDM.simplified.results[[i]], n.trees= SDM.simplified.results[[i]]$gbm.call$best.trees, type="response", progress="text"))
plot(p_SDM[[i]], xlim=c(round(min(occ$lon)-1,0), round(max(occ$lon)+1,0)), ylim=c(round(min(occ$lat)-1,0), round(max(occ$lat)+1,0)))
points(occ, pch=16, cex=0.5)   
#Get the environmental predictions for this changed environment per occurrence point
p_occ[[i]] <- extract(p_SDM[[i]], occ_sp, fun=mean, na.rm=TRUE, weights=FALSE, cellnumbers=FALSE, small=FALSE, df=FALSE, factors=FALSE, sp=FALSE)

#Plot the current versus future temperature
plot(pred_occ[[i]], t_occ[[i]])
abline(0, 1, col="blue")
#Use RMA to get the relationship between current and future
#Combine data into a dataframe.
model2data <- data.frame(pred_occ[[i]], t_occ[[i]])
model2 <- lmodel2(t_occ[[i]] ~ pred_occ[[i]], data = model2data, range.y="relative", range.x="relative", nperm=99)
#Instead of using default plotting use abline with RMA slope and intercept
abline(model2$regression.results[4,2], model2$regression.results[4,3])


#Calculate residual in which a positive value means future > current and negative means future < current
t_residuals[[i]] <- t_occ[[i]]-pred_occ[[i]]
#Combine residuals with occurrence data.frame
t_occ_sp_resid <- spCbind(occ_sp, residuals[[i]])
names(t_occ_sp_resid) <- c("occ", "residuals")
spplot(t_occ_sp_resid, zcol=c("residuals"))
#Calculate the percentage change in the distribution relative to current.
(t_change[[i]] <- sum(t_residuals[[i]], na.rm =TRUE)*100/sum(pred_occ[[i]], na.rm =TRUE))

#Plot the current versus future precipitation
plot(pred_occ[[i]], p_occ[[i]])
x <- c(min(pred_occ[[i]]), max(pred_occ[[i]]))
y <- model2$regression.results[4,2]*x+model2$regression.results[4,3]
lines(x, y)
abline(0, 1, col="blue")
#Use RMA to get the relationship between current and future
#Combine data into a dataframe.
model2data <- data.frame(pred_occ[[i]], p_occ[[i]])
model2 <- lmodel2(p_occ[[i]] ~ pred_occ[[i]], data = model2data, range.y="relative", range.x="relative", nperm=99)
#Instead of using default plotting use abline with RMA slope and intercept
abline(model2$regression.results[4,2], model2$regression.results[4,3])


#Calculate residual in which a positive value means future > current and negative means future < current
p_residuals[[i]] <- p_occ[[i]]-pred_occ[[i]]
#Combine residuals with occurrence data.frame
p_occ_sp_resid <- spCbind(occ_sp, residuals[[i]])
names(p_occ_sp_resid) <- c("occ", "residuals")
spplot(p_occ_sp_resid, zcol=c("residuals"))
#Calculate the percentage change in the distribution relative to current.
(p_change[[i]] <- sum(p_residuals[[i]], na.rm =TRUE)*100/sum(pred_occ[[i]], na.rm =TRUE))
}

(t_change_sp <- data.frame(cocs, unlist(t_change)))
clip <- pipe("pbcopy", "w")                       
write.table(t_change_sp, sep="\t", file=clip)                           
close(clip)
(p_change_sp <- data.frame(cocs, unlist(p_change)))
clip <- pipe("pbcopy", "w")                       
write.table(p_change_sp, sep="\t", file=clip)                           
close(clip)