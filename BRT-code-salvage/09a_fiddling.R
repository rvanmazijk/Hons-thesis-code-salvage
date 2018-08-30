# Fiddling ---------------------------------------------------------------------

# Apply that mega_gbm_step_loop_ijk() to SWAFR & GCFR in parallel?
# Nah, rather do tolerances in parallel, and do regions in sequence

#cluster <- makeCluster(detectCores() - 1, outfile = "")
#clusterMap(cluster,
#    data = list(
#        as.data.frame(GCFR_tibble),
#        as.data.frame(SWAFR_tibble)
#    ),
#    out_dir = list(
#        out_dir_GCFR,
#        out_dir_SWAFR
#    ),
#    fun = mega_gbm_step_loop_ijk,
#    gbm.x = 2:37,
#    gbm.y = 1,
#    tolerances = tolerances,
#    tree_complexities = tree_complexities,
#    learning_rates = learning_rates
#)
#stopCluster(cluster)

practice <- function() {

    # Protea-SDMs-esque set up
    foo <- dismo::gbm.step(
        data            = as.data.frame(GCFR_tibble),
        gbm.x           = c(2:37),
        gbm.y           = 1,
        family          = "gaussian",
        tree.complexity = 5,
        learning.rate   = 0.01,
        bag.fraction    = 0.7,
        prev.stratify   = TRUE,
        n.folds         = 10,
        step.size       = 50,
        silent          = FALSE,
        plot.main       = TRUE,
        plot.folds      = TRUE
    )

    # 2017-09-26 set up
    foo <- dismo::gbm.step(
        data            = as.data.frame(GCFR_tibble),
        gbm.x           = c(2:37),
        gbm.y           = 1,
        family          = "gaussian",
        tree.complexity = 5,
        learning.rate   = 0.01,
        bag.fraction    = 0.7,
        prev.stratify   = TRUE,
        n.folds         = 10,
        silent          = FALSE,
        plot.main       = TRUE,
        plot.folds      = TRUE,
        n.trees         = 1,     # !! new !!
        step.size       = 1,     # !!
        max.trees       = 5000   # !!
    )

    # 2017-09-26 set up 2
    foo <- dismo::gbm.step(
        data             = as.data.frame(GCFR_tibble),
        gbm.x            = c(2:37),
        gbm.y            = 1,
        family           = "gaussian",
        tree.complexity  = 2,
        learning.rate    = 0.01,
        bag.fraction     = 0.75,   # the default
        prev.stratify    = TRUE,
        n.folds          = 10,
        silent           = FALSE,
        plot.main        = TRUE,
        n.trees          = 1,      # !! new !!
        step.size        = 1,      # !!
        max.trees        = 5000,   # !!
        tolerance.method = "auto",
        tolerance        = 0.00000725  # tiny multiple of total mean deviance
        #tolerance        = 0.000007282450255  # tiny multiple of total mean deviance
    )

    # 2017-09-26 set up 3
    foo <- dismo::gbm.step(
        data             = as.data.frame(GCFR_tibble),
        gbm.x            = c(2:37),
        gbm.y            = 1,
        family           = "gaussian",
        tree.complexity  = 2,
        learning.rate    = 0.01,
        bag.fraction     = 0.75,   # the default
        prev.stratify    = TRUE,
        n.folds          = 10,
        silent           = FALSE,
        plot.main        = TRUE,
        n.trees          = 1,      # !! new !!
        step.size        = 1,      # !!
        max.trees        = 5000,   # !!
        tolerance.method = "auto",
        tolerance        = NULL  # tiny multiple of total mean deviance
        #tolerance        = 0.000007282450255  # tiny multiple of total mean deviance
    )

}

practice_old <- function() {

    foo <- train_BRT(
        data = as.data.frame(GCFR_tibble),
        gbm_x = 2:37,
        gbm_y = 1,
        tree_complexity = 1,
        learning_rate = 0.01
    )
    summary(foo$raw)
    names(foo$raw)
    gbm.plot(foo$raw)

    foo_simp <- gbm.simplify(foo$raw)

    foo_looped <- train_BRT_loop(
        data = as.data.frame(GCFR_tibble),
        gbm_x = 2:37,
        gbm_y = 1,
        tree_complexities = 1:10,
        learning_rates = c(0.01, 0.005, 0.001, 0.0005, 0.0001)
    )
    foo_looped %>% map(map, summary)

}

# From protea-SDMs -------------------------------------------------------------

tree_complexity <- 1:5
learning.rate <- c(0.01, 0.005, 0.001, 0.0005, 0.0001)

training_results <- vector("list")
#length(cocs_test)
for (i in 1:20) {  # FIXME: why 20? what is `length(cocs_test)?`
    spoccs_test <- occs[
        substr(occs$CODE_no_sub_sp, 1, 6) == substr(cocs_test[i], 1, 6),
        ]
    occ_test <- data.frame(cbind(spoccs_test[, 1], spoccs_test[, 2], 1))
    names(occ_test) <- c("lon", "lat", "P_A")
    bg.coords_sp <- data.frame(
        occs[, 1][!occs$CODE_no_sub_sp == cocs_test[i]],
        occs[, 2][!occs$CODE_no_sub_sp == cocs_test[i]],
        0
    )
    names(bg.coords_sp) <- c("lon", "lat", "P_A")

    #Subsample a random selection of 5% of the the background points
    bg.coords <-
        bg.coords_sp[sample(nrow(bg.coords_sp), 0.05 * nrow(bg.coords_sp)), ]
    pa <- data.frame(rbind(occ_test, bg.coords))
    pa_sp <- as.data.frame(pa)
    coordinates(pa_sp) <- ~ lon + lat
    proj4string(pa_sp) <-
        "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    data_array <-
        data.frame(
            pa,
            extract(
                cur_predictors,
                pa_sp,
                fun = mean,
                na.rm = TRUE,
                weights = FALSE,
                cellnumbers = FALSE,
                small = FALSE,
                df = FALSE,
                layer = 1,
                nl = length(current_fnames),
                factors = FALSE,
                sp = FALSE
            )
        )
    head(data_array)

    All_vars <- c(4:ncol(data_array))

    #Optimise for the current data
    #setup parallel backend to use n processors
    cl <- makeCluster(cores)
    registerDoParallel(cl)

    #Run the actual function
    foreach(l = tree.complexity) %do% {
        foreach(j = learning.rate) %do% {
            nam = paste0("gbm_tc", l, "lr", j)
            assign(nam, step.train.fx(tree.com = l, learn = j))
        }
    }

    #Stop parallel
    stopCluster(cl)
    registerDoSEQ()

    #disable scientific notation
    options(scipen = 999)

    #Find all item in workspace that contain "gbm_tc"
    train.all <- ls(pattern = "gbm_tc")

    #cbind each list that contains "gbm_tc"
    train.results <- list(do.call(cbind, mget(train.all)))

    #Place in a data frame
    train.results <- do.call(rbind, lapply(train.results, rbind))
    train.results <-
        data.frame(matrix(unlist(train.results), ncol = 7 , byrow = T))

    #Change column names
    colnames(train.results) <-
        c("TC", "LR", "n.trees", "AUC", "cv.AUC", "dev", "cv.dev")

    #Round 4:7
    train.results[, 4:7] <- round(train.results[, 4:7], digits = 3)

    #Sort by cv.dev, cv.AUC, AUC
    training_results[[i]] <-
        train.results[order(train.results$cv.dev,
                            -train.results$cv.AUC,
                            -train.results$AUC), ]
    names(train.results_out[[i]]) <- cocs_test[i]
}

train.results_df <- train.results_out[[i]]
train.results_df$species <- names(train.results_out[[i]])[1]
colnames(train.results_df) <-
    c("TC",
      "LR",
      "n.trees",
      "AUC",
      "cv.AUC",
      "dev",
      "cv.dev",
      "species")
for (i in 7:10) {
    train.results_temp <-
        data.frame(train.results_out[[i]], names(train.results_out[[i]])[1])
    colnames(train.results_temp) <-
        c("TC",
          "LR",
          "n.trees",
          "AUC",
          "cv.AUC",
          "dev",
          "cv.dev",
          "species")
    train.results_df <- rbind(train.results_df, train.results_temp)
}
write.csv(
    train.results_df,
    "/Users/michaelcramer/Dropbox/Bio3013F_2016/Bio3013F_2016_project/Data/train.results_df1_20.csv"
)

i <- 1
#Set the range for i below to encompass the species you need to run.
for (i in 1:length(genera)) {
    #length(cocs)
    spoccs <- occ[occ$Genus_code %in% genera[i], ]
    occ_brt <- data.frame(cbind(spoccs[, 3], spoccs[, 4], 1))
    names(occ_brt) <- c("lon", "lat", "P_A")
    bg.coords_sp <-
        data.frame(occ[, 3][!occ$Genus_code %in% genera[i]], occ[, 4][!occ$Genus_code %in% genera[i]], 0)
    names(bg.coords_sp) <- c("lon", "lat", "P_A")

    #Subsample a random selection of 5% of the the background points
    bg.coords <-
        bg.coords_sp[sample(nrow(bg.coords_sp), 0.05 * nrow(bg.coords_sp)), ]
    pa <- data.frame(rbind(occ_brt, bg.coords))
    pa_sp <- as.data.frame(pa)
    coordinates(pa_sp) <- ~ lon + lat
    proj4string(pa_sp) <-
        "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    data_array <-
        data.frame(
            pa,
            extract(
                cur_predictors,
                pa_sp,
                fun = mean,
                na.rm = TRUE,
                weights = FALSE,
                cellnumbers = FALSE,
                small = FALSE,
                df = FALSE,
                layer = 1,
                nl = length(current_fnames),
                factors = FALSE,
                sp = FALSE
            )
        )
    head(data_array)

    All_vars <- c(4:ncol(data_array))

    #Now use the optimal results below....
    SDM.results <- gbm.step(
        data = data_array,
        gbm.x = All_vars,
        gbm.y = 3,
        plot.main = TRUE,
        family = "bernoulli",
        step.size = 50,
        tree.complexity = tree.com,
        learning.rate = learn,
        prev.stratify = TRUE,
        max.trees = 20000,
        n.folds = 10,
        bag.fraction = 0.7,
        n.cores = cores
    )

    (
        pseudo_r2.results <-
            1 - (
                SDM.results$cv.statistics$deviance.mean / SDM.results$self.statistics$mean.null
            )
    )
    summary(SDM.results)

    #Simplify models
    SDM.simp <-
        gbm.simplify(SDM.results, n.drops = "auto") # n.drops = "auto"

    #Work out the optimal number of drops (copied from the gbm.simplify code)
    y.max <-
        1.5 * max(SDM.simp$deviance.summary[, 1] + SDM.simp$deviance.summary[, 2])
    y.min <-
        1.5 * min(SDM.simp$deviance.summary[, 1] - SDM.simp$deviance.summary[, 2])
    min.y <- min(c(0, SDM.simp$deviance.summary[, 1]))
    (min.pos <-
            match(min.y, c(0, SDM.simp$deviance.summary[, 1])) - 1) # subtract one because now zero base

    #Contingency: if min.pos really is 0 set to 1....
    if (min.pos == 0)
        min.pos <- 1

    SDM.simplified.results <- gbm.step(
        data = data_array,
        gbm.x =  SDM.simp$pred.list[[min.pos]],
        gbm.y = 3,
        plot.main = TRUE,
        family = "bernoulli",
        step.size = 50,
        tree.complexity = tree.com,
        learning.rate = learn,
        prev.stratify = TRUE,
        max.trees = 20000,
        n.folds = 10,
        bag.fraction = 0.7,
        n.cores = cores
    )

    summary(SDM.simplified.results)
    SDM.simplified.results <- SDM.results
    #Save the smplified model to disk
    file_name <-
        paste("Data/Ruan/brt_models_genus/SDM_", genera[i], sep = "")
    dput(SDM.simplified.results,
         file = file_name,
         control = "all")
}

###Setup to run model predictions
#Set pdf to on
pdf <- "on"

#Once models have been dveloped we can use them for predicting
model_names <-
    sort(
        list.files(
            path = "Data/Ruan/brt_models_genus",
            full.names = TRUE,
            recursive = FALSE,
            include.dirs = FALSE
        )
    )
(species_names <- substr(model_names, 32, nchar(model_names)))

#Read in the stored list of occurrences
occ <- read.csv("Data/occ.csv")
occ <- occ[, 2:ncol(occ)]
head(occ)
#Write out a list of codes for those genera for which there are more than 100 occurrences known. Note that this immediately removes species that have very small population size.
(genera <- as.character(unique(occ$Genus_code)))

#Load the landscape transformation mask so that it can be used as a mask
ltransform <- raster("GIS_layers/LC13_GTI_120_NvsT.tif")
ltransform <-
    projectRaster(ltransform, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#Set all 0 (transformed areas) to NA. 1 = natural areas
ltransform[ltransform == 0] <- NA
plot(ltransform)

#Set up lists to store variables.
models_summary <- list()
species_output <- list()
species_output_ltf <- list()
enviro_list <- list()

model_sum <- data.frame(matrix(nrow = 1, ncol = 3))
names(model_sum) <- c("species_code", "var", "rel.inf")
i <- 1
for (i in 1:length(model_names)) {
    #length(model_names)
    #Keep a record of which species currently on. Note this is sensitive to directory names
    (species_code <- substr(model_names[i], 32, nchar(model_names[i])))

    #Get the saved models
    brt_model <- dget(model_names[i], keep.source = FALSE)

    #Check models
    models_summary[[i]] <- data.frame(species_code, summary(brt_model))
    model_sum <- rbind(model_sum, models_summary[[i]])


    (
        pseudo_r2_simp.results <-
            1 - (
                brt_model$cv.statistics$deviance.mean / brt_model$self.statistics$mean.null
            )
    )

    #Get the occurrence data for this species
    spoccs <- occ[occ$Genus_code == species_code, ]
    occ_brt <- data.frame(cbind(spoccs[, 3], spoccs[, 4], 1))
    names(occ_brt) <- c("lon", "lat", "P_A")
    #head(occ_brt)

    #Get background coordinates for this species
    bg.coords <-
        data.frame(occ[, 3][!occ$Genus_code == genera[i]], occ[, 4][!occ$Genus_code == genera[i]], 0)
    names(bg.coords) <- c("lon", "lat", "P_A")

    #Subsample a random selection of 5% of the the background points
    bg.coords <-
        bg.coords[sample(nrow(bg.coords), 0.05 * nrow(bg.coords)), ]
    pa <- data.frame(rbind(occ_brt, bg.coords))
    pa_sp <- as.data.frame(pa)
    coordinates(pa_sp) <- ~ lon + lat
    proj4string(pa_sp) <-
        "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    data_array <-
        data.frame(
            pa,
            extract(
                cur_predictors,
                pa_sp,
                fun = mean,
                na.rm = TRUE,
                weights = FALSE,
                cellnumbers = FALSE,
                small = FALSE,
                df = FALSE,
                layer = 1,
                nl = length(current_fnames),
                factors = FALSE,
                sp = FALSE
            )
        )
    #head(data_array)

    #Evaluate model
    (
        brt.eval <-
            evaluate(
                p = occ_brt[, 1:2],
                a = bg.coords[, 1:2],
                model = brt_model,
                n.trees = brt_model$gbm.call$best.trees,
                x = cur_predictors,
                type = "response"
            )
    )
    #plot(brt.eval, "ROC")
    #plot(brt.eval, "TPR")
    #boxplot(brt.eval)

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
    coordinates(occ_brt_sp) <- ~ lon + lat
    proj4string(occ_brt_sp) <-
        "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    extent(occ_brt_sp)
    #Predict to current environmental from boosted regression tree
    predicted_SDM <-
        predict(
            cur_predictors,
            brt_model,
            n.trees = brt_model$gbm.call$best.trees,
            type = "response",
            progress = "text"
        )

    #Crop to an extent that is inflated by 1 degree in all directions
    predicted_SDM_crop <- crop(predicted_SDM, extent(occ_brt_sp) + 2)
    #Crop the land transformation raster to same extent, resample to the same resolution and mask the prediction with the transformation raster. NA values will indicate transformed land.
    ltransform_crop <- crop(ltransform, predicted_SDM_crop)
    ltransform_crop <-
        resample(ltransform_crop, predicted_SDM_crop, method = "ngb")
    predicted_SDM_crop_ltf <- mask(predicted_SDM_crop, ltransform_crop)

    #Store raster in output file as GTiff
    writeRaster(
        predicted_SDM_crop,
        paste(
            "Data/Ruan/Current_preds_genus/predicted_SDM_",
            species_code,
            sep = ""
        ),
        format = "GTiff",
        overwrite = TRUE
    )
    writeRaster(
        predicted_SDM_crop_ltf,
        paste(
            "Data/Ruan/Current_preds_ltf_genus/predicted_SDM_",
            species_code,
            "_ltf",
            sep = ""
        ),
        format = "GTiff",
        overwrite = TRUE
    )


    #Plot the predicted envelope for the occurence data limited to 1 degree from actual occurrence data
    if (pdf == "on") {
        pdf(
            paste(
                "Data/Ruan/predicted_SDM_genus/",
                "predicted_SDM_",
                species_code,
                ".pdf",
                sep = ""
            ),
            width = 10,
            height = 10
        )
    }
    plot(
        predicted_SDM_crop,
        las = 1,
        xlab = expression("Lon (" * degree * ")"),
        ylab = expression("Lat (" * degree * ")"),
        xlim = c(round(min(occ_brt$lon) - 1, 0), round(max(occ_brt$lon) + 1, 0)),
        ylim = c(round(min(occ_brt$lat) - 1, 0), round(max(occ_brt$lat) + 1, 0))
    )
    plot(
        SA_border_simp,
        las = 1,
        xlim = c(round(min(occ_brt$lon) - 1, 0), round(max(occ_brt$lon) + 1, 0)),
        ylim = c(round(min(occ_brt$lat) - 1, 0), round(max(occ_brt$lat) + 1, 0)),
        add = TRUE
    )
    points(occ_brt, pch = 16, cex = 0.5)
    if (pdf == "on") {
        dev.off()
    }

    if (pdf == "on") {
        pdf(
            paste(
                "Data/Ruan/predicted_SDM_ltf_genus/",
                "predicted_SDM_",
                species_code,
                "_ltf.pdf",
                sep = ""
            ),
            width = 10,
            height = 10
        )
    }
    plot(
        predicted_SDM_crop_ltf,
        las = 1,
        xlab = expression("Lon (" * degree * ")"),
        ylab = expression("Lat (" * degree * ")"),
        xlim = c(round(min(occ_brt$lon) - 1, 0), round(max(occ_brt$lon) + 1, 0)),
        ylim = c(round(min(occ_brt$lat) - 1, 0), round(max(occ_brt$lat) + 1, 0))
    )
    plot(
        SA_border_simp,
        las = 1,
        xlim = c(round(min(occ_brt$lon) - 1, 0), round(max(occ_brt$lon) + 1, 0)),
        ylim = c(round(min(occ_brt$lat) - 1, 0), round(max(occ_brt$lat) + 1, 0)),
        add = TRUE
    )
    points(occ_brt, pch = 16, cex = 0.5)
    if (pdf == "on") {
        dev.off()
    }


    # From veg-wars ----------------------------------------------------------------

    brt_results_init <- gbm::gbm.step(
        data = circ_results_final[!is.na(circ_results_final$gr_max_regular_dist),],
        gbm.x           = c(43, 44, 45, 49, 50),  # All the topog vars
        gbm.y           = 21,  # is gr_max_regular_dist
        plot.main       = TRUE,
        family          = "gaussian",
        step.size       = 50,
        tree.complexity = 5,
        learning.rate   = 0.001,
        max.trees       = 10000,
        n.folds         = 10,
        bag.fraction    = 0.5
    )

    save(brt_results_init, file = here("Results", "gr_brt_results_init22.RData"))

    (pseudo_r2_results <- 1 - (
        brt_results_init$cv.statistics$deviance.mean /
            brt_results_init$self.statistics$mean.null
    ))
    summary(brt_results_init)

    # Simplify models
    brt_simp <- gbm.simplify(brt_results_init, n.drops = "auto")
    save(brt_simp, file = here("Results", "gr_brt_simp22.RData"))

    # Work out the optimal number of drops (copied from the gbm.simplify code)
    (y.max <- 1.5 * max(
        brt_simp$deviance.summary[, 1] +
            brt_simp$deviance.summary[, 2]
    ))
    (y.min <- 1.5 * min(
        brt_simp$deviance.summary[, 1] -
            brt_simp$deviance.summary[, 2]
    ))
    (min.y <- min(c(0, brt_simp$deviance.summary[, 1])))
    (min.pos <- match(min.y, c(0, brt_simp$deviance.summary[, 1])) - 1)
    # (subtract one because now zero base)
    # Contingency: if min.pos really is 0 set to 1....
    if (min.pos == 0) min.pos <- 1

    # Run the brt proper with the simplified configuration
    brt_results_simp <- gbm.step(
        data = circ_results_final[!is.na(circ_results_final$gr_max_regular_dist), ],
        gbm.x           = brt_simp$pred.list[[min.pos]],
        gbm.y           = 21,
        plot.main       = TRUE,
        family          = "gaussian",
        step.size       = 50,
        tree.complexity = 5,
        learning.rate   = 0.001,
        max.trees       = 10000,
        n.folds         = 10,
        bag.fraction    = 0.5
    )
    save(brt_results_simp, file = here("Results", "gr_brt_results_simp22.RData"))

    (pseudo_r2.results <- 1 - (
        brt_results_simp$cv.statistics$deviance.mean /
            brt_results_simp$self.statistics$mean.null
    ))
    summary(brt_results_simp)

    # Plot partial dependences
    par(mar = c(4, 4, 1, 1), crt = 90, las = 1)
    gbm.plot(brt_results_simp,
             n.plots      = 4,
             rug          = TRUE,
             x.label      = "",
             y.label      = "",
             write.title  = FALSE,
             plot.layout  = c(2, 2),
             common.scale = FALSE,
             smooth       = FALSE,
             cex.axis     = 1,
             cex.lab      = 1,
             show.contrib = TRUE
    )
