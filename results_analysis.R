################################################################################
#                             EwE results processing                           #
################################################################################
# packages ---------------------------------------------------------------------
require('dplyr')
require('ggplot2')
require('tidyr')
require('cowplot')
require("ggtext")

# functions   ------------------------------------------------------------------
# delete part of the directory for option names
replace_fun <- function(x) { sub("^.{1,144}", "",x)} # Rename each file by removing the first 124 characters of the filename
# get all the options in the output of MC
get_options_MC <- function(fd_MC){
  file_options <- list.files(path = paste0(fd_MC,'mc_output_trial0001/'), pattern='.csv', recursive = TRUE, full.names = TRUE)
  file_options <- lapply(file_options, FUN = replace_fun)
  file_options <- unlist(file_options)
  return(file_options)
}
# assign classes to FGs
assign_class <- function(df){
  # read csv file with class assigned to FG
  classes <- read.csv(paste0(fd_classes,"FG_class.csv"), sep = ';', header = TRUE)
  # add new column to df
  df$class <- NA
  # assign class to each fg based on csv file
  for (sp in unique(classes$Group.name)){
    df$class[which(df$species == sp)] <- classes$class[which(classes$Group.name == sp)]
  }
  return(df)
}
# omit outside the limits
outside_limits <- function(fd_MC_limits,df,var){
  # read csv file with class assigned to FG
  limits <- read.csv(paste0(fd_MC_limits,"MC_",var,"_limits.csv"), sep = ',', header = TRUE)
  limits <- limits[,-c(1,7)]
  limits <- na.omit(limits)

  # find trials out of limits based on csv file
  outliers <- NULL
  for (sp in unique(limits$Group.name)){
      x <- subset(df,species == sp & year == "1991-01-01")
      trial_out <- x$trial[which(x$param < limits$Lower.limit[which(limits$Group.name==sp)]|x$param > limits$Upper.limit[which(limits$Group.name==sp)])]

      # Find outliers
      outliers <- c(outliers,trial_out)

  }

    return(outliers)
}
#select best 90% fits
SS_MC <- function(fd_MC,total_trials,percentage){
  # to get correct numbering for files
  ## !!!check how named of more than 10,000!!!!!!!!!!!
  char_vector <- sprintf("%05d", 1:total_trials)
  ss_mc_combined <- data.frame("trial" = NA, "SS" = NA)

  for (trialn in char_vector) {
    # get list of excel files in folders
    ss_list <- list.files(path = paste0(fd_MC,"mc_input/"), pattern=paste0('mc_trial',trialn,'_Biomass.csv'), recursive = TRUE, full.names = TRUE)
    if (identical(ss_list, character(0))){
      next
    }
    # to delete consumption-biomass files when biomass asked
    #ss_list <- file.list[!grepl(paste0('consumption-',param,'_annual.csv'), file.list)]

    # read them
    ss_mc <- lapply(ss_list, read.csv, skip = 9, header=FALSE)

    # as df, give colnames, add trial number and rbind
    ss_mc_t <- t(as.data.frame(ss_mc)[1:2,])[2,]
    ss_mc_df <- data.frame( "trial" = as.numeric(ss_mc_t[1]), "SS" = as.numeric(ss_mc_t[2]))

    ss_mc_combined <- rbind(ss_mc_combined,ss_mc_df)
  }
  # omit NA values
  ss_mc_combined <- na.omit(ss_mc_combined)
  ss_mc_90 <- ss_mc_combined[order(ss_mc_combined$SS),]

  # Calculate the number of rows to keep (90% of the total)
  n90 <- floor((percentage/100) * nrow(ss_mc_90))

  # Take the 90% lowest values
  ss_mc_90 <<- ss_mc_90[1:n90, ]
}
# find outliers, ie crazy starting values
get_outlier <- function(df){

  outliers_df <- data.frame("year" = NA,   "param" = NA,  "species" = NA, "trial" = NA)

  for (sp in unique(df$species)){
    x <- subset(df,species == sp & year == "1991")
    x <- na.omit(x)
  # Calculate IQR
  Q1 <- quantile(x$param, 0.25, na.rm = TRUE)  # First quartile (25%)
  Q3 <- quantile(x$param, 0.75, na.rm = TRUE)  # Third quartile (75%)
  IQR_value <- IQR(x$param)      # Interquartile range

  # Define lower and upper bounds for outliers
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value

  # Find outliers
  outliers <- x[x$param < lower_bound | x$param > upper_bound,]

  outliers_df <- rbind(outliers_df,outliers)
  }
  outliers_df <- na.omit(outliers_df)
  return(outliers_df)
  }
# retrieve MC output of specific option (eg biomass) -> output of functions is a df with all data for selected option
param_MC <- function(){
  # to get correct numbering for files
  ## !!!check how named of more than 10,000!!!!!!!!!!!
  char_vector <- sprintf("%04d", ss_mc_90$trial)
  # column names or to name the FG correctly
  cols <<- c("year",
            "Harbour porpoise",
            "Seals",
            "Seabirds (discard)",
            "Seabirds (non-discard)",
            "Sharks",
            "Rays",
            "Juvenile Cod",
            "Cod (adult)",
            "Juvenile Whiting",
            "Whiting (adult)",
            "Other gadoids",
            "Demersal fish",
            "Juvenile Herring",
            "Herring (adult)",
            "Sprat",
            "Mackerel",
            "Horse mackerel",
            "Sandeels",
            "Juvenile Plaice",
            "Plaice (adult)",
            "Dab",
            "Other flatfish",
            "Juvenile Sole",
            "Sole (adult)",
            "Sea Bass",
            "Pelagic fish",
            "Squid & cuttlefish",
            "Carnivorous zooplankton",
            "Herbivorous plankton (copepods)",
            "Gelatinous zooplankton",
            "Large crabs + shrimps",
            "Blue mussels (reefs)",
            "Blue mussels (aquaculture)",
            "Epifaunal macrobenthos (mobile grazers)",
            "Infaunal macrobenthos",
            "Crangon",
            "Small mobile epifauna (swarming crustaceans)",
            "Small infauna (polychaetes)",
            "Sessile epifauna",
            "Meiofauna",
            "Phytoplankton",
            "Detritus",
            "Discards")
  # new colmumn to df
  param_mc_combined <- NA

  for (trialn in char_vector) {
    # get list of excel files in folders
    fd1 = paste0(fd_MC,'mc_output_trial',trialn,"/")
    file.list <- list.files(path = fd1, pattern=paste0(param,'_annual.csv'), recursive = TRUE, full.names = TRUE)
    if (identical(file.list, character(0))){
      next
    }
    # to delete consumption-biomass files when biomass asked
    param_list <- file.list[!grepl(paste0('consumption-',param,'_annual.csv'), file.list)]

    # read them
    param_mc <- lapply(param_list, read.csv, skip = 10, header=FALSE)

    # as df, give colnames, add trial number and rbind
    param_mc_df <- as.data.frame(param_mc)
    colnames(param_mc_df) <- cols[1:ncol(param_mc_df)]


    #write code to get all columns below each other with years and colnames :-)
    param_mc_dft <- data.frame("year" = as.Date(NA), "param" = NA, "species" = NA)
    start_date2 <- as.Date("1991-01-01")   # Define start date

    for (i in 2:ncol(param_mc_df)){
      param_mc_dft1 <- param_mc_df[,c(1,i)]
      param_mc_dft1$species <- colnames(param_mc_df)[i]
      param_mc_dft1$year <- seq.Date(from = start_date2, by = "year", length.out = 33)
      colnames(param_mc_dft1) <- c("year","param","species")
      param_mc_dft <- rbind(param_mc_dft,param_mc_dft1)
    }
    param_mc_dft <- na.omit(param_mc_dft)
    param_mc_dft$trial <- trialn
    param_mc_combined <- rbind(param_mc_combined,param_mc_dft)
  }
  # omit NA values
  param_mc_combined <<- na.omit(param_mc_combined)
}
# retrieve monthly MC output of specific option (eg biomass) -> output of functions is a df with all data for selected option
param_monthly_MC <- function(){
  # to get correct numbering for files
  ## !!!check how named of more than 10,000!!!!!!!!!!!
  char_vector <- sprintf("%04d", ss_mc_90$trial)
  # column names or to name the FG correctly
  cols <<- c("year",
             "Harbour porpoise",
             "Seals",
             "Seabirds (discard)",
             "Seabirds (non-discard)",
             "Sharks",
             "Rays",
             "Juvenile Cod",
             "Cod (adult)",
             "Juvenile Whiting",
             "Whiting (adult)",
             "Other gadoids",
             "Demersal fish",
             "Juvenile Herring",
             "Herring (adult)",
             "Sprat",
             "Mackerel",
             "Horse mackerel",
             "Sandeels",
             "Juvenile Plaice",
             "Plaice (adult)",
             "Dab",
             "Other flatfish",
             "Juvenile Sole",
             "Sole (adult)",
             "Sea Bass",
             "Pelagic fish",
             "Squid & cuttlefish",
             "Carnivorous zooplankton",
             "Herbivorous plankton (copepods)",
             "Gelatinous zooplankton",
             "Large crabs + shrimps",
             "Blue mussels (reefs)",
             "Blue mussels (aquaculture)",
             "Epifaunal macrobenthos (mobile grazers)",
             "Infaunal macrobenthos",
             "Crangon",
             "Small mobile epifauna (swarming crustaceans)",
             "Small infauna (polychaetes)",
             "Sessile epifauna",
             "Meiofauna",
             "Phytoplankton",
             "Detritus",
             "Discards")
  # new colmumn to df
  param_mc_combined <- NA

  for (trialn in char_vector) {
    # get list of excel files in folders
    fd1 = paste0(fd_MC,'mc_output_trial',trialn,"/")
    file.list <- list.files(path = fd1, pattern=paste0(param,'_monthly.csv'), recursive = TRUE, full.names = TRUE)
    if (identical(file.list, character(0))){
      next
    }
    # to delete consumption-biomass files when biomass asked
    param_list <- file.list[!grepl(paste0('consumption-',param,'_monthly.csv'), file.list)]

    # read them
    param_mc <- lapply(param_list, read.csv, skip = 10, header=FALSE)

    # as df, give colnames, add trial number and rbind
    param_mc_df <- as.data.frame(param_mc)
    colnames(param_mc_df) <- cols[1:ncol(param_mc_df)]


    #write code to get all columns below each other with years and colnames :-)
    param_mc_dft <- data.frame("year" = as.Date(NA), "param" = NA, "species" = NA)
    start_date2 <- as.Date("1991-01-01")   # Define start date

    for (i in 2:ncol(param_mc_df)){
      param_mc_dft1 <- param_mc_df[,c(1,i)]
      param_mc_dft1$species <- colnames(param_mc_df)[i]
      colnames(param_mc_dft1) <- c("year","param","species")
      param_mc_dft1$year <- seq.Date(from = start_date2, by = "month", length.out = 384)
      param_mc_dft <- rbind(param_mc_dft,param_mc_dft1)
    }
    param_mc_dft <- na.omit(param_mc_dft)
    param_mc_dft$trial <- trialn
    param_mc_combined <- rbind(param_mc_combined,param_mc_dft)
  }
  # omit NA values
  param_mc_combined <<- na.omit(param_mc_combined)
}
# calculate median, 5 and 95 quantiles per species per year
param_MC_summary <- function(df, calctype){
  # take median and quantiles (based on Bentley et al. (2017))
  if (calctype == "minmax"){
    param_mc_grouped <- df %>% group_by(year,species) %>% mutate( median = median(param, na.rm = TRUE),
                                                                min   = min(param, na.rm =TRUE),# quantile(param, probs= 0.025, na.rm = TRUE),
                                                                max  = max(param, na.rm =TRUE)) #quantile(param, probs= 0.975, na.rm = TRUE))
  } else {
    param_mc_grouped <- df %>% group_by(year,species) %>% mutate( median = median(param, na.rm = TRUE),
                                                                min   = quantile(param, probs= 0.025, na.rm = TRUE),
                                                                max  = quantile(param, probs= 0.975, na.rm = TRUE))
  }
  # make it an df
  param_mc_grouped <- as.data.frame(param_mc_grouped)
  # omit NA values
  param_mc_grouped <- na.omit(param_mc_grouped)
  # assign the correct class to FGs
  param_mc_grouped <<- assign_class(param_mc_grouped)
}
# retrieve best fitted model output
best_fit_param <- function(fd_bestfit){
  # column names or to name the FG correctly
  cols <- c("year",
             "Harbour porpoise",
             "Seals",
             "Seabirds (discard)",
             "Seabirds (non-discard)",
             "Sharks",
             "Rays",
             "Juvenile Cod",
             "Cod (adult)",
             "Juvenile Whiting",
             "Whiting (adult)",
             "Other gadoids",
             "Demersal fish",
             "Juvenile Herring",
             "Herring (adult)",
             "Sprat",
             "Mackerel",
             "Horse mackerel",
             "Sandeels",
             "Juvenile Plaice",
             "Plaice (adult)",
             "Dab",
             "Other flatfish",
             "Juvenile Sole",
             "Sole (adult)",
             "Sea Bass",
             "Pelagic fish",
             "Squid & cuttlefish",
             "Carnivorous zooplankton",
             "Herbivorous plankton (copepods)",
             "Gelatinous zooplankton",
             "Large crabs + shrimps",
             "Blue mussels (reefs)",
             "Blue mussels (aquaculture)",
             "Epifaunal macrobenthos (mobile grazers)",
             "Infaunal macrobenthos",
             "Crangon",
             "Small mobile epifauna (swarming crustaceans)",
             "Small infauna (polychaetes)",
             "Sessile epifauna",
             "Meiofauna",
             "Phytoplankton",
             "Detritus",
             "Discards")

    # get list of excel files in folders
    file.list <- list.files(path = fd_bestfit, pattern=paste0(param,'_annual.csv'), recursive = TRUE, full.names = TRUE)
    param_list <- file.list[!grepl(paste0('consumption-',param,'_annual.csv'), file.list)]

    # read them
    param_bestfit <- lapply(param_list, read.csv, skip = 10, header=FALSE)

    # as df, give colnames, add trial number and rbind
    param_bestfit_df <- as.data.frame(param_bestfit)
    colnames(param_bestfit_df) <- cols[1:ncol(param_bestfit_df)]

    #write code to get all columns below each other with years and colnames :-)
    param_bestfit_dft <- data.frame("year" = as.Date(NA), "param" = NA, "species" = NA)
    start_date2 <- as.Date("1991-01-01")   # Define start date

    for (i in 2:ncol(param_bestfit_df)){
      param_bestfit_dft1 <- param_bestfit_df[,c(1,i)]
      param_bestfit_dft1$species <- colnames(param_bestfit_df)[i]
      param_bestfit_dft1$year <- seq.Date(from = start_date2, by = "year", length.out = 33)
      colnames(param_bestfit_dft1) <- c("year","param","species")
      param_bestfit_dft <- rbind(param_bestfit_dft,param_bestfit_dft1)
    }
    # omit NA values
    param_bestfit_dft <- na.omit(param_bestfit_dft)
    # assign correct class to FGs
    param_bestfit_dft <- assign_class(param_bestfit_dft)

    return(param_bestfit_dft)
}
# retrieve our fitted model output
our_fit_param <- function(fd_ourfit){
  # column names or to name the FG correctly
  cols <- c("year",
            "Harbour porpoise",
            "Seals",
            "Seabirds (discard)",
            "Seabirds (non-discard)",
            "Sharks",
            "Rays",
            "Juvenile Cod",
            "Cod (adult)",
            "Juvenile Whiting",
            "Whiting (adult)",
            "Other gadoids",
            "Demersal fish",
            "Juvenile Herring",
            "Herring (adult)",
            "Sprat",
            "Mackerel",
            "Horse mackerel",
            "Sandeels",
            "Juvenile Plaice",
            "Plaice (adult)",
            "Dab",
            "Other flatfish",
            "Juvenile Sole",
            "Sole (adult)",
            "Sea Bass",
            "Pelagic fish",
            "Squid & cuttlefish",
            "Carnivorous zooplankton",
            "Herbivorous plankton (copepods)",
            "Gelatinous zooplankton",
            "Large crabs + shrimps",
            "Blue mussels (reefs)",
            "Blue mussels (aquaculture)",
            "Epifaunal macrobenthos (mobile grazers)",
            "Infaunal macrobenthos",
            "Crangon",
            "Small mobile epifauna (swarming crustaceans)",
            "Small infauna (polychaetes)",
            "Sessile epifauna",
            "Meiofauna",
            "Phytoplankton",
            "Detritus",
            "Discards")

  # get list of excel files in folders
  file.list <- list.files(path = fd_ourfit, pattern=paste0(param,'_annual.csv'), recursive = TRUE, full.names = TRUE)
  param_list <- file.list[!grepl(paste0('consumption-',param,'_annual.csv'), file.list)]

  # read them
  param_ourfit <- lapply(param_list, read.csv, skip = 10, header=FALSE)

  # as df, give colnames, add trial number and rbind
  param_ourfit_df <- as.data.frame(param_ourfit)
  colnames(param_ourfit_df) <- cols[1:ncol(param_ourfit_df)]

  #write code to get all columns below each other with years and colnames :-)
  param_ourfit_dft <- data.frame("year" = as.Date(NA), "param" = NA, "species" = NA)
  start_date2 <- as.Date("1991-01-01")   # Define start date

  for (i in 2:ncol(param_ourfit_df)){
    param_ourfit_dft1 <- param_ourfit_df[,c(1,i)]
    param_ourfit_dft1$species <- colnames(param_ourfit_df)[i]
    param_ourfit_dft1$year <- seq.Date(from = start_date2, by = "year", length.out = 33)
    colnames(param_ourfit_dft1) <- c("year","param","species")
    param_ourfit_dft <- rbind(param_ourfit_dft,param_ourfit_dft1)
  }
  # omit NA values
  param_ourfit_dft <- na.omit(param_ourfit_dft)
  # assign correct class to FGs
  param_ourfit_dft <- assign_class(param_ourfit_dft)

  return(param_ourfit_dft)
}
# retrieve TS data
TS_param <- function(fd_TS, df){
  # column names or to name the FG correctly
  cols <- c("year",
            "Harbour porpoise",
            "Seals",
            "Seabirds (discard)",
            "Seabirds (non-discard)",
            "Sharks",
            "Rays",
            "Juvenile Cod",
            "Cod (adult)",
            "Juvenile Whiting",
            "Whiting (adult)",
            "Other gadoids",
            "Demersal fish",
            "Juvenile Herring",
            "Herring (adult)",
            "Sprat",
            "Mackerel",
            "Horse mackerel",
            "Sandeels",
            "Juvenile Plaice",
            "Plaice (adult)",
            "Dab",
            "Other flatfish",
            "Juvenile Sole",
            "Sole (adult)",
            "Sea Bass",
            "Pelagic fish",
            "Squid & cuttlefish",
            "Carnivorous zooplankton",
            "Herbivorous plankton (copepods)",
            "Gelatinous zooplankton",
            "Large crabs + shrimps",
            "Blue mussels (reefs)",
            "Blue mussels (aquaculture)",
            "Epifaunal macrobenthos (mobile grazers)",
            "Infaunal macrobenthos",
            "Crangon",
            "Small mobile epifauna (swarming crustaceans)",
            "Small infauna (polychaetes)",
            "Sessile epifauna",
            "Meiofauna",
            "Phytoplankton",
            "Detritus",
            "Discards")

  # read csv file and clean up unwanted info from TS csv, only validation data wanted, no drivers
  param_TS_df <- read.csv(file = fd_TS, header = TRUE, sep = ',')
  #param_TS_df <- param_TS_df[,-which(param_TS_df[1,] == 0)] # if TS included with weight zero
  param_TS_df <- param_TS_df[,-which(param_TS_df[3,] == 3|param_TS_df[3,] == 4)]

  # give correct FG name to the FG in TS
  for(i in 2: ncol(param_TS_df)){
    param_TS_df[2,i] <- cols[param_TS_df[2,i]+1]
  }

  # df used to change code to type and rel_abs later (L345-346)
  TS_info <- data.frame(
    "rel_abs" = c("relative", "absolute",  "absolute"),
    "type" = c("biomass", "biomass", "catch"),
    "code" = c(0, 1, 6))

  # run first because of issue with rel biomass in EwE differently calculated
  # LOAD EwE file with rel biomass
  param_TS_df_EwE <- read.csv(file = fd_TS_EwE, header = TRUE, skip = 7, sep = ',')
  # remove pred B
  param_TS_df_EwE <- param_TS_df_EwE[,seq(1, ncol(param_TS_df_EwE), by = 2)]
  # set squid & pelagic aside as it has less rel biomass 'observations'
  squid_cuttlefish <- param_TS_df_EwE$biomass..observed..B_Squid.Cuttlefish_IVc_rel
  pelagic <- param_TS_df_EwE$biomass..observed..B_PelagicFish_IVc_abs_rescaled
  # remove squid & pelagic rel B obs and NAs
  param_TS_df_EwE <- na.omit(param_TS_df_EwE[-c(which(colnames(param_TS_df_EwE)=="biomass..observed..B_Squid.Cuttlefish_IVc_rel"),which(colnames(param_TS_df_EwE)=="biomass..observed..B_PelagicFish_IVc_abs_rescaled"),ncol(param_TS_df_EwE))])
  # add pelagic (first) & squid back (order related to colnames)
  param_TS_df_EwE$biomass..observed..B_PelagicFish_IVc_abs_rescaled <- pelagic[as.numeric(rownames(param_TS_df_EwE))]
  param_TS_df_EwE$biomass..observed..B_Squid.Cuttlefish_IVc_rel <- squid_cuttlefish[as.numeric(rownames(param_TS_df_EwE))]
  # add first 3 rows with info from TS csv
  colnames(param_TS_df_EwE) <- colnames(param_TS_df[c(1,(ncol(param_TS_df)-(ncol(param_TS_df_EwE)-2)):ncol(param_TS_df))])
  param_TS_df_EwE <- rbind(param_TS_df[1:3,c(1,(ncol(param_TS_df)-(ncol(param_TS_df_EwE)-2)):ncol(param_TS_df))], param_TS_df_EwE)   # -2 to account for cols with dates
  param_TS_df_EwE[3,c(2:ncol(param_TS_df_EwE))] <- 1
  param_TS_df <- cbind(param_TS_df,param_TS_df_EwE[,-1])

  #write code to get all columns below each other with years and colnames :-)
  param_TS_dft <- data.frame("year" = as.Date(NA), "param" = NA, "species" = NA, "type" = NA , "rel_abs" = NA)
  start_date2 <- as.Date("1991-01-01")   # Define start date

  for (i in 2:ncol(param_TS_df)){
    param_TS_dft1 <- param_TS_df[c(4:nrow(param_TS_df)),c(1,i)]
    param_TS_dft1$species <- param_TS_df[2,i]
    param_TS_dft1$type <- TS_info$type[which(TS_info$code == param_TS_df[3,i])]
    param_TS_dft1$rel_abs <- TS_info$rel_abs[which(TS_info$code == param_TS_df[3,i])]
    colnames(param_TS_dft1) <- c("year","param","species","type","rel_abs")
    param_TS_dft1$year <- seq.Date(from = start_date2, by = "year", length.out = 33)
    param_TS_dft <- rbind(param_TS_dft,param_TS_dft1)
  }
  # omit NA values
  param_TS_dft <- na.omit(param_TS_dft)
  # have years and values as numerics
  #param_TS_dft$year <- as.numeric(param_TS_dft$year)
  param_TS_dft$param <- as.numeric(param_TS_dft$param)
  #assign the correct class to FGs
  param_TS_dft <- assign_class(param_TS_dft)

  # only keep absolute biomass
  param_TS_dft <- subset(param_TS_dft, rel_abs == "absolute")

  # estimated the absolute biomass for FG where relative biomass was given -> required for plots
  # to do estimate abs B = rel B * B of 1991 of best fit.
  ##!!!! IS DIFFERENT IN ECOSIM GROUPPLOTS --> FOR NOW USE VALUES OBTAINED FROM ECOSIM GROUPPLOTS
  # only for biomass as these are included as relative biomass
  #for (i in which(param_TS_dft$rel_abs == 'relative' & param_TS_dft$type == "biomass")){
  #  if (param_TS_dft$species[i]!= "Squid & cuttlefish") {
  #    param_TS_dft$param[i] <- param_TS_dft$param[i] * df$param[which(df$year == "1991-01-01" & df$species == param_TS_dft$species[i])]
  #  } else {
  #    param_TS_dft$param[i] <- NA
  #    #param_TS_dft$param[i]
  #  }
  #    param_TS_dft$rel_abs[i] <- "estimated_abs"
  #}

  return(param_TS_dft)
}
# plotting the parameter
MC_plot_param <- function(df,df2,df3,df4, var, subset_multi_FG, subset_by_species, subset_by_class, y_title){
  # have them in an order that makes sense, i.e. FG number
  df$species <- factor(df$species, levels = cols[-1])
  df2$species <- factor(df2$species, levels = cols[-1])
  df3$species <- factor(df3$species, levels = cols[-1])
  df4$species <- factor(df4$species, levels = cols[-1])

  # take subset to add catch or biomass TS to plots
  df4 <- subset(df4, type == var)

  # subset for 1 specific species
  if (!is.na(subset_by_species)){
    df <- subset(df, species == subset_by_species)
    df2 <- subset(df2, species == subset_by_species)
    df3 <- subset(df3, species == subset_by_species)
    df4 <- subset(df4, species == subset_by_species)
  }
  # subset for 1 specific class
  if (!is.na(subset_by_class)){
    df <- subset(df, class == subset_by_class)
    df2 <- subset(df2, class == subset_by_class)
    df3 <- subset(df3, class == subset_by_class)
    df4 <- subset(df4, class == subset_by_class)
  }
  # subset multiple species/classes
  if (subset_multi_FG == "yes"){
    df <- specific_subset(df)
    df2 <- specific_subset(df2)
    df3 <- specific_subset(df3)
    df4 <- specific_subset(df4)
  }

  # plot
param_graph <- ggplot() +
  geom_line(data = df2, aes(x = as.Date(year), y = param, linetype = "solid"), colour = "#354d9b", linewidth = 1.5 ) + # best fit (obtained by stepwise fitting)
  #geom_line(data = df, aes(x = as.Date(year), y = median, colour = class, linetype = "dotted"), linewidth = 1.5 ) + # median line
  #geom_line(data = df3, aes(x = as.Date(year), y = param, colour = class, linetype = "dashed"), linewidth = 1.5 ) + # our fit line
  geom_point(data = df4, aes(x = as.Date(year), y = param, shape = "A"), colour = "#354d9b", size = 2) +  # TS data
  geom_ribbon(data = df, aes(x= as.Date(year), ymin = min, ymax = max, fill= "#354d9b"),  alpha = 0.15) +
  facet_wrap(vars(species), ncol = 3, scales = "free_y")+
  scale_fill_manual(values = "#354d9b",
                      labels = "confidence interval" ,
                      name = NULL)+
  #scale_colour_manual(values = "#354d9b",
  #                  labels = NULL ,
  #                  name = NULL)+

  scale_linetype_manual(values = c("solid", "dotted", "dashed"),   # Custom linetypes
                        labels = c("best fit", "median" , "our fit"),   # Custom labels
                        name = NULL)+
  scale_shape_manual(values = c("A" = 1),
                     labels = "stock assessment" ,
                     name = NULL)+
  #xlim(1991,2022) +
  #ylim(0,7.5)+
  theme_bw()+
  theme(text = element_text(size=10), axis.text.x=element_text(angle=90, hjust=1))+
  theme(legend.position = "bottom",legend.direction = "vertical",legend.background = element_rect(),legend.title = element_text(size=14,face = 'bold'),
        legend.text = element_text(size=12),legend.text.align = 0,axis.text.x = element_text(colour="black",size=14),
        axis.text.y = element_text(colour="black",size=14), axis.title.x = element_text(colour="black",size=16),
        axis.title.y = element_text(colour="black",size=16), strip.text = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x = "Year", y= y_title)

return(param_graph)
}
# Ecological Indicator
EcoInd <- function(dir, calctype){
  # read eco ind files and select columns
  ecoind_mc <- read.csv(paste0(dir,"biodiv_ind_Monte Carlo.csv"), skip = 8, header = TRUE)
  ecoind_mc <- ecoind_mc %>% select(Time,  Total.B, Commercial.B, Total.C,
                                  TL.catch, MTI, TL.community, Trial)

  #if (length(which(ecoind_mc$Total.B == "∞")) != 0 ) {
  #ecoind_mc <- ecoind_mc[-which(ecoind_mc$Trial %in% unique(ecoind_mc$Trial[which(ecoind_mc$Total.B == "∞")])),] # delete false results
  #ecoind_mc <- as.data.frame(sapply(ecoind_mc, as.numeric))
  #}

  #if (length(which(ecoind_mc$TL.catch > 5)) != 0 ) {
  #  ecoind_mc <- ecoind_mc[-which(ecoind_mc$Trial %in% unique(ecoind_mc$Trial[which(ecoind_mc$TL.catch > 5)])),] # delete false results
  #}

  #if (length(which(ecoind_mc$TL.catch < 0)) != 0 ) {
  #  ecoind_mc <- ecoind_mc[-which(ecoind_mc$Trial %in% unique(ecoind_mc$Trial[which(ecoind_mc$TL.catch < 0)])),] # delete false results
  }

  ecoind_mc <- ecoind_mc[which(ecoind_mc$Trial %in% as.numeric(param_mc_combined$trial)),]

  # group mc runs and take minmax or quantiles
  if (calctype == "minmax"){
    ecoind_mc_grouped <<- ecoind_mc %>% group_by(Time) %>% summarise( med_total_B = median(Total.B, na.rm = TRUE),
                                                                  min_total_B   = min(Total.B, na.rm =TRUE),# quantile(param, probs= 0.025, na.rm = TRUE),
                                                                  max_total_B  = max(Total.B, na.rm =TRUE),
                                                          med_Commercial_B = median(Commercial.B, na.rm = TRUE),
                                                          min_Commercial_B   = min(Commercial.B, na.rm =TRUE),# quantile(param, probs= 0.025, na.rm = TRUE),
                                                          max_Commercial_B  = max(Commercial.B, na.rm =TRUE),
                                                          med_total_C = median(Total.C, na.rm = TRUE),
                                                          min_total_C   = min(Total.C, na.rm =TRUE),# quantile(param, probs= 0.025, na.rm = TRUE),
                                                          max_total_C  = max(Total.C, na.rm =TRUE),
                                                          med_TL_catch = median(TL.catch, na.rm = TRUE),
                                                          min_TL_catch   = min(TL.catch, na.rm =TRUE),# quantile(param, probs= 0.025, na.rm = TRUE),
                                                          max_TL_catch  = max(TL.catch, na.rm =TRUE),
                                                          med_MTI = median(MTI, na.rm = TRUE),
                                                          min_MTI   = min(MTI, na.rm =TRUE),# quantile(param, probs= 0.025, na.rm = TRUE),
                                                          max_MTI  = max(MTI, na.rm =TRUE),
                                                          med_TL_community = median(TL.community, na.rm = TRUE),
                                                          min_TL_community   = min(TL.community, na.rm =TRUE),# quantile(param, probs= 0.025, na.rm = TRUE),
                                                          max_TL_community  = max(TL.community, na.rm =TRUE)) #quantile(param, probs= 0.975, na.rm = TRUE))
    # change month numbers to dates
    start_date <- as.Date("1991-01-01")   # Define start date
    monthly_sequence <- seq.Date(from = start_date, by = "month", length.out = max(ecoind_mc$Time))   # Create sequence of monthly dates
    ecoind_mc_grouped$Time <- monthly_sequence
    } else {
    ecoind_mc_grouped <<- ecoind_mc %>% group_by(Time) %>% summarise( med_total_B = median(Total.B, na.rm = TRUE),
                                                                 min_total_B   = quantile(Total.B, probs= 0.025,na.rm =TRUE),# quantile(param, probs= 0.025, na.rm = TRUE),
                                                                 max_total_B  = quantile(Total.B, probs= 0.975,na.rm =TRUE),
                                                                 med_Commercial_B = median(Commercial.B, na.rm = TRUE),
                                                                 min_Commercial_B   = quantile(Commercial.B, probs= 0.025,na.rm =TRUE),# quantile(param, probs= 0.025, na.rm = TRUE),
                                                                 max_Commercial_B  = quantile(Commercial.B, probs= 0.975,na.rm =TRUE),
                                                                 med_total_C = median(Total.C, na.rm = TRUE),
                                                                 min_total_C   = quantile(Total.C,probs= 0.025, na.rm =TRUE),# quantile(param, probs= 0.025, na.rm = TRUE),
                                                                 max_total_C  = quantile(Total.C,probs= 0.975, na.rm =TRUE),
                                                                 med_TL_catch = median(TL.catch, na.rm = TRUE),
                                                                 min_TL_catch   = quantile(TL.catch, probs= 0.025,na.rm =TRUE),# quantile(param, probs= 0.025, na.rm = TRUE),
                                                                 max_TL_catch  = quantile(TL.catch, probs= 0.975,na.rm =TRUE),
                                                                 med_MTI = median(MTI, na.rm = TRUE),
                                                                 min_MTI   = quantile(MTI, probs= 0.025,na.rm =TRUE),# quantile(param, probs= 0.025, na.rm = TRUE),
                                                                 max_MTI  = quantile(MTI, probs= 0.975,na.rm =TRUE),
                                                                 med_TL_community = median(TL.community, na.rm = TRUE),
                                                                 min_TL_community   = quantile(TL.community, probs= 0.025,na.rm =TRUE),# quantile(param, probs= 0.025, na.rm = TRUE),
                                                                 max_TL_community  = quantile(TL.community,probs= 0.975, na.rm =TRUE)) #quantile(param, probs= 0.975, na.rm = TRUE))

    # change month numbers to dates
    start_date <- as.Date("1991-01-01")   # Define start date
    monthly_sequence <- seq.Date(from = start_date, by = "month", length.out = max(ecoind_mc$Time))   # Create sequence of monthly dates
    ecoind_mc_grouped$Time <- monthly_sequence
    }

  ecoind_sim <- read.csv(paste0(dir,"NA_Annual_IndicesWithoutPPR.csv"), header = TRUE)
  ecoind_sim <<- ecoind_sim %>% select(Year,  Biomass, Commercial.Biomass, Catch,
                                   TLc, mTLco)
  ecoind_sim$Time <- paste0(ecoind_sim$Year,"-06-01")
  ecoind_sim$Time <-as.Date.character(ecoind_sim$Time)

  # change month numbers to dates
  #ecoind_sim$Time <- monthly_sequence # define startdate and create sequence reused here
  # plot (dark blue = "#354d9b", ligthblue = "#31b7bc", yellow = "#f7c97c")
  EcoInd_B_graph <<- ggplot() +
    #geom_line(data = ecoind_mc_grouped, aes(x = Time, y = med_total_B, colour = "Total biomass", linetype = "median"), linewidth = 1.5) +
    #geom_line(data = ecoind_mc_grouped, aes(x = Time, y = med_Commercial_B, colour = "Commercial biomass", linetype = "median"), linewidth = 1.5) +
    geom_ribbon(data = ecoind_mc_grouped, aes(x = Time, ymin = min_total_B, ymax = max_total_B, fill = "Total biomass"), alpha = 0.15) +
    #geom_ribbon(data = ecoind_mc_grouped, aes(x = Time, ymin = min_Commercial_B, ymax = max_Commercial_B, fill = "Commercial biomass"), alpha = 0.15) +
    geom_line(data = ecoind_sim, aes(x = Time, y = Biomass, colour = "Total biomass"), linetype = "solid", linewidth = 1.5) +
    #geom_line(data = ecoind_sim, aes(x = Time, y = Commercial.Biomass, colour = "Commercial biomass"), linetype = "solid", linewidth = 1.5) +
    scale_colour_manual(values = c("Total biomass" = "#31b7bc"),#"Commercial biomass" = "#354d9b",
                        name = "") +
    scale_fill_manual(values = c("Total biomass" = "#31b7bc"),#"Commercial biomass" = "#354d9b",
                        name = "") +
    theme_bw()+
    theme(
      text = element_text(size = 10),
      axis.text.x = element_text(angle = 90, hjust = 1, colour = "black", size = 14),
      axis.text.y = element_text(colour = "black", size = 14),
      axis.title.x = element_text(colour = "black", size = 16,),
      axis.title.y = element_text(colour = "black", size = 16,),
      strip.text = element_text(size = 12),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.background = element_rect(),
      legend.title = element_text(size = 14, face = 'bold'),
      legend.text = element_text(size = 12),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank()
    ) +
    labs(x = "Year", y= expression(paste("Biomass (t  ",km^-2,")")))

  EcoInd_C_graph <<- ggplot() +
    #geom_line(data = ecoind_mc_grouped, aes(x = Time, y = med_total_C, colour = "Total catch", linetype = "median"), linewidth = 1.5) +
    geom_ribbon(data = ecoind_mc_grouped, aes(x = Time, ymin = min_total_C, ymax = max_total_C, fill = "Total catch"), alpha = 0.15) +
     geom_line(data = ecoind_sim, aes(x = Time, y = Catch, colour = "Total catch"), linetype = "solid", linewidth = 1.5) +
    scale_colour_manual(values = c("Total catch" = "#354d9b"),
                        name = "") +
    scale_fill_manual(values = c("Total catch" = "#354d9b"),
                      name = "") +
    theme_bw()+
    theme(
      text = element_text(size = 10),
      axis.text.x = element_text(angle = 90, hjust = 1, colour = "black", size = 14),
      axis.text.y = element_text(colour = "black", size = 14),
      axis.title.x = element_text(colour = "black", size = 16,),
      axis.title.y = element_text(colour = "black", size = 16,),
      strip.text = element_text(size = 12),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.background = element_rect(),
      legend.title = element_text(size = 14, face = 'bold'),
      legend.text = element_text(size = 12),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank()
    ) +
    labs(x = "Year", y= expression(paste("Catch (t  ",km^-2,y^-1,")")))

  EcoInd_TL_graph <<- ggplot() +
    #geom_line(data = ecoind_mc_grouped, aes(x = Time, y = med_TL_catch, colour = "TL catch", linetype = "median"), linewidth = 1.5) +
    #geom_line(data = ecoind_mc_grouped, aes(x = Time, y = med_TL_community, colour = "TL community", linetype = "median"), linewidth = 1.5) +
    geom_ribbon(data = ecoind_mc_grouped, aes(x = Time, ymin = min_TL_catch, ymax = max_TL_catch, fill = "TL catch"), alpha = 0.15) +
    geom_ribbon(data = ecoind_mc_grouped, aes(x = Time, ymin = min_TL_community, ymax = max_TL_community, fill = "TL community"), alpha = 0.15) +
    geom_line(data = ecoind_sim, aes(x = Time, y = TLc, colour = "TL catch"), linetype = "solid", linewidth = 1.5) +
    geom_line(data = ecoind_sim, aes(x = Time, y = mTLco, colour = "TL community"), linetype = "solid", linewidth = 1.5) +
    scale_colour_manual(values = c("TL catch" = "#354d9b", "TL community" = "#31b7bc"),
                        name = NULL) +
    scale_fill_manual(values = c("TL catch" = "#354d9b","TL community" = "#31b7bc"),
                      name = NULL) +
    theme_bw() +
    theme(
      text = element_text(size = 10),
      axis.text.x = element_text(angle = 90, hjust = 1, colour = "black", size = 14),
      axis.text.y = element_text(colour = "black", size = 14),
      axis.title.x = element_text(colour = "black", size = 16,),
      axis.title.y = element_text(colour = "black", size = 16,),
      strip.text = element_text(size = 12),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.background = element_rect(),
      legend.title = element_text(size = 14, face = 'bold'),
      legend.text = element_text(size = 12),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank()
    ) +
    labs(x = "Year", y = expression("Trophic level"))

}
ecoind_NA <- function(dir){
  # annually
  ecoind_NA_annual <- read.csv(paste0(dir,"NA_Annual_IndicesWithoutPPR.csv"),header = TRUE, row.names = NULL)
  #colnames(ecoind_NA_annual) <- c(colnames(ecoind_NA_annual)[-1], "NA")
  #ecoind_NA_annual <- ecoind_NA_annual[,-ncol(ecoind_NA_annual)]
  ecoind_NA_annual$date <- paste0(ecoind_NA_annual$Year,"-01-01")
  ecoind_NA_annual$date <-as.Date.character(ecoind_NA_annual$date)
  ecoind_NA_annual <- ecoind_NA_annual %>% select("date",
                                                   "Throughput", "Export", "Resp", "Prim.prod", "Prod", "Prop.flow.det",
                                                   "Capacity", "Ascendency", "Asc.import", "Asc.flow", "Asc.export", "Asc.resp",
                                                   "Entropy", "Ovh.import", "Ovh.flow", "Ovh.export", "Ovh.resp",
                                                   "Biomass", "Catch",
                                                   "FCI", "Path.length", "AMI", "TLc","Commercial.Biomass",	"mTLco") ## AMI is simiar to SOI measures measures the degree of omnivory in a food web by considering the distribution of flows across different trophic levels.
  #"PCI", "TLc", "Shannon.diversity.index", "FiB.index", "Det..TE..weighted.",
  #"PP.TE..weighted.", "Total.TE..weigthed."

  # calculate proportions of flows
  ecoind_NA_annual$TST <- ecoind_NA_annual$Throughput
  ecoind_NA_annual$Ex <- ecoind_NA_annual$Export / ecoind_NA_annual$Throughput
  ecoind_NA_annual$PP <- ecoind_NA_annual$Prim.prod / ecoind_NA_annual$Throughput
  ecoind_NA_annual$FD <- ecoind_NA_annual$Prop.flow.det
  ecoind_NA_annual$R <- ecoind_NA_annual$Resp / ecoind_NA_annual$Throughput
  ecoind_NA_annual$AC <- ecoind_NA_annual$Ascendency / ecoind_NA_annual$Capacity
  ecoind_NA_annual$OC <- 1 - ecoind_NA_annual$AC
  ecoind_NA_annual$IFO <- ecoind_NA_annual$Ovh.flow

  ecoind_NA_annual <<- ecoind_NA_annual %>% select("date",
                                                  "TST", #"Ex", "R", "PP", "FD", #"Q",
                                                  "IFO",
                                                  "FCI",
                                                  "OC",
                                                  "AC",
                                                  "Biomass",
                                                  "Catch",
                                                  "TLc",
                                                  "Commercial.Biomass",
                                                  "mTLco")


  # monthly
  ecoind_NA_monthly <- read.csv(paste0(dir,"NA_Monthly_IndicesWithoutPPR.csv"),header = TRUE, row.names = NULL)
  #colnames(ecoind_NA_monthly) <- c(colnames(ecoind_NA_monthly[-1]), "date")

  for (i in 1:length(rownames(ecoind_NA_monthly))){
    if (nchar(substr(ecoind_NA_monthly$Year[i], 6, 7)) == 1){
      ecoind_NA_monthly$date[i] <- paste0(substr(ecoind_NA_monthly$Year[i], 1, 4),"-0",substr(ecoind_NA_monthly$Year[i], 6, 7),"-01")
      ecoind_NA_monthly$date <-as.Date.character(ecoind_NA_monthly$date)
    } else {
      ecoind_NA_monthly$date[i] <- paste0(substr(ecoind_NA_monthly$Year[i], 1, 4),"-",substr(ecoind_NA_monthly$Year[i], 6, 7),"-01")
      ecoind_NA_monthly$date <-as.Date.character(ecoind_NA_monthly$date)
    }
  }

  ecoind_NA_monthly <- ecoind_NA_monthly %>% select("date",
                                                     "Throughput", "Export", "Resp", "Prim.prod", "Prod", "Prop.flow.det",
                                                     "Capacity", "Ascendency", "Asc.import", "Asc.flow", "Asc.export", "Asc.resp",
                                                     "Entropy", "Ovh.import", "Ovh.flow", "Ovh.export", "Ovh.resp",
                                                     "Biomass", "Catch",
                                                     "FCI", "Path.length", "AMI", "TLc","Commercial.Biomass",	"mTLco") ## AMI is simiar to SOI measures measures the degree of omnivory in a food web by considering the distribution of flows across different trophic levels.
  #"PCI", "TLc", "Shannon.diversity.index", "FiB.index", "Det..TE..weighted.",
  #"PP.TE..weighted.", "Total.TE..weigthed."

  # calculate proportions of flows
  ecoind_NA_monthly$TST <- ecoind_NA_monthly$Throughput
  ecoind_NA_monthly$Ex <- ecoind_NA_monthly$Export / ecoind_NA_monthly$Throughput
  ecoind_NA_monthly$PP <- ecoind_NA_monthly$Prim.prod / ecoind_NA_monthly$Throughput
  ecoind_NA_monthly$FD <- ecoind_NA_monthly$Prop.flow.det
  ecoind_NA_monthly$R <- ecoind_NA_monthly$Resp / ecoind_NA_monthly$Throughput
  ecoind_NA_monthly$AC <- ecoind_NA_monthly$Ascendency / ecoind_NA_monthly$Capacity
  ecoind_NA_monthly$OC <- 1 - ecoind_NA_monthly$AC
  ecoind_NA_monthly$IFO <- ecoind_NA_monthly$Ovh.flow

  ecoind_NA_monthly <<- ecoind_NA_monthly %>% select("date",
                                                  "TST", #"Ex", "R", "PP", "FD", #"Q",
                                                  "IFO",
                                                  "FCI",
                                                  "OC",
                                                  "AC",
                                                  "Biomass",
                                                  "Catch",
                                                  "TLc",
                                                  "Commercial.Biomass",
                                                  "mTLco")
}
ecoind_NA_plot <- function(dataset){
  ecoind_NA_annual$OC <- ecoind_NA_annual$OC  *100
  ecoind_NA_annual$AC <- ecoind_NA_annual$AC  *100
  EcoInd_NA <- list()

  if (dataset == "annual"){
  for (i in 1:(length(colnames(ecoind_NA_annual))-1)){
    colname <- colnames(ecoind_NA_annual)[i+1]
    #plot
   plot <- ggplot(data = ecoind_NA_annual) +
    #geom_point(aes(x = date, y = value), colour = "#354d9b", shape = 16, size = 1.5) +
    geom_line(aes(x = date, y = .data[[colname]]), colour = "#354d9b", linetype = "solid", linewidth = 1.5) + #"#354d9b"
    scale_x_date(date_breaks = "4 year", date_labels = "%Y") +

    labs(x = "Year", y =if (i == 1) {
      bquote(.(colnames(ecoind_NA_annual)[i+1])~"("~.("t km"^-2 ~ "y"^-1)~")")
    } else {
      paste(colnames(ecoind_NA_annual)[i+1], "(%)")
    }) +
    theme_bw() +
    theme(
      text = element_text(size = 10),
      axis.text.x = element_text(angle = 90, hjust = 1, colour = "black", size = 14),
      axis.text.y = element_text(colour = "black", size = 14),
      axis.title.x = element_text(colour = "black", size = 16,),
      axis.title.y = element_text(colour = "black", size = 16,),
      strip.text = element_text(size = 12),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.background = element_rect(),
      legend.title = element_text(size = 14, face = 'bold'),
      legend.text = element_text(size = 12),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank()
    )
   EcoInd_NA[[i]] <<- plot
    }
    }else{
      for (i in 1:(length(colnames(ecoind_NA_monthly))-1)){
        colname <- colnames(ecoind_NA_annual)[i+1]
        #plot
       plot <- ggplot(data = ecoind_NA_monthly) +
          #geom_point(aes(x = date, y = value), colour = "#354d9b", shape = 16, size = 1.5) +
          geom_line(aes(x = date, y = .data[[colname]]), colour = "#354d9b", linetype = "solid", linewidth = 1.5) + #"#354d9b"
          scale_x_date(date_breaks = "4 year", date_labels = "%Y") +

          labs(x = "Year", y =if (i == 1) {
            bquote(.(colnames(ecoind_NA_annual)[i+1])~"("~.("t km"^-2 ~ "y"^-1)~")")
          } else {
            paste(colnames(ecoind_NA_monthly)[i+1], "(%)")
          }) +
          theme_bw() +
          theme(
            text = element_text(size = 10),
            axis.text.x = element_text(angle = 90, hjust = 1, colour = "black", size = 14),
            axis.text.y = element_text(colour = "black", size = 14),
            axis.title.x = element_text(colour = "black", size = 16,),
            axis.title.y = element_text(colour = "black", size = 16,),
            strip.text = element_text(size = 12),
            legend.position = "bottom",
            legend.direction = "vertical",
            legend.background = element_rect(),
            legend.title = element_text(size = 14, face = 'bold'),
            legend.text = element_text(size = 12),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()
          )
       EcoInd_NA[[i]] <<- plot
      }
      }
  return(EcoInd_NA)
}

# MC output   ------------------------------------------------------------------
# required info to run the code
# directory to folder with MC output (folders)
fd_MC = 'C:/Users/stevenp/OneDrive - VLIZ/Documents/EwE output/SBNS_1991_2023_V17_TechnicalReport_diets/mc_V15_TechnicalReport/'
fd_bestfit = 'C:/Users/stevenp/OneDrive - VLIZ/Documents/EwE output/SBNS_1991_2023_V17_TechnicalReport_with_MC_NA/ecosim_V15_TechnicalReport'
fd_ourfit = 'C:/Users/stevenp/OneDrive - VLIZ/Documents/EwE output/SBNS_1991_2023_V17_TechnicalReport_with_MC_NA/ecosim_V15_TechnicalReport'
fd_classes = "C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/EMBENS Rpath/"
fd_TS = "C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/SBNS models/TS_SBNS_V24_TechnicalReport_Brel.csv"
fd_TS_EwE = "C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/SBNS models/SBNS_1991_2023_V16_TechnicalReport_allfit_biomass.csv"
fd_MC_limits = "C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/EMBENS Rpath/"
#get_options_MC(fd_MC)


# total number of trials ran
total_trials <- 2000
param <- "biomass" # biomass now annual

#get best X% trials based on lowest SS
SS_MC(fd_MC,total_trials,100)

# get parameter outputs
param_MC()
#param_monthly_MC()

# remove bad runs (based on biomass, so only run with biomass as parameter)
#param_mc_combined$param<-as.numeric(param_mc_combined$param) # set parameter values as numeric (needed when false results are included)
if (param == "biomass"){
bad_runs <- which(param_mc_combined$param > 20)
bad_runs2 <- param_mc_combined[bad_runs,]
bad_runs2 <- subset(bad_runs2, year == "1991-01-01")
bad_runs3 <- subset(bad_runs2, species != "Epifaunal macrobenthos (mobile grazers)"& species != "Infaunal macrobenthos"&
                      species != "Small infauna (polychaetes)"& species != "Sessile epifauna"& species !=  "Discards"&
                      species != "Detritus"& species != "Small mobile epifauna (swarming crustaceans)" & species != "Meiofauna" )
bad_runs3
unique(bad_runs3$trial)
}

# calculate median & quantiles per species per year
param_MC_summary(param_mc_combined,"Q") #"minmax" (minimum and maximum) or "Q" (quantiles)

# load best fit, our fit and TS
param_bestfit <- best_fit_param(fd_bestfit)
param_ourfit <- our_fit_param(fd_ourfit)
param_TS <- TS_param(fd_TS,param_ourfit)

# you can select one species or one class
subset_by_species <- NA # if not required = NA
subset_by_class <- NA # if not required = NA

# you can select several species or classes by adjusting the function to subset the df with only the preferred species/classes
multiple_FGs <- "yes" # "yes" or "no"
specific_subset <- function(df){subset(df,species == "Plaice (adult)"|
                                         species == "Herring (adult)"|
                                         species == "Sole (adult)"|
                                         species == "Cod (adult)"|
                                         species == "Whiting (adult)"|
                                         species == "Mackerel"|
                                         species == "Horse mackerel"|
                                         species == "Sprat"|
                                         species == "Sandeels"|
                                         species == "Sea Bass"|
                                         species == "Other flatfish"|
                                         species == "Rays"|
                                         species == "Sharks"|
                                         species == "Other gadoids"|
                                         species == "Demersal fish"|
                                         species == "Pelagic fish"|
                                         species == "Squid & cuttlefish"|
                                         species == "Dab")
                              }


# species the correct label on y-axis
y_title <- expression(paste("Biomass (t  ",km^-2,")" ))   #"Catch (t  ",km^-2,y^-1,")"

# plot
# provide df with median & quantiles, df with best fit, df with our fit, df with TS, the parameter, specific subset with multiple FGs, 1species t subset, 1class to subset, y-axis title
MC_plot_param(param_mc_grouped, param_bestfit, param_ourfit, param_TS, param, multiple_FGs, subset_by_species, subset_by_class, y_title)



 #Ecological Indicators ---------------------------------------------------------
# analyse ecological indicators that ran with MC
fd_EcoInd = 'C:/Users/stevenp/OneDrive - VLIZ/Documents/EwE output/SBNS_1991_2023_V17_TechnicalReport_diets/mc_V15_TechnicalReport/'
EcoInd(fd_EcoInd, "minmax") #directory and minmax or quantiles; only ecoind with MC
ecoind_mc_grouped_df <- as.data.frame(ecoind_mc_grouped)
start_date <- as.Date("1991-01-01")   # Define start date
monthly_sequence <- seq.Date(from = start_date, by = "month", length.out = max(ecoind_mc_grouped_df$Time))   # Create sequence of monthly dates
ecoind_mc_grouped$Time <- monthly_sequence

plot_grid(EcoInd_B_graph, EcoInd_C_graph, EcoInd_TL_graph, labels = "AUTO", ncol = 3, align = 'v') +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

# analyse eco ind that come from Network analysis and not ran wit MC
fd_ecoind_NA <- "C:/Users/stevenp/OneDrive - VLIZ/Documents/EwE output/SBNS_1991_2023_V16_TechnicalReport/ecosim_V15_TechnicalReport/"
ecoind_NA(fd_ecoind_NA)
ecoind_NA_annual
ecoind_NA_monthly

ecoind_NA_plot("annual") # annual or monthly
plot_grid(EcoInd_NA[[1]], EcoInd_NA[[2]], EcoInd_NA[[3]], EcoInd_NA[[4]], EcoInd_NA[[5]], labels = "AUTO", ncol = 5, align = 'h') +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

write.csv(ecoind_NA_annual, "C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/Manuscripts/Ecosim paper/annual_eco_ind.csv")
