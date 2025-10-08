################################################################################
#                          EwE Monte-Carlo processing                          #
################################################################################
# packages
require('dplyr')
require('ggplot2')

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
      param_mc_dft1$year <- seq.Date(from = start_date2, by = "year", length.out = 32)
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
      param_bestfit_dft1$year <- seq.Date(from = start_date2, by = "year", length.out = 32)
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
    param_ourfit_dft1$year <- seq.Date(from = start_date2, by = "year", length.out = 32)
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
  param_TS_df <- param_TS_df[,-which(param_TS_df[1,] == 0)]
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

  # estimated the absolute biomass for FG where relative biomass was given -> required for plots
  # to do estimate abs B = rel B * B of 1991 of best fit.
  for (i in which(param_TS_dft$rel_abs == 'relative' & param_TS_dft$type == "biomass")){
    if (param_TS_dft$species[i]!= "Squid & cuttlefish") {
      param_TS_dft$param[i] <- param_TS_dft$param[i] * df$param[which(df$year == "1991-01-01" & df$species == param_TS_dft$species[i])]
    } else {
      param_TS_dft$param[i] <- NA
      #param_TS_dft$param[i]
    }
      param_TS_dft$rel_abs[i] <- "estimated_abs"
  }

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
  geom_line(data = df2, aes(x = as.Date(year), y = param, linetype = "solid"), colour = "#354d9b", linewidth = 1 ) + # best fit (obtained by stepwise fitting)
  #geom_line(data = df, aes(x = year, y = median, colour = class, linetype = "dotted"), linewidth = 1 ) + # median line
  #geom_line(data = df3, aes(x = year, y = param, colour = class, linetype = "dashed"), linewidth = 1 ) + # our fit line
  geom_point(data = df4, aes(x = as.Date(year), y = param, shape = "A"), colour = "#354d9b", size = 1) +  # TS data
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
        legend.text = element_text(size=12),legend.text.align = 0,axis.text.x = element_text(colour="black",size=11.5),
        axis.text.y = element_text(colour="black",size=14), axis.title.x = element_text(colour="black",size=16,face = 'bold'),
        axis.title.y = element_text(colour="black",size=16,face = 'bold'), strip.text = element_text(size = 12))+
  labs(x = "year", y= y_title)

return(param_graph)
}
# Ecological Indicator
EcoInd <- function(dir, calctype){
  # read eco ind files and select columns
  ecoind_mc <- read.csv(paste0(dir,"biodiv_ind_Monte Carlo.csv"), skip = 8, header = TRUE)
  ecoind_mc <- ecoind_mc %>% select(Time,  Total.B, Commercial.B, Total.C,
                                  TL.catch, MTI, TL.community)

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

  ecoind_sim <- read.csv(paste0(dir,"biodiv_ind_Ecosim.csv"), skip = 8, header = TRUE)
  ecoind_sim <<- ecoind_sim %>% select(Time,  Total.B, Commercial.B, Predatory.B, Kempton.s.Q, Shannon.diversity,  Total.C,
                                    Predatory.C, Discards, TL.catch, MTI, TL.community, TL.community.2)

  # change month numbers to dates
  ecoind_sim$Time <- monthly_sequence # define startdate and create sequence reused here
  # plot (dark blue = "#354d9b", ligthblue = "#31b7bc", yellow = "#f7c97c")
  EcoInd_B_graph <<- ggplot() +
    geom_line(data = ecoind_mc_grouped, aes(x = Time, y = med_total_B, colour = "Total biomass", linetype = "median"), linewidth = 1) +
    geom_line(data = ecoind_mc_grouped, aes(x = Time, y = med_Commercial_B, colour = "Commercial biomass", linetype = "median"), linewidth = 1) +
    geom_ribbon(data = ecoind_mc_grouped, aes(x = Time, ymin = min_total_B, ymax = max_total_B), fill = "#354d9b", alpha = 0.15) +
    geom_ribbon(data = ecoind_mc_grouped, aes(x = Time, ymin = min_Commercial_B, ymax = max_Commercial_B), fill = "#31b7bc", alpha = 0.15) +
    geom_line(data = ecoind_sim, aes(x = Time, y = Total.B, colour = "Total biomass", linetype = "best fit"), linewidth = 1) +
    geom_line(data = ecoind_sim, aes(x = Time, y = Commercial.B, colour = "Commercial biomass", linetype = "best fit"), linewidth = 1) +
    scale_colour_manual(values = c("Total biomass" = "#354d9b", "Commercial biomass" = "#31b7bc")) +
    scale_linetype_manual(values = c("best fit" = "solid", "median" = "dotted")) +
    theme_bw()+
    theme(
      text = element_text(size = 10),
      axis.text.x = element_text(angle = 90, hjust = 1, colour = "black", size = 11.5),
      axis.text.y = element_text(colour = "black", size = 14),
      axis.title.x = element_text(colour = "black", size = 16, face = 'bold'),
      axis.title.y = element_text(colour = "black", size = 16, face = 'bold'),
      strip.text = element_text(size = 12),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.background = element_rect(),
      legend.title = element_text(size = 14, face = 'bold'),
      legend.text = element_text(size = 12)
    ) +
    labs(x = "year", y= expression(paste("Biomass (t  ",km^-2,")")))

  EcoInd_C_graph <<- ggplot() +
    geom_line(data = ecoind_mc_grouped, aes(x = Time, y = med_total_C, colour = "Total catch", linetype = "median"), linewidth = 1) +
    geom_ribbon(data = ecoind_mc_grouped, aes(x = Time, ymin = min_total_C, ymax = max_total_C), fill = "#354d9b", alpha = 0.15) +
     geom_line(data = ecoind_sim, aes(x = Time, y = Total.C, colour = "Total catch", linetype = "best fit"), linewidth = 1) +
    scale_colour_manual(values = c("Total catch" = "#354d9b")) +
    scale_linetype_manual(values = c("best fit" = "solid", "median" = "dotted")) +
    theme_bw()+
    theme(
      text = element_text(size = 10),
      axis.text.x = element_text(angle = 90, hjust = 1, colour = "black", size = 11.5),
      axis.text.y = element_text(colour = "black", size = 14),
      axis.title.x = element_text(colour = "black", size = 16, face = 'bold'),
      axis.title.y = element_text(colour = "black", size = 16, face = 'bold'),
      strip.text = element_text(size = 12),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.background = element_rect(),
      legend.title = element_text(size = 14, face = 'bold'),
      legend.text = element_text(size = 12)
    ) +
    labs(x = "year", y= expression(paste("Catch (t  ",km^-2,y^-1,")")))

  EcoInd_TL_graph <<- ggplot() +
    geom_line(data = ecoind_mc_grouped, aes(x = Time, y = med_TL_catch, colour = "TL catch", linetype = "median"), linewidth = 1) +
    geom_line(data = ecoind_mc_grouped, aes(x = Time, y = med_TL_community, colour = "TL community", linetype = "median"), linewidth = 1) +
    geom_ribbon(data = ecoind_mc_grouped, aes(x = Time, ymin = min_TL_catch, ymax = max_TL_catch), fill = "#354d9b", alpha = 0.15) +
    geom_ribbon(data = ecoind_mc_grouped, aes(x = Time, ymin = min_TL_community, ymax = max_TL_community), fill = "#31b7bc", alpha = 0.15) +
    geom_line(data = ecoind_sim, aes(x = Time, y = TL.catch, colour = "TL catch", linetype = "best fit"), linewidth = 1) +
    geom_line(data = ecoind_sim, aes(x = Time, y = TL.community, colour = "TL community", linetype = "best fit"), linewidth = 1) +
    scale_colour_manual(values = c("TL catch" = "#354d9b", "TL community" = "#31b7bc"),
                        name = NULL) +
    scale_linetype_manual(values = c("best fit" = "solid", "median" = "dotted"),
                          name = NULL) +
    theme_bw() +
    theme(
      text = element_text(size = 10),
      axis.text.x = element_text(angle = 90, hjust = 1, colour = "black", size = 11.5),
      axis.text.y = element_text(colour = "black", size = 14),
      axis.title.x = element_text(colour = "black", size = 16, face = 'bold'),
      axis.title.y = element_text(colour = "black", size = 16, face = 'bold'),
      strip.text = element_text(size = 12),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.background = element_rect(),
      legend.title = element_text(size = 14, face = 'bold'),
      legend.text = element_text(size = 12)
    ) +
    labs(x = "Year", y = expression("Trophic level"))

}
# Presentation VMSD scenario TLs ------------------------------------------
predictions <- ecoind_sim %>% select(Time, TL.catch, TL.community)

start_date2 <- as.Date("2024-01-01")   # Define start date
monthly_sequence2 <- seq.Date(from = start_date2, by = "month", length.out = 913)
predictions2 <- data.frame("Time" = monthly_sequence2, "good_catch" = NA, "neutral_catch" = NA, "bad_catch" = NA, "good_com" = NA, "neutral_com" = NA, "bad_com" = NA)
predictions2[1,2:4] <- predictions[nrow(predictions),2]
predictions2[1,5:7] <- predictions[nrow(predictions),3]
for(i in 2:nrow(predictions2)){
  predictions2$good_catch[i] <- predictions2$good_catch[i-1] * 1.00005
  predictions2$neutral_catch[i] <- predictions2$neutral_catch[1]
  predictions2$bad_catch[i] <- predictions2$bad_catch[i-1] * 0.99995

  predictions2$good_com[i] <- predictions2$good_com[i-1] * 1.00005
  predictions2$neutral_com[i] <- predictions2$neutral_com[1]
  predictions2$bad_com[i] <- predictions2$bad_com[i-1] * 0.99995
}


EcoInd_TL_graph <<- ggplot() +
  #geom_line(data = ecoind_mc_grouped, aes(x = Time, y = med_TL_catch, colour = "TL catch", linetype = "median"), linewidth = 1) +
  #geom_line(data = ecoind_mc_grouped, aes(x = Time, y = med_TL_community, colour = "TL community", linetype = "median"), linewidth = 1) +
  #geom_ribbon(data = ecoind_mc_grouped, aes(x = Time, ymin = min_TL_catch, ymax = max_TL_catch), fill = "#354d9b", alpha = 0.15) +
  #geom_ribbon(data = ecoind_mc_grouped, aes(x = Time, ymin = min_TL_community, ymax = max_TL_community), fill = "#31b7bc", alpha = 0.15) +
  #geom_line(data = ecoind_sim, aes(x = Time, y = TL.catch, colour = "TL catch", linetype = "best fit"), linewidth = 1) +
  geom_line(data = ecoind_sim, aes(x = Time, y = TL.community, colour = "TL community", linetype = "best fit"), linewidth = 3) +
  #geom_line(data = predictions2, aes(x = Time, y = good_catch, colour = "good", linetype = "prediction"), linewidth = 1) +
  geom_line(data = predictions2, aes(x = Time, y = good_com, colour = "good", linetype = "prediction"), linewidth = 3) +
  #geom_line(data = predictions2, aes(x = Time, y = neutral_catch, colour = "neutral", linetype = "prediction"), linewidth = 1) +
  geom_line(data = predictions2, aes(x = Time, y = neutral_com, colour = "neutral", linetype = "prediction"), linewidth = 3) +
  #geom_line(data = predictions2, aes(x = Time, y = bad_catch, colour = "bad", linetype = "prediction"), linewidth = 1) +
  geom_line(data = predictions2, aes(x = Time, y = bad_com, colour = "bad", linetype = "prediction"), linewidth = 3) +
  scale_colour_manual(values = c("TL catch" = "#354d9b", "TL community" = "#31b7bc", "good" = "green3", neutral = "yellow2", "bad" = "red2"),
                      name = NULL) +
  scale_linetype_manual(values = c("best fit" = "solid", "prediction" = "dashed"),
                        name = NULL) +
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1, colour = "black", size = 11.5),
    axis.text.y = element_text(colour = "black", size = 14),
    axis.title.x = element_text(colour = "black", size = 16, face = 'bold'),
    axis.title.y = element_text(colour = "black", size = 16, face = 'bold'),
    strip.text = element_text(size = 12),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.background = element_rect(),
    legend.title = element_text(size = 14, face = 'bold'),
    legend.text = element_text(size = 12)
  ) +
  labs(x = "Year", y = expression("Trophic level"))
# MC output   ------------------------------------------------------------------
# required info to run the code
# directory to folder with MC output (folders)
fd_MC = 'C:/Users/stevenp/OneDrive - VLIZ/Documents/EwE output/SBNS_1991_V12_YearlyAverageTemp - rec_cod/mc_YearlyAverageTemperature_0,01/'
fd_bestfit = 'C:/Users/stevenp/OneDrive - VLIZ/Documents/EwE output/SBNS_1991_V12_YearlyAverageTemp - rec_cod/best_fit'
fd_ourfit = 'C:/Users/stevenp/OneDrive - VLIZ/Documents/EwE output/SBNS_1991_2023_V13_TechnicalReport/ecosim_YearlyAverageTemperature/our fit'
fd_classes = "C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/EMBENS Rpath/"
fd_TS = "C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/SBNS models/TS_SBNS_V23_Ffull2024_Bfull2023_Hooks2.csv"
fd_MC_limits = "C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/EMBENS Rpath/"
fd_EcoInd = 'C:/Users/stevenp/OneDrive - VLIZ/Documents/EwE output/B_SBNS_1991_V12_YearlyAverageTemp - rec_cod/EcoInd/'
#get_options_MC(fd_MC)


# total number of trials ran
total_trials <- 1000
param <- "biomass" # biomass now annual

#get best X% trials based on lowest SS
SS_MC(fd_MC,total_trials,100)
ss_mc_90 <- ss_mc_90[-which(ss_mc_90$SS > 1600),]
# get parameter outputs
param_MC()
param_monthly_MC()
#param_mc_combined$param <- as.numeric(param_mc_combined$param)

#out of MC limits
trials_out_of_CV_limits <- unique(outside_limits(fd_MC_limits,param_mc_combined,"B"))
test <- param_mc_combined[-which(unique(trials_out_of_CV_limits) %in% param_mc_combined$trial),]
#check for outliers
#out_trials <- get_outlier(param_mc_combined)

#param_mc_combined$trial <- as.numeric(param_mc_combined$trial)
#test <- param_mc_combined[-which(param_mc_combined$trial %in% as.numeric(unique(out_trials$trial))),]

# calculate median & quantiles per species per year
param_mc_combined$param<-as.numeric(param_mc_combined$param)
param_MC_summary(param_mc_combined,"Q") #"minmax" (minimum and maximum) or "Q" (quantiles)

# load best fit, our fit and TS
param_bestfit <- best_fit_param(fd_bestfit)
param_ourfit <- our_fit_param(fd_ourfit)
param_TS <- TS_param(fd_TS,param_bestfit)

# you can select one species or one class
subset_by_species <- NA # if not required = NA
subset_by_class <- NA # if not required = NA

# you can select several species or classes by adjusting the function to subset the df with only the preferred species/classes
multiple_FGs <- "yes" # "yes" or "no"
specific_subset <- function(df){subset(df, species == "Sole (adult)"|species == "Juvenile Sole"|species == "Plaice (adult)"|species == "Juvenile Plaice"|species == "Herring (adult)"|species == "Whiting (adult)")
                              }


# species the correct label on y-axis
y_title <- expression(paste("Biomass(t  ",km^-2,")"))

# plot
# provide df with median & quantiles, df with best fit, df with our fit, df with TS, the parameter, specific subset with multiple FGs, 1species t subset, 1class to subset, y-axis title
MC_plot_param(param_mc_grouped, param_bestfit, param_ourfit, param_TS, param, multiple_FGs, subset_by_species, subset_by_class, y_title)



#EcoInd
EcoInd(fd_EcoInd, "quantiles") #directory and minmax or quantiles
EcoInd_TL_graph
EcoInd_B_graph
EcoInd_C_graph

