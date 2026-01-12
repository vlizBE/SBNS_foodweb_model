# DIET COMPOSITION IVc
# DIET ---------------------------------------

diet_IVbc <- read.csv("data/diet IVbc_SP_01042021.csv", header = TRUE, stringsAsFactors = FALSE)
bio_IVbc <- read.csv("data/biomass_IVbc_SP_01042021.csv", header = TRUE, stringsAsFactors = FALSE)

#rows and cols to eliminate
eliminate <- c(6,7,8,10,11,12,17,18,19,20,22,33,37,38,39,40,41,42,43,44,45,48,53,58,63,64,67)
eliminate2 <- c(6,7,8,10,11,12,17,18,19,20,22,33,37,38,39,40,41,42,43,44,45,48,53,58,63,64)
# weighted mean ------------------
diet_IVbc_copy <- diet_IVbc
colnames(diet_IVbc) <- c("species", diet_IVbc[-c(65:71),1])
rownames(diet_IVbc) <- diet_IVbc[,1]
diet_IVbc <- diet_IVbc[-c(70,71),-1]
bio_IVbc_copy <- bio_IVbc

# sharks (Juvenile sharks, Spurdog, Large piscivorous sharks, Small sharks)
y = bio_IVbc[c(5:8),2]/sum(bio_IVbc[c(5:8),2])

diet <-  array(NA, dim =c(69,1), list(c(1:69),"sharks"))
for (i in c(1:69)){
  diet[i,1] <-  weighted.mean(diet_IVbc[i,c(5:8)],y)
}
sharks_pred <- diet

# rays (Juvenile rays, Starry ray + others, Thornback & Spotted ray, Skate + cuckoo ray)
y = bio_IVbc[c(9:12),2]/sum(bio_IVbc[c(9:12),2])

diet <-  array(NA, dim =c(69,1), list(c(1:69),"rays"))
for (i in c(1:69)){
  diet[i,1] <-  weighted.mean(diet_IVbc[i,c(9:12)],y)
}
rays_pred <- diet

# flatfish (flounder, lemon sole, turbot, brill)
y = bio_IVbc[c(34,37,39,40),2]/sum(bio_IVbc[c(34,37,39,40),2])

diet <-  array(NA, dim =c(69,1), list(c(1:69),"flatfish"))
for (i in c(1:69)){
  diet[i,1] <-  weighted.mean(diet_IVbc[i,c(34,37,39,40)],y)
}
flat_fish_pred <- diet

# demersal fish (gurnards, dragonets, small demersal fish)
y = bio_IVbc[c(23,43,45),2]/sum(bio_IVbc[c(23,43,45),2])

diet <-  array(NA, dim =c(69,1), list(c(1:69),"demersal fish"))
for (i in c(1:69)){
  diet[i,1] <-  weighted.mean(diet_IVbc[i,c(23,43,45)],y)
}
demersal_fish_pred <- diet

# benthos (code not used when benthos is not aggregated)
#y = bio_IVbc[c(54,55,59:62),2]/sum(bio_IVbc[c(54,55,59:62),2])

#diet <-  array(NA, dim =c(69,1), list(c(1:69),"benthos"))
#for (i in c(1:69)){
#  diet[i,1] <-  weighted.mean(diet_IVbc[i,c(54,55,59:62)],y)
#}
#benthos_pred <- diet

# Large crab (& shrimp)
y = bio_IVbc[c(52,58),2]/sum(bio_IVbc[c(52,58),2])

diet <-  array(NA, dim =c(69,1), list(c(1:69),"large crab"))
for (i in c(1:69)){
  diet[i,1] <-  weighted.mean(diet_IVbc[i,c(52,58)],y)
}
large_crab_pred <- diet

# sum of preyed on ------------------
# sharks
preyed_on <-  array(NA, dim =c(1,64), list("sharks",c(1:64)))
for (i in c(1:64)){
  preyed_on[1,i] <- sum(diet_IVbc[c(5:8),i])
}
sharks_prey <- preyed_on

# rays
preyed_on <-  array(NA, dim =c(1,64), list("rays",c(1:64)))
for (i in c(1:64)){
  preyed_on[1,i] <-  sum(diet_IVbc[c(9:12),i])
}
rays_prey <- preyed_on

# demersal fish
preyed_on <-  array(NA, dim =c(1,64), list("demersal fish",c(1:64)))
for (i in c(1:64)){
  preyed_on[1,i] <-  sum(diet_IVbc[c(23,43,45),i])
}
demersal_fish_prey <- preyed_on

# flatfish
preyed_on <-  array(NA, dim =c(1,64), list("flatfish",c(1:64)))
for (i in c(1:64)){
  preyed_on[1,i] <-  sum(diet_IVbc[c(34,37,39,40),i])
}
flat_fish_prey <- preyed_on

# benthos (code not used when benthos not aggregated)
#preyed_on <-  array(NA, dim =c(1,64), list("benthos",c(1:64)))
#for (i in c(1:64)){
#  preyed_on[1,i] <-  sum(diet_IVbc[c(54,55,59:62),i])
#}
#benthos_prey <- preyed_on

# Large crab (& shrimp)
preyed_on <-  array(NA, dim =c(1,64), list("large crab",c(1:64)))
for (i in c(1:64)){
  preyed_on[1,i] <-  sum(diet_IVbc[c(52,58),i])
}
large_crab_prey <- preyed_on

# Detritus
preyed_on <-  array(NA, dim =c(1,64), list("detritus",c(1:64)))
for (i in c(1:64)){
  preyed_on[1,i] <-  sum(diet_IVbc[c(66,67),i])
}
detritus_prey <- preyed_on

# change IVbc diet with IVc diet
## group diets
diet_IVbc[,5] <- sharks_pred
diet_IVbc[,9] <- rays_pred
diet_IVbc[,23] <- demersal_fish_pred
diet_IVbc[,34] <- flat_fish_pred
#diet_IVbc[,54] <- benthos_pred (code not used when benthos not aggregated)
diet_IVbc[,52] <- large_crab_pred
# combine FGs as prey as well
diet_IVbc[5,] <- sharks_prey
diet_IVbc[9,] <- rays_prey
diet_IVbc[23,] <- demersal_fish_prey
diet_IVbc[34,] <- flat_fish_prey
#diet_IVbc[54,] <- benthos_prey (code not used when benthos not aggregated)
diet_IVbc[52,] <- large_crab_prey
diet_IVbc[66,] <- detritus_prey

# eliminate the FGs not needed
test <- diet_IVbc[-eliminate,-eliminate2]
write.csv(test, "data/diet test 26022024.csv", row.names = TRUE)
test <- cbind(test[,c(1:24)],0,test[,c(25:30)],0,test[,c(31:38)])
test <- rbind(test[c(1:24),],0,test[c(25:30),],0,test[c(31:42),])

param_data <- read.csv("data/parameters Rpath - 27022024.csv", header = TRUE, stringsAsFactors = FALSE)
rownames(test) <- c(param_data$group[-c(44:60)],"Import")
colnames(test) <- param_data$group[-c(41:58)]


#diet_discard_seabirds <- read.csv("data/diet_discard_seabirds.csv", header = TRUE, stringsAsFactors = FALSE)
#diet_nondiscard_seabirds <- read.csv("data/diet_nondiscard_seabirds.csv", header = TRUE, stringsAsFactors = FALSE)
#diet_harbour_popoise <- read.csv("data/Diet harbour porpoise.csv", header = TRUE, stringsAsFactors = FALSE)
#diet_harbour_popoise$X <-  c(param_data$group[-c(46:60)],"Import")
#for (a in diet_discard_seabirds$species[-21]){
#  test$`Seabirds (discard)`[which(rownames(test) == a)] <- diet_discard_seabirds$diet_percentage_corrected[which(diet_discard_seabirds$species == a)]
#}
#for (b in diet_nondiscard_seabirds$species[-20]){
#  test$`Seabirds (non-discard)`[which(rownames(test) == b)] <- diet_nondiscard_seabirds$diet_percentage_corrected[which(diet_nondiscard_seabirds$species == b)]
#}
#for (c in diet_harbour_popoise$X){
#  test$`Harbour porpoise`[which(rownames(test) == c)] <- diet_harbour_popoise$Harbour.porpoise[which(diet_harbour_popoise$X == c)]
#}

diet_IVc <- test # safety control step

# diet seabass -----------------------------------------------------
seabass1 <- c(18.7,20.1,40.1,1,5.9,1.5,4.2,0.3,0.9,2.6,2.9,0.1)/98.3 #Spitz et al 2013
seabass2 <- c(0.55,0.06,2.365,36.02,0.36,0.555,71.63)/111.54 #LaFaille 2001
seabass3 <- c(1.9,15.2,71.4,2)/90.5 #CEFAS
diet_IVc[26,25] <- (0.190233978+0.02099448) /3                                    #Pelagic fish
diet_IVc[17,25] <-(0.204476094/3)                                                 #Horse mackerel
diet_IVc[16,25] <-(0.407934893 /3)                                                #Mackerel
diet_IVc[11,25] <-(0.060020346 /3)                                                #other gadoids
diet_IVc[9,25] <-(0.015259410 /3)* (bio_IVbc[15,2]/sum(bio_IVbc[c(15,16),2]))       #juv. whiting
diet_IVc[10,25] <- (0.015259410 /3)* (bio_IVbc[16,2]/sum(bio_IVbc[c(15,16),2]))    #ad. whiting
diet_IVc[12,25] <-(0.042726348 +0.02099448 )/3                                    #demersal fish
diet_IVc[23,25] <-(0.003051882 /3)/ (bio_IVbc[35,2]/sum(bio_IVbc[c(35,36),2]))    #juv. sola
diet_IVc[24,25] <-(0.003051882 /3)/ (bio_IVbc[36,2]/sum(bio_IVbc[c(35,36),2]))    #ad. sole
diet_IVc[18,25] <-(0.009155646 /3)                                                #sandeels
diet_IVc[27,25] <-(0.026449644 +0.0005379236 )/3                                  #squid & cuttlefish
diet_IVc[35,25] <-((0.026449644 +0.0212031558 +0.16795580 )/3) * (bio_IVbc[57,2]/sum(bio_IVbc[c(56,57),2]))  #crangon crangon <5
diet_IVc[36,25] <-((0.026449644 +0.0212031558 +0.16795580 )/3) * (bio_IVbc[56,2]/sum(bio_IVbc[c(56,57),2]))  #crangon crangon
diet_IVc[31,25] <-(0.029501526 +0.3229334768 +0.78895028 )/3                      #crab & shrimps
diet_IVc[33,25] <-(0.0032275417/3)                                                #epifaunal benthos
diet_IVc[44,25] <-(0.0049757934 +0.02209945)/3                                    #import
diet_IVc[29,25] <-(0.6421911422/3)                                                #copepods

# preyed upon
seabass_demersal_ratio <- 0.003890698/(5.065607+0.003890698) #biomass seabass and demersal fish
demersal_seabass_ratio <- 5.065607/(5.065607+0.003890698)
diet_IVc[25,] <- diet_IVc[12,] * seabass_demersal_ratio
diet_IVc[12,] <-  diet_IVc[12,] * demersal_seabass_ratio

# add aquaculture blue mussels' diets ------------------------------------------
diet_IVc[c(29,41,42),"Blue mussels (aquaculture)"] <- c(0.01,0.85,0.14) # prey
diet_IVc["Blue mussels (aquaculture)", c(2,3,4,8,10,12)] <- 0.0001   # predator:  small portion given, can be adapted (6,18,19,21,22; rays, flatfish etc not as longline)

# Diet total to 1 -----------------------------------
sumofdiet <- apply(diet_IVc,2,sum)
for (i in 1:length(diet_IVc[1,])){
  for (row in 1:44){
    diet_IVc[row,i] <- diet_IVc[row,i]/sumofdiet[i]
  }
}

apply(diet_IVc,2,sum) # check if sum of diets is 1

# write diet csv file --------------------------------------
write.csv(diet_IVc, "data/diet IVc.csv", row.names = TRUE)

# update diets compositions based an DAPSTOM data set ----------------------------------------------------------
############ DIET composition
library(dplyr)
library(ggplot2)

##########
our_species <- read.csv("C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/ECOSIM/speciesListGoM_QAQC_AMkeys_MS.csv")
ewe_fgs <- read.csv("C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/ECOSIM/ewe_fgs.csv")
diets <- read.csv("C:/Users/stevenp/OneDrive - VLIZ/Documents/stevenp/Ecopath with Ecosim/ECOSIM/download_12864_'DAPSTOM view'.csv")

############## subsetting years
diets <- diets %>% select("year",
                          "ices.division",
                          "num.stomachs",
                          "mean.length.cm",
                          "pred.wgt.g",
                          "pred.taxa",
                          "pred.funct.grp",
                          "cpw",
                          "prey.aphiaid",
                          "prey.taxa",
                          "prey.funct.grp")
diets_1991_present <- subset(diets, year > 1990 & year < 1993)
#diets_1991_present_NS <- subset(diets_1991_present, ices.division == "IVc"|ices.division =="IVb"|ices.division =="IVa"|ices.division =="VIId")

diets_1991_present_our_species <- diets_1991_present[diets_1991_present$pred.taxa %in% our_species$Sciname,]
#diets_1991_present_our_species <- na.omit(diets_1991_present_our_species)

########### assigning ewe groups
diets_1991_present_our_species$EwE <- NA
for (i in unique(diets_1991_present_our_species$pred.taxa)){
  diets_1991_present_our_species$EwE[which(diets_1991_present_our_species$pred.taxa==i)] <- our_species$EwE_name[which(our_species$Sciname==i)]
}

for (i in unique(diets_1991_present_our_species$prey.taxa)){
  if (i %in% our_species$Sciname){
    diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa==i)] <- our_species$EwE_name[which(our_species$Sciname==i)]
  }
}

#diets_1991_present_our_species <- na.omit(diets_1991_present_our_species)


diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Calanoida")] <- "Herbivorous plankton (copepods)"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Temora longicornis")] <- "Herbivorous plankton (copepods)"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Euphausiidae")] <- "Herbivorous plankton (copepods)"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Gadidae")] <- "Other gadoids"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Pollachius virens")] <- "Other gadoids"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Ammodytes")] <- "Sandeels"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Cephalopoda")] <- "Squid & cuttlefish"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Anomalocera patersoni")] <- "Herbivorous plankton (copepods)"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Arthropoda")] <- "Large crabs + shrimps "
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Gastropoda")] <- "Epifaunal macrobenthos (mobile grazers)"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Cancridae")] <- "Large crabs + shrimps "
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Calanus")] <- "Herbivorous plankton (copepods)"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Decapoda")] <- "Large crabs + shrimps "
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Podon")] <- "Herbivorous plankton (copepods)"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Callionymus lyra")] <- "Demersal fish"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Hippoglossoides platessoides")] <- "Other flatfish"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Pleuronectiformes")] <- "Other flatfish"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Gammaridae")] <- "Herbivorous plankton (copepods)"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Caligus")] <- "Carnivorous zooplankton"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Candacia")] <- "Herbivorous plankton (copepods)"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Evadne nordmanni")] <- "Herbivorous plankton (copepods)"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Oikopleura")] <- ""
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Polyphemidae")] <- "Herbivorous plankton (copepods)"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Centropages typicus")] <- "Herbivorous plankton (copepods)"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Parasagitta elegans")] <- "Carnivorous zooplankton"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Polychaeta")] <- "Small infauna (polychaetes)"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Hyperoche medusarum")] <- "Herbivorous plankton (copepods)"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Hyperia galba")] <- "Carnivorous zooplankton"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Parathemisto abyssorum")] <- "Carnivorous zooplankton"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Metridia")] <- "Herbivorous plankton (copepods)"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Maxillopoda")] <- "Herbivorous plankton (copepods)"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Nematoda")] <- "Meiofauna"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Glyptocephalus cynoglossus")] <- "Other flatfish"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Melanogrammus aeglefinus")] <- "Other gadoids"
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Nephrops norvegicus")] <- "Large crabs + shrimps "
diets_1991_present_our_species$prey.taxa[which(diets_1991_present_our_species$prey.taxa== "Hyperiidae")] <- "Carnivorous zooplankton"



`%not_in%` <- purrr::negate(`%in%`)
not_in_EwE <- which(diets_1991_present_our_species$prey.taxa %not_in% ewe_fgs$Group.name)
unique(diets_1991_present_our_species$prey.taxa[not_in_EwE])

############# predators diets
predators <- NULL

for (i in unique(diets_1991_present_our_species$EwE)){
  subset_pred <- subset(diets_1991_present_our_species, EwE == i)
  predators[[i]] <-aggregate(subset_pred$cpw, by=list(Category=subset_pred$prey.taxa), FUN=sum)
}


########### COD - SQUID
squids <- c("Squid & cuttlefish", "Sepiola atlantica", "Loligo", "Loliginidae",  "Octopodidae")
cod <- subset(diets_1991_present_our_species, pred.taxa == "Gadus morhua")
juvenile_cod <-subset(cod, mean.length.cm < 55)
adult_cod <- subset(cod, mean.length.cm > 54)

portion_squid_juvcoddiet <- sum(juvenile_cod$cpw[juvenile_cod$prey.taxa %in% squids])/sum(juvenile_cod$cpw)
portion_squid_adcoddiet <- sum(adult_cod$cpw[adult_cod$prey.taxa %in% squids])/sum(adult_cod$cpw)

# see year distribution
test_juv <- juvenile_cod[juvenile_cod$prey.taxa %in% squids,]
test_juv <- test_juv %>% group_by(year)
test_juv$year

test_ad <- adult_cod[adult_cod$prey.taxa %in% squids,]
test_ad <- test_ad %>% group_by(year)
test_ad$year



############## WHITING - SQUID
squids <- c("Squid & cuttlefish", "Sepiola atlantica", "Loligo", "Loliginidae",  "Octopodidae")
whiting <- subset(diets_1991_present_our_species, pred.taxa == "Merlangius merlangus")
juvenile_whiting <-subset(whiting, mean.length.cm < 28.2)
adult_whiting <- subset(whiting, mean.length.cm > 28.1)

portion_squid_juvwhitingdiet <- sum(juvenile_whiting$cpw[juvenile_whiting$prey.taxa %in% squids])/sum(juvenile_whiting$cpw)
portion_squid_adwhitingdiet <- sum(adult_whiting$cpw[adult_whiting$prey.taxa %in% squids])/sum(adult_whiting$cpw)

# see year distribution
test_juv <- juvenile_whiting[juvenile_whiting$prey.taxa %in% squids,]
test_juv <- test_juv %>% group_by(year)
test_juv$year

test_ad <- adult_whiting[adult_whiting$prey.taxa %in% squids,]
test_ad <- test_ad %>% group_by(year)
test_ad$year

