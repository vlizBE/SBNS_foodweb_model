area_IVc_km2 <- 63633.85614 #km2

# subset to IVc division and year 1991 HISTORICAL LANDINGS
NS_fisheries_catch <- read.csv("data/ICES_1950-2010.csv", header = TRUE, stringsAsFactors = FALSE)
IVc_fisheries_catch <- subset(NS_fisheries_catch_91, NS_fisheries_catch_91$Division== "IV c")
IVc_fisheries_catch <- IVc_fisheries_catch[,-3]
for (i in 3:63){
  IVc_fisheries_catch[which(IVc_fisheries_catch[,i]=="."),i] <- 0
  IVc_fisheries_catch[which(IVc_fisheries_catch[,i]=="-"),i] <- 0
  IVc_fisheries_catch[which(IVc_fisheries_catch[,i]=="<0.5"),i] <- 0.5
}

#change to numeric instead of character
  for (j in 3:63){
IVc_fisheries_catch[,j] <- as.numeric(IVc_fisheries_catch[,j])
  }


#assign species from IVc_fisheries_catch not occuring in total landings species list to one that does
IVc_fisheries_catch$Species[which(IVc_fisheries_catch$Species=="Cuttlefish,bobtail squids nei")] <- "Cuttlefish, bobtail squids nei"
IVc_fisheries_catch$Species[which(IVc_fisheries_catch$Species=="Crangon shrimps nei")] <- "Common shrimp"
IVc_fisheries_catch$Species[which(IVc_fisheries_catch$Species=="Red mullet")] <- "Surmullets(=Red mullets) nei"
IVc_fisheries_catch$Species[which(IVc_fisheries_catch$Species=="Aesop shrimp")] <- "Pandalus shrimps nei"
IVc_fisheries_catch$Species[which(IVc_fisheries_catch$Species=="Clupeoids nei")] <- "Herrings, sardines nei"
IVc_fisheries_catch$Species[which(IVc_fisheries_catch$Species=="Scallops nei")] <- "Queen scallop"

#write.csv(rownames(total_catch),"data/totalcatchspecies.csv")  # list with all the caught species names according to the IVc_fisheries_catch df

# assign species to EwE FGs
EwE_FG <- read.csv("data/totalcatchspecies.csv",header = TRUE)
EwE_FG$Species <- as.character(EwE_FG$Species)
EwE_FG$EwE_FG <- as.character(EwE_FG$EwE_FG)


# check if all species are in FGs or excluded
check <- NULL
IVc_fisheries_catch$EwE_FG <- NA
for (i in c(1:length(IVc_fisheries_catch$Species))){
  match <- which(IVc_fisheries_catch$Species[i]==EwE_FG$Species)
  if (length(match)==0){
    check <- rbind(check,i)
    next
  }
  IVc_fisheries_catch$EwE_FG[i] <- as.character(EwE_FG$EwE_FG[match])
}

#######################################################################
# All these species are not in FG or excluded -> change manual OR add to excel list OR exclude all (now as FG NA)
IVc_fisheries_catch$Species[check]

IVc_fisheries_catch$EwE_FG[which(IVc_fisheries_catch$Species=="Norway pout")] <- "Other gadoids"
IVc_fisheries_catch$EwE_FG[which(IVc_fisheries_catch$EwE_FG=="rays")] <- "Rays"
IVc_fisheries_catch$EwE_FG[94] <- "ND"  # Greenland halibut
IVc_fisheries_catch$EwE_FG[which(IVc_fisheries_catch$Species=="Sturgeons nei")] <- "ND"

# deze lijn zet ze voorlopig in FG ND dus worden eruit gefilterd
IVc_fisheries_catch$EwE_FG[which(is.na(IVc_fisheries_catch$EwE_FG))] <- "ND"

#######################################################################
# FISHERIES LANDINGS 1991 using landings from 2003 -----------------------------------------
#species total landings
subset_year <- IVc_fisheries_catch

total_landings <- array(NA,dim= c(length(unique(subset_year$EwE_FG)),61))
row.names(total_landings) <- c(unique(subset_year$EwE_FG))
colnames(total_landings) <- c(1950:2010)

for(sp in unique(subset_year$EwE_FG)){
  subset_species <- subset(subset_year,subset_year$EwE_FG==sp)
  total_landings[sp,] <- colSums(subset_species[,c(3:63)])
}
