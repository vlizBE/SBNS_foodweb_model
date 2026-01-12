area_IVc_km2 <- 63633.85614 #km2

#FISHERIES LANDINGS AND DISCARDS
# assigning EwE fleets with fishing techn and gear -----------------------------------------
# subset to IVc division and year 1991 STECF
NS_fisheries_catch <- read.csv("data/Fisheries Landing STECF IVc.csv", header = TRUE, stringsAsFactors = FALSE)
NS_fisheries_catch <- subset(NS_fisheries_catch,year==2003)

#asssign EwE fleet
techn_gear <- data.frame("techn"= NS_fisheries_catch$mesh.size.range,"gear"= NS_fisheries_catch$gear,"EwE_fleet" = NA)
techn_gear$EwE_fleet[which(techn_gear$techn=='<16' & techn_gear$gear=='BEAM')] <- 'shrimp'
techn_gear$EwE_fleet[which(techn_gear$techn=='16-31' & techn_gear$gear=='BEAM')] <- 'shrimp'
techn_gear$EwE_fleet[which(techn_gear$techn!='16-31' & techn_gear$techn !='<16' & techn_gear$gear=='BEAM')] <- 'beam'
techn_gear$EwE_fleet[which(techn_gear$gear=='GILL' | techn_gear$gear=='TRAMMEL')] <- 'nets'
techn_gear$EwE_fleet[which(techn_gear$gear=='DREDGE')] <- 'dredge'
techn_gear$EwE_fleet[which(techn_gear$gear=='PEL_TRAWL')] <- 'pelagic'
techn_gear$EwE_fleet[which(techn_gear$gear=='POTS')] <- 'pots'
techn_gear$EwE_fleet[which(techn_gear$gear=='LONGLINE')] <- 'hooks'
techn_gear$EwE_fleet[which(techn_gear$gear=='OTTER'| techn_gear$gear=='DEM_SEINE')] <- 'demersal'
techn_gear$EwE_fleet[which(techn_gear$gear=='NONE')] <- 'other'

#write.csv(techn_gear,"data/tech&gear&EwEfleet_2003.csv")

IVc_fisheries_catch <- NS_fisheries_catch
IVc_fisheries_catch$EwE_fleet <- techn_gear$EwE_fleet

# assigning caught species to EwE FG  -----------------------------------------
#write.csv(unique(IVc_fisheries_catch$species),"data/totalcatchspecies2003.csv")

#assign species to FGs
EwE_FG_2003 <- read.csv("data/totalcatchspecies2003.csv",header = TRUE)
EwE_FG_2003$Species <- as.character(EwE_FG_2003$Species)
EwE_FG_2003$EwE_FG <- as.character(EwE_FG_2003$EwE_FG)

IVc_fisheries_catch$EwE_FG <- NA
check <- NULL
for (i in c(1:length(IVc_fisheries_catch$species))){
  match <- which(IVc_fisheries_catch$species[i]==EwE_FG_2003$Species)
  if (length(match)==0){
    check <- rbind(check,i)
    next
  }
  IVc_fisheries_catch$EwE_FG[i] <- as.character(EwE_FG_2003$EwE_FG[match])
}

#FISHERIES LANDINGS 1991 using landings from 2003 -----------------------------------------
#species landing per country, per fleet (for 2003)
country_landings <- NULL
subset_year <- IVc_fisheries_catch
for (c in unique(subset_year$country)){
  subset_country <- subset(subset_year,country==c)
  landings <- NULL
  for (fl in unique(subset_country$EwE_fleet)) {
    subset_landings <- subset(subset_country,EwE_fleet==fl)
    species_landings <- array(NA,dim= c(length(unique(subset_landings$EwE_FG)),1),list(c(unique(subset_landings$EwE_FG)),"landings"))
    for(sp in unique(subset_landings$EwE_FG)){
      subset_species <- subset(subset_landings,EwE_FG==sp)
      species_landings[sp,] <- sum(subset_species$landings)
    }
    landings[[fl]] <- as.data.frame(species_landings)
  }
  country_landings[[c]] <- landings
}

#species total landings
subset_year <- IVc_fisheries_catch
total_landings <- array(NA,dim= c(length(unique(subset_year$EwE_FG)),1),list(c(unique(subset_year$EwE_FG)),"catch"))
for(sp in unique(subset_year$EwE_FG)){
  subset_species <- subset(subset_year,subset_year$EwE_FG==sp)
  total_landings[sp,] <- sum(subset_species$landings)
}

#fleet proportion to total species landings per country per fleet
country_ratio <- NULL

for (c in unique(subset_year$country)){
  subset_country <- country_landings[[c]]
  ratio <- NULL
  for (fl in names(subset_country)) {
    subset_landings <- subset_country[[fl]]
    ratio_sp_landings <- array(NA,dim= c(length(rownames(subset_landings)),1),list(c(rownames(subset_landings)),"ratio"))
    for(sp in rownames(subset_landings)){
      ratio_sp_landings[sp,] <- subset_landings[which(rownames(subset_landings)==sp),]/total_landings[which(rownames(total_landings)==sp),]
    }
    ratio[[fl]] <- as.data.frame(ratio_sp_landings)
  }
  country_ratio[[c]] <- ratio
}

# subset to IVc division and year 1991 HISTORICAL LANDINGS
NS_fisheries_catch_91 <- read.csv("data/ICES_1950-2010.csv", header = TRUE, stringsAsFactors = FALSE)
IVc_fisheries_catch_91 <- subset(NS_fisheries_catch_91, NS_fisheries_catch_91$Division== "IV c")
IVc_fisheries_catch_91 <- IVc_fisheries_catch_91[,c('Country',"Species","X1991")]
IVc_fisheries_catch_91$X1991[which(IVc_fisheries_catch_91$X1991=="<0.5")] <- 0.5
IVc_fisheries_catch_91$X1991 <- as.numeric(IVc_fisheries_catch_91$X1991)
IVc_fisheries_catch_91 <- na.omit(IVc_fisheries_catch_91)

#assign species from IVc_fisheries_catch_91 not occuring in total landings species list to one that does
IVc_fisheries_catch_91$Species[which(IVc_fisheries_catch_91$Species=="Cuttlefish,bobtail squids nei")] <- "Cuttlefish - bobtail squids nei"
IVc_fisheries_catch_91$Species[which(IVc_fisheries_catch_91$Species=="Crangon shrimps nei")] <- "Common shrimp"
IVc_fisheries_catch_91$Species[which(IVc_fisheries_catch_91$Species=="Red mullet")] <- "Surmullets(=Red mullets) nei"
IVc_fisheries_catch_91$Species[which(IVc_fisheries_catch_91$Species=="Aesop shrimp")] <- "Pandalus shrimps nei"
IVc_fisheries_catch_91$Species[which(IVc_fisheries_catch_91$Species=="Clupeoids nei")] <- "Herrings, sardines nei"
IVc_fisheries_catch_91$Species[which(IVc_fisheries_catch_91$Species=="Scallops nei")] <- "Queen scallop"
IVc_fisheries_catch_91$Species[which(IVc_fisheries_catch_91$Species=="Porgies, seabreams nei")] <- "Porgies - seabreams nei"
IVc_fisheries_catch_91$Species[which(IVc_fisheries_catch_91$Species=="Octopuses, etc. nei")] <- "Octopuses - etc. nei"
IVc_fisheries_catch_91$Species[which(IVc_fisheries_catch_91$Species=="Herrings, sardines nei")] <- "Herrings - sardines nei"
IVc_fisheries_catch_91$Species[which(IVc_fisheries_catch_91$Species=="Gurnards, searobins nei")] <- "Gurnards - searobins nei"

#write.csv(rownames(total_catch),"C:/Users/stevenp/Documents/stevenp/Ecopath with Ecosim/Food web info/NS/Fisheries/totalcatchspecies.csv")

# assign species to EwE FGs
EwE_FG <- read.csv("data/totalcatchspecies.csv",header = TRUE)
EwE_FG$Species <- as.character(EwE_FG$Species)
EwE_FG$EwE_FG <- as.character(EwE_FG$EwE_FG)

check <- NULL
IVc_fisheries_catch_91$EwE_FG <- NA
for (i in c(1:length(IVc_fisheries_catch_91$Species))){
  match <- which(IVc_fisheries_catch_91$Species[i]==EwE_FG$Species)
  if (length(match)==0){
    check <- rbind(check,i)
    next
  }
  IVc_fisheries_catch_91$EwE_FG[i] <- as.character(EwE_FG$EwE_FG[match])
}
IVc_fisheries_catch_91$EwE_FG[which(IVc_fisheries_catch_91$Species=="Norway pout")] <- "Other gadoids"
IVc_fisheries_catch_91$EwE_FG[94] <- "ND" # greenland halibut
IVc_fisheries_catch_91$EwE_FG[which(IVc_fisheries_catch_91$Species=="Sturgeons nei")] <- "ND"

#unique(IVc_fisheries_catch_91$EwE_FG)
#unique(IVc_fisheries_catch$EwE_FG)

# multiply landings 1991 with catch ratio per country and fleet
#species total landings
subset_year_91 <- IVc_fisheries_catch_91
total_landings_91 <- array(NA,dim= c(length(unique(subset_year_91$EwE_FG)),1),list(c(unique(subset_year_91$EwE_FG)),"catch"))
for(sp in unique(subset_year_91$EwE_FG)){
  subset_species <- subset(subset_year_91,subset_year_91$EwE_FG==sp)
  total_landings_91[sp,] <- sum(subset_species$X1991)
}

catch_per_country <- NULL

for (c in unique(IVc_fisheries_catch$country)){
  subset_country <- country_ratio[[c]]
  catch_per_fleet <- NULL
  for (fl in names(subset_country)) {
    subset_catch <- subset_country[[fl]]
    same_species <- which(unique(rownames(subset_catch))%in%rownames(total_landings_91))
    sp_catch_91 <- array(NA,dim= c(length(rownames(subset_catch)[same_species]),1),list(c(rownames(subset_catch)[same_species]),"catch_91"))
    for(sp in rownames(subset_catch)[same_species]){
      sp_catch_91[sp,] <- sum(total_landings_91[which(rownames(total_landings_91)==sp)])*subset_catch[which(rownames(subset_catch)==sp),]
    }
    catch_per_fleet[[fl]] <- as.data.frame(sp_catch_91)
  }
  catch_per_country[[c]] <- catch_per_fleet
}
EwE_sumcatch_per_country <- catch_per_country

# from list to df
combined_fleet <- NULL
for (fl in c("shrimp","demersal","beam","nets","pots","other","pelagic","hooks","dredge")){
  t <- NULL
  for (c in c( "BEL", "NLD", "SCO", "DEU", "DNK", "ENG", "SWE", "GBJ")){
    if (fl %in% names(catch_per_country[[c]])){
      t <- catch_per_country[[c]][fl]
      t$EwE_FG <- rownames(data.frame(catch_per_country[[c]][fl]))
      t$fleet <-fl
      combined_fleet <- rbind(combined_fleet,data.frame(t))
    }
  }
}

df <- combined_fleet

df2 <- array(NA,dim = c(length(unique(df$EwE_FG)),length(unique(df$fleet))), list(c(unique(as.character(df$EwE_FG))),c(unique(as.character(df$fleet)))))
for (a in (unique(as.character(df$fleet)))){
  for (b in (unique(as.character(df$EwE_FG)))){
    if (length(which(as.character(df$EwE_FG)==b & as.character(df$fleet)==a)) != 0){
      df2[b,a] <- sum(df$catch_91[which(as.character(df$EwE_FG)==b & as.character(df$fleet)==a)])
    } else {
      df2[b,a] <- 0
    }
  }
}

# check if total catch equals sum of fleets per EwE FG
#checking <- data.frame("sum"=apply(df2,1,sum))
#total_landings_91

#FISHERIES ADULT-JUVENILE RATIO 1991 using landings and discards from 2003 -------------------
NS_fisheries_catch <- read.csv("data/Fisheries landing & discard at age NS.csv", header = TRUE, stringsAsFactors = FALSE) #https://stecf.jrc.ec.europa.eu/dd/effort/graphs-annex
NS_fisheries_catch <- subset(NS_fisheries_catch,year==2003)
unique(NS_fisheries_catch$regulated.gear)
#asssign EwE fleet
techn_gear <- data.frame("gear"= NS_fisheries_catch$regulated.gear,"EwE_fleet" = NA)
techn_gear$EwE_fleet[which(techn_gear$gear=='BEAM')] <- 'shrimp'
techn_gear$EwE_fleet[which(techn_gear$gear=='BT1')] <- 'beam'
techn_gear$EwE_fleet[which(techn_gear$gear=='BT2')] <- 'beam'
techn_gear$EwE_fleet[which(techn_gear$gear=='GN1' | techn_gear$gear=='GT1')] <- 'nets'
techn_gear$EwE_fleet[which(techn_gear$gear=='DREDGE')] <- 'dredge'
techn_gear$EwE_fleet[which(techn_gear$gear=='PEL_TRAWL'|techn_gear$gear=='PEL_SEINE')] <- 'pelagic'
techn_gear$EwE_fleet[which(techn_gear$gear=='POTS')] <- 'pots'
techn_gear$EwE_fleet[which(techn_gear$gear=='LL1')] <- 'hooks'
techn_gear$EwE_fleet[which(techn_gear$gear=='OTTER'|techn_gear$gear=='TR3'|techn_gear$gear=='TR1'|techn_gear$gear=='TR2'| techn_gear$gear=='DEM_SEINE')] <- 'demersal'
techn_gear$EwE_fleet[which(techn_gear$gear=='NONE')] <- 'other'

#write.csv(techn_gear,"data/tech&gear&EwEfleet_2003.csv")

IVc_fisheries_catch <- NS_fisheries_catch
IVc_fisheries_catch$EwE_fleet <- techn_gear$EwE_fleet

#write.csv(unique(IVc_fisheries_catch$species),"data/totalcatchspecies2003.csv")

#assign species to FGs
EwE_FG_2003 <- read.csv("data/totalcatchspecies2003.csv",header = TRUE)
EwE_FG_2003$Species <- as.character(EwE_FG_2003$Species)
EwE_FG_2003$EwE_FG <- as.character(EwE_FG_2003$EwE_FG)

check <- NULL
IVc_fisheries_catch$EwE_FG <- NA
for (i in c(1:length(IVc_fisheries_catch$species))){
  match <- which(IVc_fisheries_catch$species[i]==EwE_FG_2003$Species)
  if (length(match)==0){
    check <- rbind(check,i)
    next
  }
  IVc_fisheries_catch$EwE_FG[i] <- as.character(EwE_FG_2003$EwE_FG[match])
}
IVc_fisheries_catch$EwE_FG[c(check)] <- "ND"

#species mean landing & discards per fleet by age (for 2003)
catch <- NULL
discardsratio_ad_juv <- NULL

subset_year <- IVc_fisheries_catch
for (fl in unique(subset_year$EwE_fleet)) {
  subset_landings <- subset(subset_year,subset_year$EwE_fleet==fl)
  species_landings <- array(NA,dim= c(length(unique(subset_landings$EwE_FG)),2),list(c(unique(subset_landings$EwE_FG)),c("landings","discards")))
  discardsratio <- array(NA,dim= c(length(unique(subset_landings$EwE_FG)),1),list(c(unique(subset_landings$EwE_FG)),c("ratio")))
  for(sp in unique(subset_landings$EwE_FG)){
    subset_species <- subset(subset_landings,subset_landings$EwE_FG==sp)
    species_landings[sp,"landings"] <- sum(na.omit(subset_species$sum_landings))
    species_landings[sp,"discards"] <- sum(na.omit(subset_species$sum_discards))
    catch[[fl]] <- species_landings
    if (sp %in% c("Cod (adult)", "Plaice (adult)","Sole (adult)", "Whiting (adult)")){    # for herring no information per age class ==> assuming all discards is juvenile
      discardsratio[sp,"ratio"] <- sum(na.omit(c(subset_species$age0_d,subset_species$age1_d,subset_species$age2_d)))/
        sum(na.omit(c(subset_species$age0_d,subset_species$age1_d,subset_species$age2_d,subset_species$age3_d,subset_species$age4_d,subset_species$age5_d,
                      subset_species$age6_d,subset_species$age7_d,subset_species$age8_d,subset_species$age9_d,subset_species$age10_d,subset_species$age11_d)))
      discardsratio_ad_juv[[fl]] <- discardsratio
    }

  }
}

#discard - landings & juv-ad ratio per fleet
fleet_ratio <- NULL

for (fl in names(catch)) {
  subset_landings <- catch[[fl]]
  ratio_sp_landings <- array(NA,dim= c(length(rownames(subset_landings)),1),list(c(rownames(subset_landings)),"discard_ratio"))
  for(sp in rownames(subset_landings)){
    ratio_sp_landings[sp,] <- subset_landings[which(rownames(subset_landings)==sp),'discards']/subset_landings[which(rownames(subset_landings)==sp),'landings']
    fleet_ratio[[fl]] <- ratio_sp_landings
  }
}

# DISCARDS of 1991 catch estimates
discards_df <- NULL

for (fl in names(fleet_ratio)) {
  subset_discard_ratio <- fleet_ratio[[fl]]
  discards <- array(NA,dim= c(length(rownames(subset_discard_ratio)),1),list(c(rownames(subset_discard_ratio)),"discards"))
  for(sp in rownames(subset_discard_ratio)){
    discards[sp,] <- df2[which(rownames(df2)==sp),fl]*subset_discard_ratio[which(rownames(subset_discard_ratio)==sp)]
    discards_df[[fl]] <- discards
  }
}

#omzetten naar df
df_disc <- NULL
for (i in names(discards_df)){
  df_disc <- rbind(df_disc,data.frame('discards' = discards_df[[i]],'EwE_FG' = rownames(discards_df[[i]]), "fleet" = i))
}

df2_disc <- array(NA,dim = c(length(unique(df_disc$EwE_FG)),length(unique(df_disc$fleet))), list(c(unique(as.character(df_disc$EwE_FG))),c(unique(as.character(df_disc$fleet)))))
for (a in (unique(as.character(df_disc$fleet)))){
  for (b in (unique(as.character(df_disc$EwE_FG)))){
    if (length(which(as.character(df_disc$EwE_FG)==b & as.character(df_disc$fleet)==a)) != 0){
      df2_disc[b,a] <- df_disc$discards[which(as.character(df_disc$EwE_FG)==b & as.character(df_disc$fleet)==a)]
    } else {
      df2_disc[b,a] <- 0
    }
  }
}

# Delete ND (not defined species to FG)
df2 <- df2[-which(rownames(df2)=="ND"),]
df2_disc <- df2_disc[-which(rownames(df2_disc)=="ND"),]

# put everything in the right order/file --------------------
Landings_IVc <- read.csv("data/Empty IVc landing-discards file.csv",header = TRUE, row.names = 1)
Discards_IVc <- read.csv("data/Empty IVc landing-discards file.csv",header = TRUE, row.names = 1)

# fill in info
for (fleet in colnames(df2)){
  for (sp in rownames(df2)){
    if (is.na(Landings_IVc[sp,fleet])){
      Landings_IVc[sp,fleet] <- df2[sp,fleet]
    }
  }
}

for (fleet in colnames(df2_disc)){
  for (sp in rownames(df2_disc)){
    if (is.na(Discards_IVc[sp,fleet])){
      Discards_IVc[sp,fleet] <- df2_disc[sp,fleet]
    }
  }
}

### ASSUMPTION: landings are adults ###
# distribute discards to juvenile stanza
for (fleet in colnames(Discards_IVc)){
  if ("Cod (adult)" %in% rownames(discardsratio_ad_juv[[fleet]])){
    Discards_IVc["Juvenile Cod",fleet] <- Discards_IVc["Cod (adult)",fleet] * discardsratio_ad_juv[[fleet]]["Cod (adult)",1]
    Discards_IVc["Cod (adult)",fleet] <- Discards_IVc["Cod (adult)",fleet] * (1-discardsratio_ad_juv[[fleet]]["Cod (adult)",1])
  }
  if ("Whiting (adult)" %in% rownames(discardsratio_ad_juv[[fleet]])){
    Discards_IVc["Juvenile Whiting",fleet] <- Discards_IVc["Whiting (adult)",fleet] * discardsratio_ad_juv[[fleet]]["Whiting (adult)",1]
    Discards_IVc["Whiting (adult)",fleet] <- Discards_IVc["Whiting (adult)",fleet] * (1-discardsratio_ad_juv[[fleet]]["Whiting (adult)",1])
  }
  if ("Plaice (adult)" %in% rownames(discardsratio_ad_juv[[fleet]])){
    Discards_IVc["Juvenile Plaice",fleet] <- Discards_IVc["Plaice (adult)",fleet] * discardsratio_ad_juv[[fleet]]["Plaice (adult)",1]
    Discards_IVc["Plaice (adult)",fleet] <- Discards_IVc["Plaice (adult)",fleet] * (1-discardsratio_ad_juv[[fleet]]["Plaice (adult)",1])
  }
  if ("Sole (adult)" %in% rownames(discardsratio_ad_juv[[fleet]])){
    Discards_IVc["Juvenile Sole",fleet] <- Discards_IVc["Sole (adult)",fleet] * discardsratio_ad_juv[[fleet]]["Sole (adult)",1]
    Discards_IVc["Sole (adult)",fleet] <- Discards_IVc["Sole (adult)",fleet] * (1-discardsratio_ad_juv[[fleet]]["Sole (adult)",1])
  }
}

# assumption: all discards of herring is juvenile
Discards_IVc["Juvenile Herring",] <- Discards_IVc["Herring (adult)",]
Discards_IVc["Herring (adult)",] <- 0

# landings and discards in tonnes km^-1 y^-1
Landings_IVc_tykm <- Landings_IVc/area_IVc_km2
Discards_IVc_tykm <- Discards_IVc/area_IVc_km2

# NAs to 0 for calculation purposes
for (r in c(1:length(rownames(Landings_IVc_tykm)))){
  for (c in c(1:length(colnames(Landings_IVc_tykm)))){
    if (is.na(Landings_IVc_tykm[r,c])){
      Landings_IVc_tykm[r,c] <- 0
    }
  }
}

for (r in c(1:length(rownames(Discards_IVc_tykm)))){
  for (c in c(1:length(colnames(Discards_IVc_tykm)))){
    if (is.na(Discards_IVc_tykm[r,c])){
      Discards_IVc_tykm[r,c] <- 0
    }
  }
}

# write csv for landing per km2 and discards per km2
write.csv(Landings_IVc_tykm,"data/landing_tonnes_year_km2_1991.csv")
write.csv(Discards_IVc_tykm,"data/discard_tonnes_year_km2_1991.csv")

# preparation for filling in Rpath dataframe
colnames(Landings_IVc_tykm) <- colnames(EMBENS_param$model[,c(13:21)])
colnames(Discards_IVc_tykm) <- colnames(EMBENS_param$model[,c(28:36)])
comm_fishery <- cbind(Landings_IVc_tykm,Discards_IVc_tykm)
order_of_FGs <- c(EMBENS_param$model$Group[c(1:31,33:38)])
comm_fishery <- comm_fishery[match(order_of_FGs, rownames(comm_fishery)), ]

